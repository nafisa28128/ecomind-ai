
# ============================================================
# EcoMind AI - Smart Waste Intelligence
# R Shiny + Google Teachable Machine
# Cloud-compatible Python configuration for Posit Connect Cloud
# ============================================================

# ---------------- 1. R PACKAGES ----------------

required_packages <- c(
  "shiny",
  "DBI",
  "RSQLite",
  "DT",
  "ggplot2",
  "reticulate"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(ggplot2)

# ---------------- 2. DIRECTORIES ----------------

APP_DIR <- getwd()

DATA_DIR <- file.path(APP_DIR, "data")
MODEL_DIR <- file.path(APP_DIR, "models")

DB_PATH <- file.path(DATA_DIR, "ecomind.sqlite")
MODEL_PATH <- file.path(MODEL_DIR, "keras_model.h5")
LABEL_PATH <- file.path(MODEL_DIR, "labels.txt")

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MODEL_DIR, showWarnings = FALSE, recursive = TRUE)

MODEL_CATEGORIES <- c(
  "Cardboard", "Glass", "Metal",
  "Paper", "Plastic", "Trash"
)

# ---------------- 3. DATABASE ----------------

initialize_database <- function() {

  con <- dbConnect(SQLite(), DB_PATH)

  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS waste_records (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT NOT NULL,
      waste_type TEXT NOT NULL,
      condition TEXT NOT NULL,
      confidence REAL,
      destination TEXT NOT NULL,
      recommendation TEXT NOT NULL,
      legal_context TEXT NOT NULL DEFAULT '',
      points INTEGER NOT NULL DEFAULT 0,
      created_at TEXT NOT NULL
    )
  ")

  existing_columns <- dbGetQuery(
    con,
    "PRAGMA table_info(waste_records)"
  )$name

  if (!"legal_context" %in% existing_columns) {
    dbExecute(
      con,
      "ALTER TABLE waste_records
       ADD COLUMN legal_context TEXT NOT NULL DEFAULT ''"
    )
  }

  dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS idx_records_user
     ON waste_records(username)"
  )
}

initialize_database()

get_connection <- function() {
  dbConnect(SQLite(), DB_PATH)
}

# ---------------- 4. AI MODEL INTEGRATION ----------------

model_object <- NULL

model_status <- paste(
  "AI model has not been loaded."
)

load_waste_model <- function() {

  if (!file.exists(MODEL_PATH)) {
    return(list(
      model = NULL,
      message = paste(
        "Model file missing:",
        MODEL_PATH
      )
    ))
  }

  if (!file.exists(LABEL_PATH)) {
    return(list(
      model = NULL,
      message = paste(
        "Label file missing:",
        LABEL_PATH
      )
    ))
  }

  tryCatch({

    # IMPORTANT:
    # Do not call use_virtualenv(), use_python(),
    # or use_conda() inside a deployed application.
    #
    # Posit Connect Cloud selects Python through
    # its deployment environment configuration.

    python_path <- Sys.getenv("RETICULATE_PYTHON")

    if (nzchar(python_path)) {
      message(
        "Using configured Python: ",
        python_path
      )
    }

    tm <- reticulate::import(
      "teachable_machine",
      delay_load = FALSE
    )

    trained_model <- tm$TeachableMachine(
      model_path = MODEL_PATH,
      labels_file_path = LABEL_PATH
    )

    list(
      model = trained_model,
      message = paste(
        "AI model loaded successfully.",
        "Ready for image classification."
      )
    )

  }, error = function(e) {

    list(
      model = NULL,
      message = paste(
        "AI model loading failed:",
        conditionMessage(e)
      )
    )

  })
}

model_load <- load_waste_model()

model_object <- model_load$model
model_status <- model_load$message

# ---------------- 5. MODEL LABELS ----------------

read_model_labels <- function() {

  if (!file.exists(LABEL_PATH)) {
    return(character(0))
  }

  labels <- trimws(
    readLines(
      LABEL_PATH,
      warn = FALSE,
      encoding = "UTF-8"
    )
  )

  labels <- sub(
    "^\\s*[0-9]+\\s+",
    "",
    labels
  )

  labels[nzchar(labels)]
}

# ---------------- 6. IMAGE PREDICTION ----------------

predict_waste_image <- function(image_path) {

  if (is.null(model_object)) {
    stop(model_status)
  }

  if (!file.exists(image_path)) {
    stop("Uploaded image could not be found.")
  }

  prediction <- model_object$classify_image(
    image_path
  )

  scores <- as.numeric(
    unlist(prediction$predictions)
  )

  labels <- read_model_labels()

  if (length(scores) != length(labels)) {
    stop(
      "The number of model outputs does not match labels.txt."
    )
  }

  if (
    length(scores) == 0 ||
    any(!is.finite(scores))
  ) {
    stop(
      "The model returned invalid prediction scores."
    )
  }

  confidence <- as.numeric(
    prediction$class_confidence
  )

  if (
    length(confidence) != 1 ||
    !is.finite(confidence)
  ) {
    stop(
      "The model returned an invalid confidence score."
    )
  }

  list(
    label = as.character(
      prediction$class_name
    ),
    confidence = confidence,
    scores = scores,
    labels = labels
  )
}

# ---------------- 7. CATEGORY NORMALIZATION ----------------

normalize_category <- function(label) {

  x <- tolower(trimws(label))

  if (grepl("cardboard|corrugated", x)) {
    return("Cardboard")
  }

  if (grepl("glass", x)) {
    return("Glass")
  }

  if (grepl(
    "metal|aluminium|aluminum|steel|tin",
    x
  )) {
    return("Metal")
  }

  if (grepl("paper", x)) {
    return("Paper")
  }

  if (grepl(
    "plastic|polymer|pet|hdpe",
    x
  )) {
    return("Plastic")
  }

  if (grepl(
    "^trash$|misc|mixed|general",
    x
  )) {
    return("Trash")
  }

  if (grepl(
    "organic|food|compost|biodegradable",
    x
  )) {
    return("Organic")
  }

  if (grepl(
    "electronic|e.?waste|battery|phone|computer",
    x
  )) {
    return("Electronic")
  }

  "Other"
}

# ---------------- 8. WASTE GUIDANCE ENGINE ----------------

GUIDANCE <- list(

  Cardboard = list(
    material = paste(
      "Paper-based fibre packaging, often corrugated."
    ),
    destination = paste(
      "Keep separate for paper and cardboard recovery",
      "or a local recycling collector."
    ),
    action = c(
      "Keep it clean and dry.",
      "Flatten boxes to reduce storage and transport volume.",
      "Remove plastic wrapping, foam and excessive tape where practical.",
      "Do not mix heavily food-soiled or wet cardboard with clean recyclable paper."
    ),
    caution = paste(
      "Wet, oily or food-contaminated cardboard may not",
      "be accepted by all recyclers."
    ),
    points = 8L,
    legal = paste(
      "Bangladesh Solid Waste Management Rules, 2021",
      "are relevant to municipal solid-waste management.",
      "Confirm applicable local collection requirements",
      "with your city corporation or municipality."
    )
  ),

  Glass = list(
    material = paste(
      "Glass packaging or glass objects.",
      "The model does not identify glass type."
    ),
    destination = paste(
      "Separate glass for a local glass collector",
      "or recovery service where accepted."
    ),
    action = c(
      "Keep glass separate from other recyclables.",
      "Do not place broken glass loose in a bag. Use a rigid, clearly marked container.",
      "Check whether the collector accepts the specific glass item."
    ),
    caution = paste(
      "Mirrors, ceramics, heat-resistant glass and window glass",
      "may not be accepted with bottle glass."
    ),
    points = 12L,
    legal = paste(
      "Follow applicable municipal solid-waste collection",
      "and worker-safety instructions."
    )
  ),

  Metal = list(
    material = paste(
      "Potentially recoverable metal packaging or scrap.",
      "The model does not identify the alloy."
    ),
    destination = paste(
      "Keep separate for a scrap-metal dealer",
      "or authorized recovery channel."
    ),
    action = c(
      "Separate metal from food residue where safe and practical.",
      "Keep sharp edges contained to prevent injury.",
      "Do not handle unidentified chemical containers or pressurized cylinders as ordinary scrap."
    ),
    caution = paste(
      "A visual prediction cannot determine alloy, chemical",
      "contamination or whether an item is safe to handle."
    ),
    points = 15L,
    legal = paste(
      "Use applicable municipal waste-management guidance.",
      "Special or contaminated metal waste may require",
      "a different handling route."
    )
  ),

  Paper = list(
    material = paste(
      "Paper products such as office paper, newspapers",
      "and printed sheets."
    ),
    destination = paste(
      "Keep clean paper separate for reuse or paper recycling."
    ),
    action = c(
      "Keep paper dry and free from food or oil.",
      "Reuse one-sided sheets where possible.",
      "Separate laminated, waxed or heavily coated paper if the collector requests it."
    ),
    caution = paste(
      "Not all paper products are recyclable",
      "in every local collection system."
    ),
    points = 8L,
    legal = paste(
      "Bangladesh Solid Waste Management Rules, 2021",
      "provide the national solid-waste management framework."
    )
  ),

  Plastic = list(
    material = paste(
      "Plastic item. Image classification does not reliably",
      "identify polymer resin or recycling code."
    ),
    destination = paste(
      "Separate clean, accepted plastic items for a local",
      "plastic collector or recycling service."
    ),
    action = c(
      "Empty the item and rinse it only when safe and practical.",
      "Check the resin code and ask the collector which types are accepted.",
      "Keep multilayer sachets and non-accepted plastics separate.",
      "Do not burn plastic or dump it into drains, canals or waterways."
    ),
    caution = paste(
      "The model cannot identify resin type, additives",
      "or food-contact safety from the image alone."
    ),
    points = 10L,
    legal = paste(
      "Bangladesh has a National Action Plan for Sustainable",
      "Plastic Management and the Solid Waste Management Rules, 2021."
    )
  ),

  Trash = list(
    material = paste(
      "Mixed or miscellaneous waste.",
      "This category may contain recyclable, organic or hazardous items."
    ),
    destination = paste(
      "Do not assume this item is non-recyclable.",
      "Inspect and separate its components where safe."
    ),
    action = c(
      "Check whether the item contains paper, plastic, metal, glass or organic material.",
      "Keep batteries, electronics, chemicals, medical waste and sharp objects out of ordinary mixed waste.",
      "Seek advice from the local waste authority if the material is unknown."
    ),
    caution = paste(
      "The TrashNet Trash class is a miscellaneous training",
      "category, not a definitive waste-treatment category."
    ),
    points = 2L,
    legal = paste(
      "Follow the relevant city corporation or municipality's",
      "waste collection instructions."
    )
  ),

  Organic = list(
    material = paste(
      "Potential biodegradable or food waste.",
      "This category is not part of the current six-class model."
    ),
    destination = paste(
      "Use a municipal organic-waste or composting service",
      "where one exists."
    ),
    action = c(
      "Keep organic material separate from plastic and other contaminants.",
      "Use a covered container to reduce pests and odour.",
      "Compost only suitable materials."
    ),
    caution = paste(
      "Organic waste is currently available through manual selection only."
    ),
    points = 10L,
    legal = paste(
      "Check the local authority's organic-waste collection",
      "and composting arrangements."
    )
  ),

  Electronic = list(
    material = paste(
      "Potential electronic waste.",
      "This category is not part of the current six-class model."
    ),
    destination = paste(
      "Use an authorized e-waste collection or take-back channel."
    ),
    action = c(
      "Do not put electronics or batteries in ordinary household bins.",
      "Do not dismantle, puncture, crush or burn batteries.",
      "Keep damaged or swollen batteries away from heat.",
      "Use a documented collection or take-back service."
    ),
    caution = paste(
      "The current model cannot detect or safely classify e-waste.",
      "Select this category manually."
    ),
    points = 20L,
    legal = paste(
      "Bangladesh Hazardous Waste (E-Waste) Management Rules, 2021",
      "are the relevant national regulatory reference."
    )
  ),

  Other = list(
    material = "Unclassified item.",
    destination = paste(
      "Check the item and local municipal guidance before disposal."
    ),
    action = c(
      "Do not assume the item is recyclable or safe based only on appearance.",
      "Separate identifiable recyclable components.",
      "Seek specialist advice for chemicals, batteries, medical waste or unknown hazardous materials."
    ),
    caution = paste(
      "AI classification is uncertain.",
      "Manual inspection is recommended."
    ),
    points = 2L,
    legal = paste(
      "Consult the relevant city corporation or municipality",
      "and applicable waste-specific rules."
    )
  )
)

get_guidance <- function(category, condition) {

  g <- GUIDANCE[[category]]

  if (is.null(g)) {
    g <- GUIDANCE$Other
  }

  if (identical(condition, "Contaminated")) {

    g$points <- max(
      1L,
      as.integer(floor(g$points / 2))
    )

    g$action <- c(
      g$action,
      paste(
        "The item is marked contaminated.",
        "Do not clean unknown chemical, medical or hazardous waste yourself."
      )
    )
  }

  g
}

# ---------------- 9. USER INTERFACE ----------------

ui <- fluidPage(

  tags$head(

    tags$title(
      "EcoMind AI | Smart Waste Intelligence"
    ),

    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700;800&family=Manrope:wght@600;700;800&display=swap');

      :root {
        --forest:#145c45;
        --green:#1f8a62;
        --mint:#e8f4ee;
        --ink:#172b25;
        --muted:#6b7d75;
        --line:#e3ece7;
      }

      html, body {
        background:#f4f7f5;
        color:var(--ink);
        font-family:'DM Sans','Segoe UI',sans-serif;
      }

      .container-fluid {
        max-width:1480px;
        padding:24px 30px 36px;
      }

      .app-header {
        background:linear-gradient(120deg,#103f35 0%,#17674d 58%,#248866 100%);
        color:white;
        padding:26px 30px;
        border-radius:20px;
        margin:4px 0 24px;
        box-shadow:0 12px 30px rgba(20,92,69,.13);
      }

      .app-brand {
        font-family:'Manrope',sans-serif;
        font-size:27px;
        font-weight:800;
        letter-spacing:-.7px;
        margin:0;
      }

      .app-subtitle {
        color:#d5eee2;
        margin-top:5px;
        font-size:13px;
        letter-spacing:.2px;
      }

      .sidebar, .main-panel {
        background:#fff;
        border:1px solid var(--line);
        border-radius:18px;
        padding:22px !important;
        box-shadow:0 5px 18px rgba(25,60,44,.045);
      }

      .metric {
        background:linear-gradient(145deg,#fff 0%,#f5faf7 100%);
        padding:20px;
        border-radius:16px;
        border:1px solid var(--line);
        margin-bottom:14px;
        min-height:112px;
      }

      .metric h4 {
        color:var(--muted);
        font-size:12px;
        font-weight:700;
        text-transform:uppercase;
        letter-spacing:.8px;
        margin:0 0 12px;
      }

      .metric h3 {
        color:var(--forest);
        font-family:'Manrope',sans-serif;
        font-size:30px;
        font-weight:800;
        margin:0;
      }

      .nav-tabs {
        border-bottom:1px solid var(--line);
        margin-bottom:22px;
      }

      .nav-tabs>li>a {
        color:var(--muted);
        font-weight:700;
        border:0 !important;
        border-radius:10px 10px 0 0;
      }

      .nav-tabs>li.active>a,
      .nav-tabs>li.active>a:hover {
        color:var(--forest);
        background:var(--mint);
        border:0 !important;
      }

      .form-control {
        border:1px solid #dce7e1;
        border-radius:9px;
        box-shadow:none;
        min-height:40px;
      }

      label {
        font-weight:700;
        color:#344b41;
      }

      .btn {
        border-radius:9px;
        font-weight:700;
        padding:9px 15px;
      }

      .btn-success, .btn-primary {
        background:#176b4e;
        border-color:#176b4e;
      }

      .small-note {
        color:var(--muted);
        font-size:12px;
        line-height:1.6;
      }

      .analyzer-heading {
        font-family:'Manrope',sans-serif;
        color:#173d30;
        font-size:23px;
        font-weight:800;
        margin:0 0 7px;
      }

      .analyzer-description {
        color:#718078;
        font-size:13px;
        line-height:1.6;
        margin-bottom:20px;
      }

      .image-panel {
        background:#ffffff;
        border:1px solid #e1eae5;
        border-radius:15px;
        padding:16px;
        box-shadow:0 4px 14px rgba(25,60,44,.035);
        margin-bottom:16px;
      }

      .image-panel-title {
        font-family:'Manrope',sans-serif;
        font-size:15px;
        font-weight:800;
        color:#214d3b;
        margin:0 0 12px;
      }

      .image-preview-area {
        width:100%;
        height:260px;
        background:#f3f6f4;
        border:1px dashed #d4e2da;
        border-radius:11px;
        display:flex;
        align-items:center;
        justify-content:center;
        overflow:hidden;
      }

      .image-preview-area .shiny-image-output {
        width:100%;
        height:100%;
        display:flex;
        align-items:center;
        justify-content:center;
      }

      .image-preview-area img {
        display:block;
        max-width:100%;
        max-height:250px;
        width:auto;
        height:auto;
        object-fit:contain;
        margin:auto;
        border-radius:7px;
      }

      .image-placeholder {
        text-align:center;
        color:#899991;
        padding:20px;
        font-size:13px;
        line-height:1.7;
      }

      .analysis-panel {
        background:#ffffff;
        border:1px solid #e1eae5;
        border-radius:15px;
        padding:20px;
        min-height:320px;
        box-shadow:0 4px 14px rgba(25,60,44,.035);
        margin-bottom:16px;
      }

      .result-card {
        background:#ffffff;
        border:1px solid #e1eae5;
        border-radius:12px;
        padding:17px;
        margin:0 0 13px;
      }

      .result-title {
        color:#145c45;
        font-family:'Manrope',sans-serif;
        font-size:25px;
        font-weight:800;
        margin:5px 0 10px;
      }

      .confidence-track {
        background:#e5eee8;
        border-radius:20px;
        height:10px;
        overflow:hidden;
        margin:9px 0 7px;
      }

      .confidence-fill {
        background:#238363;
        height:10px;
        border-radius:20px;
      }

      .warning-card {
        background:#fff8e8;
        border:1px solid #f0dfb5;
        border-radius:12px;
        padding:14px;
        margin:12px 0;
        color:#5e4c27;
      }

      .section-label {
        color:#557166;
        font-size:11px;
        font-weight:800;
        letter-spacing:1px;
        text-transform:uppercase;
        margin:10px 0 8px;
      }

      .result-card p,
      .analysis-panel p,
      .result-card li {
        color:#40554a;
        font-size:13px;
        line-height:1.75;
      }

      .result-card ul {
        padding-left:20px;
        margin-bottom:8px;
      }

      .source-link {
        overflow-wrap:anywhere;
      }

      .analyzer-column {
        padding-left:8px;
        padding-right:8px;
      }

      @media(max-width:767px) {
        .container-fluid {
          padding:12px;
        }

        .app-header {
          padding:22px;
        }

        .main-panel {
          padding:16px !important;
        }

        .image-preview-area {
          height:220px;
        }

        .image-preview-area img {
          max-height:210px;
        }

        .analysis-panel {
          padding:15px;
        }

        .result-title {
          font-size:22px;
        }
      }
    "))
  ),

  div(
    class = "app-header",
    div(
      class = "app-brand",
      "♻ EcoMind AI"
    ),
    div(
      class = "app-subtitle",
      paste(
        "SMART WASTE INTELLIGENCE",
        "• IDENTIFY / RECOVER / REWARD"
      )
    )
  ),

  sidebarLayout(

    sidebarPanel(

      textInput(
        "username",
        "Your name / nickname",
        value = "Guest"
      ),

      helpText(
        paste(
          "Use a nickname.",
          "This prototype does not provide secure user accounts."
        )
      ),

      fileInput(
        "waste_image",
        "Upload a waste photo",
        accept = c(
          "image/png",
          "image/jpeg",
          "image/jpg",
          "image/webp"
        )
      ),

      selectInput(
        "manual_type",
        "Identification method / category",
        choices = c(
          "AI: detect from photo" = "AUTO",
          "Cardboard" = "Cardboard",
          "Glass" = "Glass",
          "Metal" = "Metal",
          "Paper" = "Paper",
          "Plastic" = "Plastic",
          "Mixed / miscellaneous trash" = "Trash",
          "Organic (manual only)" = "Organic",
          "Electronic waste (manual only)" = "Electronic",
          "Other / uncertain" = "Other"
        ),
        selected = "AUTO"
      ),

      selectInput(
        "condition",
        "Item condition",
        choices = c(
          "Clean",
          "Contaminated"
        )
      ),

      actionButton(
        "analyze",
        "Analyze item",
        class = "btn-success",
        icon = icon("search")
      ),

      tags$hr(),

      checkboxInput(
        "confirm_action",
        paste(
          "I have followed the suggested",
          "sorting / disposal action"
        ),
        FALSE
      ),

      actionButton(
        "record_item",
        "Save activity and claim points",
        class = "btn-primary"
      ),

      tags$hr(),

      strong("Model status"),

      p(
        class = "small-note",
        textOutput("model_status")
      ),

      width = 3,
      class = "sidebar"
    ),

    mainPanel(

      tabsetPanel(
        id = "main_tabs",

        # ---------------- DASHBOARD ----------------

        tabPanel(
          "Dashboard",
          br(),

          fluidRow(

            column(
              4,
              div(
                class = "metric",
                h4("Items recorded"),
                h3(
                  textOutput(
                    "item_count",
                    inline = TRUE
                  )
                )
              )
            ),

            column(
              4,
              div(
                class = "metric",
                h4("Reward points"),
                h3(
                  textOutput(
                    "points_total",
                    inline = TRUE
                  )
                )
              )
            ),

            column(
              4,
              div(
                class = "metric",
                h4("Contributors"),
                h3(
                  textOutput(
                    "user_count",
                    inline = TRUE
                  )
                )
              )
            )
          ),

          plotOutput(
            "waste_plot",
            height = "320px"
          )
        ),

        # ---------------- WASTE ANALYZER ----------------

        tabPanel(
          "Waste Analyzer",
          br(),

          h3(
            class = "analyzer-heading",
            "Waste Analysis"
          ),

          p(
            class = "analyzer-description",
            paste(
              "Upload a photograph and review the AI classification,",
              "material information and recommended recovery pathway."
            )
          ),

          fluidRow(

            column(
              4,
              class = "analyzer-column",

              div(
                class = "image-panel",

                div(
                  class = "image-panel-title",
                  "Uploaded image"
                ),

                div(
                  class = "image-preview-area",
                  uiOutput("image_preview_area")
                ),

                p(
                  class = "small-note",
                  paste(
                    "Your uploaded photograph is displayed",
                    "separately from the analysis."
                  )
                )
              )
            ),

            column(
              8,
              class = "analyzer-column",

              div(
                class = "analysis-panel",
                uiOutput("analysis_result")
              )
            )
          )
        ),

        # ---------------- ACTIVITY & REWARDS ----------------

        tabPanel(
          "Activity & Rewards",
          br(),
          DTOutput("records_table")
        ),

        # ---------------- ABOUT THE MODEL ----------------

        tabPanel(
          "About the model",
          br(),

          h4("Current model scope"),

          p(
            paste(
              "The current image model was trained using six",
              "TrashNet-style classes: Cardboard, Glass, Metal,",
              "Paper, Plastic and Trash."
            )
          ),

          p(
            paste(
              "Organic and Electronic are manual-only categories",
              "in this version. They are not recognised by the",
              "current trained model."
            )
          ),

          p(
            paste(
              "A model confidence score is not the same as accuracy.",
              "Test the model using new images before relying on its predictions."
            )
          ),

          h4("Important limitations"),

          tags$ul(
            tags$li(
              paste(
                "A photo cannot reliably identify polymer resin,",
                "chemical contamination, glass composition or battery condition."
              )
            ),
            tags$li(
              paste(
                "The Trash class is broad and should not be treated",
                "as a specific disposal instruction."
              )
            ),
            tags$li(
              paste(
                "Legal guidance is informational. Confirm exact",
                "requirements with the official legal text and relevant local authority."
              )
            )
          )
        )
      ),

      class = "main-panel"
    )
  )
)

# ---------------- 10. SERVER ----------------

server <- function(input, output, session) {

  analysis <- reactiveVal(NULL)

  safe_username <- reactive({

    nm <- trimws(input$username)

    if (
      is.null(nm) ||
      !nzchar(nm)
    ) {
      "Guest"
    } else {
      substr(nm, 1, 40)
    }
  })

  # ---------------- DATABASE RETRIEVAL ----------------

  fetch_records <- function() {

    con <- get_connection()

    on.exit(
      dbDisconnect(con),
      add = TRUE
    )

    dbGetQuery(con, "
      SELECT
        id,
        username,
        waste_type,
        condition,
        confidence,
        destination,
        recommendation,
        legal_context,
        points,
        created_at
      FROM waste_records
      ORDER BY id DESC
    ")
  }

  records <- reactive({
    input$record_item
    fetch_records()
  })

  # ---------------- MODEL STATUS ----------------

  output$model_status <- renderText({
    model_status
  })

  # ---------------- IMAGE PREVIEW ----------------

  output$image_preview_area <- renderUI({

    if (is.null(input$waste_image)) {

      return(
        div(
          class = "image-placeholder",

          icon(
            "image",
            class = "fa-3x"
          ),

          br(),

          strong("No image uploaded"),

          br(),

          "Upload a waste photograph using the sidebar."
        )
      )
    }

    imageOutput(
      "image_preview",
      height = "260px",
      width = "100%"
    )
  })

  output$image_preview <- renderImage({

    req(input$waste_image)

    list(
      src = input$waste_image$datapath,
      contentType = input$waste_image$type,
      alt = "Uploaded waste photo"
    )

  }, deleteFile = FALSE)

  # ---------------- ANALYZE WASTE ----------------

  observeEvent(input$analyze, {

    req(input$manual_type)

    tryCatch({

      category <- NULL
      confidence <- NA_real_
      prediction_label <- NULL
      low_confidence <- FALSE
      score_table <- NULL

      if (
        identical(
          input$manual_type,
          "AUTO"
        )
      ) {

        req(input$waste_image)

        if (is.null(model_object)) {
          stop(model_status)
        }

        prediction <- predict_waste_image(
          input$waste_image$datapath
        )

        prediction_label <- prediction$label

        category <- normalize_category(
          prediction$label
        )

        confidence <- as.numeric(
          prediction$confidence
        )

        low_confidence <- (
          !is.finite(confidence) ||
          confidence < 0.60
        )

        score_table <- data.frame(
          Class = prediction$labels,
          Score = round(
            as.numeric(prediction$scores) * 100,
            1
          ),
          check.names = FALSE
        )

        score_table <- score_table[
          order(
            score_table$Score,
            decreasing = TRUE
          ),
          ,
          drop = FALSE
        ]

      } else {

        category <- input$manual_type
      }

      guidance_category <- if (low_confidence) {
        "Other"
      } else {
        category
      }

      g <- get_guidance(
        guidance_category,
        input$condition
      )

      analysis(list(
        category = category,
        guidance_category = guidance_category,
        confidence = confidence,
        prediction_label = prediction_label,
        low_confidence = low_confidence,
        score_table = score_table,
        condition = input$condition,
        material = g$material,
        destination = g$destination,
        action = g$action,
        caution = g$caution,
        legal = g$legal,
        points = g$points,
        is_manual = !identical(
          input$manual_type,
          "AUTO"
        )
      ))

      updateTabsetPanel(
        session,
        "main_tabs",
        selected = "Waste Analyzer"
      )

      showNotification(
        paste(
          "Analysis completed.",
          "Review the limitations before acting."
        ),
        type = "message"
      )

    }, error = function(e) {

      analysis(NULL)

      showNotification(
        paste(
          "Analysis could not be completed:",
          conditionMessage(e)
        ),
        type = "error",
        duration = 12
      )
    })
  })

  # ---------------- DISPLAY RESULTS ----------------

  output$analysis_result <- renderUI({

    x <- analysis()

    if (is.null(x)) {

      return(
        tagList(

          h4(
            "Your analysis will appear here",
            style = "color:#214d3b; font-weight:800;"
          ),

          p(
            paste(
              "Upload a photograph and click Analyze item.",
              "The AI classification and recommendations",
              "will be displayed in this panel."
            )
          ),

          div(
            class = "warning-card",

            strong("Supported AI categories"),

            p(
              paste(
                "Cardboard, Glass, Metal, Paper, Plastic and Trash."
              )
            ),

            p(
              paste(
                "Organic and Electronic are available",
                "through manual selection only."
              )
            )
          )
        )
      )
    }

    # ---------------- CONFIDENCE DISPLAY ----------------

    conf_ui <- if (is.finite(x$confidence)) {

      tagList(

        p(
          strong(
            paste0(
              "Model confidence: ",
              round(x$confidence * 100, 1),
              "%"
            )
          )
        ),

        div(
          class = "confidence-track",

          div(
            class = "confidence-fill",
            style = paste0(
              "width:",
              max(
                0,
                min(100, x$confidence * 100)
              ),
              "%;"
            )
          )
        ),

        p(
          class = "small-note",
          paste(
            "Confidence is the model's score for this prediction,",
            "not independently measured accuracy."
          )
        )
      )

    } else {

      p(
        class = "small-note",
        paste(
          "Category selected manually.",
          "No AI confidence score is available."
        )
      )
    }

    # ---------------- LOW CONFIDENCE WARNING ----------------

    warning_ui <- if (
      isTRUE(x$low_confidence)
    ) {

      div(
        class = "warning-card",

        strong("Low-confidence prediction"),

        p(
          paste(
            "The model is not sufficiently certain",
            "(below the provisional 60% threshold).",
            "The material-specific guidance is withheld.",
            "Retake the photo or select the category manually."
          )
        )
      )

    } else {
      NULL
    }

    # ---------------- OTHER MODEL SCORES ----------------

    score_ui <- if (!is.null(x$score_table)) {

      tagList(

        div(
          class = "section-label",
          "Other model scores"
        ),

        DTOutput("prediction_scores")
      )

    } else {
      NULL
    }

    # ---------------- COMPLETE RESULT ----------------

    tagList(

      div(
        class = "result-card",

        div(
          class = "section-label",
          "Identification result"
        ),

        div(
          class = "result-title",

          if (x$low_confidence) {
            "Needs manual review"
          } else {
            x$category
          }
        ),

        p(
          if (!is.null(x$prediction_label)) {
            paste(
              "Original model label:",
              x$prediction_label
            )
          } else {
            "Manual category selection"
          }
        ),

        conf_ui
      ),

      warning_ui,

      score_ui,

      # MATERIAL AND DISPOSAL GUIDANCE

      div(
        class = "result-card",

        div(
          class = "section-label",
          "Material information"
        ),

        p(x$material),

        div(
          class = "section-label",
          "Suggested recovery pathway"
        ),

        p(
          strong(x$destination)
        ),

        div(
          class = "section-label",
          "Recommended actions"
        ),

        tags$ul(
          lapply(
            x$action,
            tags$li
          )
        ),

        div(
          class = "warning-card",

          strong("Handling limitation"),

          p(x$caution)
        )
      ),

      # BANGLADESH REGULATORY CONTEXT

      div(
        class = "result-card",

        div(
          class = "section-label",
          "Bangladesh regulatory context"
        ),

        p(x$legal),

        p(
          class = "small-note",
          paste(
            "This is general information, not a legal determination.",
            "Check the official gazette and your local authority",
            "for current requirements."
          )
        ),

        tags$ul(

          tags$li(
            a(
              paste(
                "Bangladesh Solid Waste Management Rules, 2021:",
                "verify official gazette text"
              ),
              href = "https://moef.gov.bd/",
              target = "_blank",
              class = "source-link"
            )
          ),

          tags$li(
            a(
              "Department of Environment, Bangladesh",
              href = "https://doe.gov.bd/",
              target = "_blank",
              class = "source-link"
            )
          ),

          tags$li(
            a(
              paste(
                "Bangladesh National Action Plan",
                "for Sustainable Plastic Management"
              ),
              href = "https://doe.gov.bd/",
              target = "_blank",
              class = "source-link"
            )
          )
        )
      ),

      # REWARDS

      div(
        class = "result-card",

        div(
          class = "section-label",
          "Prototype reward"
        ),

        h4(
          paste(
            "Potential reward:",
            x$points,
            "points"
          ),
          style = "color:#145c45; font-weight:800;"
        ),

        p(
          class = "small-note",
          paste(
            "Points are prototype incentives, not money",
            "and not proof of recycling or environmental impact."
          )
        )
      )
    )
  })

  # ---------------- MODEL SCORE TABLE ----------------

  output$prediction_scores <- renderDT({

    x <- analysis()

    req(
      x,
      x$score_table
    )

    datatable(
      x$score_table,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE
      )
    )
  })

  # ---------------- SAVE ACTIVITY ----------------

  observeEvent(input$record_item, {

    x <- analysis()

    if (is.null(x)) {

      showNotification(
        "Analyze an item before saving it.",
        type = "warning"
      )

      return()
    }

    if (isTRUE(x$low_confidence)) {

      showNotification(
        paste(
          "Please manually verify the category",
          "before saving a low-confidence prediction."
        ),
        type = "warning"
      )

      return()
    }

    if (!isTRUE(input$confirm_action)) {

      showNotification(
        paste(
          "Confirm that you followed the suggested action",
          "before claiming points."
        ),
        type = "warning"
      )

      return()
    }

    con <- get_connection()

    tryCatch({

      dbExecute(
        con,

        paste(
          "INSERT INTO waste_records",
          "(username, waste_type, condition, confidence,",
          "destination, recommendation, legal_context,",
          "points, created_at)",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ),

        params = list(

          safe_username(),

          x$category,

          x$condition,

          if (is.finite(x$confidence)) {
            x$confidence
          } else {
            NA_real_
          },

          x$destination,

          paste(
            x$action,
            collapse = " "
          ),

          x$legal,

          as.integer(x$points),

          format(
            Sys.time(),
            "%Y-%m-%d %H:%M:%S"
          )
        )
      )

      showNotification(
        "Activity saved successfully.",
        type = "message"
      )

      analysis(NULL)

      updateCheckboxInput(
        session,
        "confirm_action",
        value = FALSE
      )

    }, error = function(e) {

      showNotification(
        paste(
          "Could not save record:",
          conditionMessage(e)
        ),
        type = "error"
      )

    }, finally = {

      dbDisconnect(con)
    })
  })

  # ---------------- DASHBOARD METRICS ----------------

  output$item_count <- renderText({
    nrow(records())
  })

  output$points_total <- renderText({
    sum(
      records()$points,
      na.rm = TRUE
    )
  })

  output$user_count <- renderText({

    d <- records()

    if (nrow(d) == 0) {
      0
    } else {
      length(
        unique(d$username)
      )
    }
  })

  # ---------------- WASTE ACTIVITY CHART ----------------

  output$waste_plot <- renderPlot({

    d <- records()

    validate(
      need(
        nrow(d) > 0,
        paste(
          "No activity yet.",
          "Analyze and save your first item."
        )
      )
    )

    counts <- as.data.frame(
      table(d$waste_type)
    )

    names(counts) <- c(
      "Waste type",
      "Items"
    )

    ggplot(
      counts,
      aes(
        x = reorder(
          `Waste type`,
          -Items
        ),
        y = Items
      )
    ) +

      geom_col(
        fill = "#238363",
        width = 0.68
      ) +

      labs(
        x = NULL,
        y = "Recorded items",
        title = "Waste activity by category"
      ) +

      theme_minimal(
        base_size = 13
      ) +

      theme(
        plot.title = element_text(
          face = "bold",
          colour = "#203d31",
          size = 15
        ),

        axis.text = element_text(
          colour = "#53675d"
        ),

        panel.grid.major.x = element_blank(),

        panel.grid.minor = element_blank()
      )
  })

  # ---------------- ACTIVITY HISTORY ----------------

  output$records_table <- renderDT({

    d <- records()

    if (nrow(d) == 0) {

      return(
        datatable(
          data.frame(
            Message = "No records yet."
          ),
          options = list(
            dom = "t"
          )
        )
      )
    }

    d$confidence <- ifelse(
      is.na(d$confidence),
      "Manual",
      paste0(
        round(
          d$confidence * 100,
          1
        ),
        "%"
      )
    )

    datatable(

      d[
        ,
        c(
          "id",
          "username",
          "waste_type",
          "condition",
          "confidence",
          "destination",
          "points",
          "created_at"
        )
      ],

      rownames = FALSE,

      options = list(
        pageLength = 8,
        scrollX = TRUE
      )
    )
  })
}

# ---------------- 11. RUN APPLICATION ----------------

shinyApp(
  ui = ui,
  server = server
)

  ui = ui,
  server = server
)
