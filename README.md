# EcoMind AI

An R/Shiny prototype for smart waste management.

## Included features

- Dashboard with waste activity statistics
- Optional image recognition using an exported Keras image classifier
- Manual waste category selection when no model is installed
- Waste-specific sorting/recovery recommendations
- SQLite persistence (records remain after the app restarts)
- Prototype reward points and activity history

**Important:** This is a prototype. Predictions may be wrong. It does not verify that an item was actually recycled, and reward points are not cash. Follow local municipal recycling rules. Do not use image predictions for hazardous-waste handling.

## 1. Install R and RStudio

Install R and RStudio Desktop. Open this project folder in RStudio.

## 2. Install packages

In the RStudio Console, run:

```r
install.packages(c("shiny", "DBI", "RSQLite", "DT", "ggplot2", "magick"))
```

Then open `app.R` and click **Run App**.

If the app crashes:
1. Run `sessionInfo()` in the Console.
2. Run `shiny::runApp()` from the project directory.
3. Read the first red error message in the Console, not just the browser error.
4. Try restarting R: Session -> Restart R.
5. Update packages with `update.packages(ask = FALSE, checkBuilt = TRUE)`.

The app can run without the image model. Choose a waste category manually.

## 3. Train the image classifier

A beginner-friendly route is Google Teachable Machine:

1. Open https://teachablemachine.withgoogle.com/
2. Choose **Image Project** -> **Standard image model**.
3. Create classes such as:
   - Plastic
   - Paper
   - Metal
   - Glass
   - Organic
   - Electronic
4. Collect varied images for each class. Aim for at least 100-300 images per class to start; more varied images are better.
5. Include different backgrounds, lighting, angles, distances, clean/dirty examples, and different objects.
6. Avoid near-duplicate photos appearing in both training and test sets.
7. Train the model and test it using photos it has never seen.
8. Use **Export Model** -> **Tensorflow** -> **Keras** (wording may vary).
9. Download the model and copy `keras_model.h5` and `labels.txt` into this project's `models/` folder.

The expected files are:

```text
models/
  keras_model.h5
  labels.txt
```

The app expects a 224 x 224 RGB model with input pixels scaled to 0-1, as used by the standard Teachable Machine image export. If you export a model with a different input size or preprocessing, adjust `predict_waste_image()` in `app.R`.

### Enable model inference in R

Install `keras3`:

```r
install.packages("keras3")
```

Then install/configure a supported backend as described in the official Keras for R documentation:
https://keras3.posit.co/

Restart R and run the app again. The app will try to load `models/keras_model.h5` automatically. If the model fails to load, the app still supports manual category selection.

### Evaluate before relying on predictions

Use a separate test set that was not used during training. Record:
- Accuracy for each waste class
- Confusion matrix
- False positives (e.g. plastic mistaken for glass)
- Performance on dirty, crushed, wet, and mixed-material items

Do not use a single overall accuracy score alone. If confidence is low, ask the user to choose the category manually. A photo cannot reliably identify all resin types, contamination, or local recyclability.

## 4. Data storage

The app creates `data/ecomind.sqlite` automatically. This database stores names/nicknames, categories, conditions, predictions, recommendations, points, and timestamps.

Do not publish real personal data. For a public deployment, add proper authentication, authorization, rate limiting, input validation, backups, and a privacy notice. The nickname field is not a secure account system.

## 5. Publish the project to GitHub

This section uploads your **source code** to GitHub so you can keep a backup, track changes, and share the project. It does not make the Shiny app run as a public website. For a live interactive website, use a Shiny hosting service such as shinyapps.io (separate setup; see the note below).

1. Sign in at https://github.com/ and create a **new, empty repository** named `ecomind-ai`. Do not add a README, license, or `.gitignore` during repository creation because those files are already in this project.
2. In RStudio, open the **Terminal** tab (not the R Console) and make sure the terminal is in this project's folder.
3. Run these commands, replacing `YOUR_USERNAME` with your GitHub username:

```bash
git init
git add app.R README.md .gitignore models/
git commit -m "Add EcoMind AI application"
git branch -M main
git remote add origin https://github.com/YOUR_USERNAME/ecomind-ai.git
git push -u origin main
```

If Git says `remote origin already exists`, run this instead of the `git remote add` command:

```bash
git remote set-url origin https://github.com/YOUR_USERNAME/ecomind-ai.git
git push -u origin main
```

For future updates:

```bash
git add app.R README.md
git commit -m "Update EcoMind AI"
git push
```

GitHub may ask you to authenticate in a browser or credential manager. Never paste a password or personal access token into `app.R`, README files, or a public repository. The `.gitignore` excludes the local SQLite database and large model-weight files so personal activity records and bulky model files are not accidentally uploaded.

### Want a live public app?

A GitHub repository alone does not host an R/Shiny application. To publish an interactive version, create a shinyapps.io account, install and configure the `rsconnect` R package, then deploy the project using the account token provided by shinyapps.io. Do not commit that token to GitHub. Also add proper authentication and privacy safeguards before making a public app that stores user activity.

## 6. Run locally

From the RStudio Console, with this folder as the working directory:

```r
shiny::runApp()
```

## Project structure

```text
ecomind-ai/
  app.R
  README.md
  .gitignore
  models/
    (put keras_model.h5 and labels.txt here)
  data/
    (SQLite database is created here)
```
