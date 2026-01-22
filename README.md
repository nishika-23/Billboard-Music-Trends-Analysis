# Billboard Music Trends Analysis (IJC437 Project) 🎵

## Profile 👩‍💻
**Nishika Thakkar**  
Data Science student with interests in data analysis, visualisation, and predictive modelling.  
Experienced in R, tidyverse, statistical analysis, and reproducible research using R Markdown and GitHub.

---

## Project Overview 📌
This project analyses long-term trends in popular music using Billboard chart data (2000–2023) combined with MusicOSet/Spotify-style audio and lyrics features. The aim is to explore how musical characteristics have evolved over time and how they relate to chart success.

---

## Research Questions ❓
- **RQ1:** How have key musical characteristics (danceability, energy, valence, and loudness) of Billboard Top 10 songs evolved between 2000 and 2023, and what do these trends indicate about changing listener preferences?  
- **RQ2:** How do the audio feature profiles of Billboard Top 10 songs differ from non–Top 10 songs, and which features show the most consistent differences?  
- **RQ3:** To what extent can Spotify audio features be used to predict whether a song will reach the Billboard Top 10?

---

## Key Findings 🔍
- Musical characteristics of Top 10 songs show clear temporal trends over time, particularly in energy and loudness.  
- Top 10 songs differ systematically from non–Top 10 songs across several audio features, most notably energy, loudness, and danceability.  
- Predictive modelling indicates that a combination of audio features can reasonably distinguish Top 10 hits from non–Top 10 songs.

---

## R Code 💻
The full, reproducible R script used for data cleaning, analysis, visualisation, and modelling is provided in:

- `IDS_final.R`

Running this script automatically:
- generates figures in `/plots/`
- generates tables in `/output_tables/`
- saves `output_model_summary.txt`

---

## How to Run the Code in RStudio 🚀
### Step 1: Download the repository

**Option A — Clone with Git (recommended):**
```bash```
git clone https://github.com/nishika-23/Billboard-Music-Trends-Analysis.git

**Option B — Download ZIP:**
- Click the green Code button on GitHub
- Select Download ZIP
- Extract the folder to your computer

### Step 2: Open the project
- Open Rstudio
- Double-click:
  Billboard-Music-Trends-Analysis.Rproj
  
- This automatically sets the correct working directory (no setwd() required).
  
### Step 3: Install required packages (first run only)
    install.packages(c(
  "tidyverse", "janitor", "stringr", "scales",
  "corrplot", "caret", "RColorBrewer",
  "gridExtra", "grid", "pROC"
))

### Step 4: Run the analysis
  - source("IDS_final.R")
  - 
### Step 5: View outputs
  - After the script finishes, you will find:
  ~ Figures: /plots/
  ~ Tables: /output_tables/
  ~ Model summary: output_model_summary.txt

  - All outputs are generated automatically from the script.

