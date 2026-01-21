👋 Hi, I’m Nishika Thakkar

🎓 I’m a Data Science student at the University of Sheffield.  
🎵 My main interest is exploring trends in popular music using data-driven approaches.  
💻 I enjoy working with R, data visualization, and exploratory analysis.

🧠 Professional Skills
- Data Cleaning & Visualization (R, ggplot2, tidyverse)
- Predictive Modeling (caret, pROC)
- Statistical Analysis & Reporting
- Reproducible Research and R Markdown

📚 Project
- [Billboard & MusicOSet Audio Feature Analysis (IJC437 Project)](https://github.com/nishika-23/Billboard-Music-Trends-Analysis)

How to Run the Code in RStudio
1. Clone or Download the Repository:
   
🔹 Option A — Clone via Git (recommended)
If Git is installed on your computer:
git clone https://github.com/nishika-23/Billboard-Music-Trends-Analysis.git
Then open the cloned folder in RStudio.

🔹 Option B — Download ZIP
If you don’t use Git:
Click the green “Code” button on this GitHub page.
Select “Download ZIP.”
Extract the ZIP file to your Downloads or Desktop.
2. Open RStudio and Set the Working Directory
Once the files are extracted, tell R where the project lives.
# Replace the path below with your folder location
setwd("/Users/nishikathakkar/Downloads/Billboard-Music-Trends-Analysis")

# Check it worked:
getwd()
✅ You should see:
"/Users/nishikathakkar/Downloads/Billboard-Music-Trends-Analysis"
If you see that path, R is now “looking” in the correct folder.

3. Run the Analysis
Now simply run:
source("IDS_final.R")
The script will automatically:
-Load the dataset (billboard_24years_lyrics_spotify.csv)
-Clean and process the data
-Generate 10 figures (saved in /plots)
-Create 8 summary tables (saved in /output_tables)
-Produce the model output file (output_model_summary.txt)

You’ll see messages like:
Table 1 (Variable Descriptions) saved as output_variable_description.csv
Finished: CSVs in working folder, plots in /plots, model summary + performance table saved.
That means your analysis completed successfully ✅

  **Note:**  
  Make sure your R has these packages installed before running:
  'tidyverse`, `ggplot2`, `readr`, `dplyr`, `pROC`, `caret`, `cowplot`, and `vroom`.

  If not, install them by running:
  install.packages(c("tidyverse","ggplot2","readr","dplyr","pROC","caret","cowplot","vroom"))
  
  


