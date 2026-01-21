👋 Hi, I’m Nishika Thakkar

🎓 I’m a Data Science student at the University of Sheffield.  
🎵 My main interest is exploring trends in popular music using data-driven approaches.  
💻 I enjoy working with R, data visualization, and exploratory analysis.

🧠 Professional Skills
- Data Cleaning & Visualization (R, ggplot2, tidyverse)
- Predictive Modeling (caret, pROC)
- Statistical Analysis & Reporting
- Reproducible Research and R Markdown

📚 Projects
- [Billboard & MusicOSet Audio Feature Analysis (IJC437 Project)](https://github.com/nishika-23/Billboard-Music-Trends-Analysis)

⚙️ How to Run the Code

To replicate this analysis and generate all outputs on your own computer:

🧾 Step 1 — Download or clone the repository
You can either:
- Click the green **Code** button on this page → “Download ZIP”,  
  then extract the ZIP file to your Desktop,  
  **OR**
- If you use Git, open Terminal or Git Bash and run:
  git clone https://github.com/nishika-23/Billboard-Music-Trends-Analysis.git
  
🧭 Step 2 — Open in RStudio
1. Open **RStudio**  
2. Go to **File → Open Project…**  
3. Select the file: `Billboard-Music-Trends-Analysis.Rproj` (or open the folder manually)

▶️ Step 3 — Run the main R script
- In the R console, type: 
  source("IDS_final.R")
  
The script will automatically:
- Clean and process the dataset  
- Create visualisations in the `plots/` folder  
- Save summary tables in `output_tables/`  
- Store model results in `output_model_summary.txt`
  
  Once completed, you’ll see a message in the console confirming:
  Finished: CSVs in working folder, plots in /plots, model summary + performance table saved.

  **Note:**  
  Make sure your R has these packages installed before running:
  'tidyverse`, `ggplot2`, `readr`, `dplyr`, `pROC`, `caret`, `cowplot`, and `vroom`.

  If not, install them by running:
  install.packages(c("tidyverse","ggplot2","readr","dplyr","pROC","caret","cowplot","vroom"))
  
  


