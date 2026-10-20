# Linescan (Intensity Plot profile) Analysis Workflow


PART 1: RUNNING THE IMAGEJ MACRO (DATA EXTRACTION)

**Prerequisites:**
- ImageJ or Fiji installed on your computer.

**Step-by-Step Instructions:**
1. Open ImageJ or Fiji.
2. Drag the image to ImageJ to open it. Open the image as **Composite**. Do not split the channels for ease.
3. Drag the ImageJ macro file (`.ijm`) into Fiji to open it, or simply copy and paste the macro code into a new macro window and click **Run** inside the editor.
4. **Smart-Merge Prompt (Optional):**
   - If you have multiple separate single-channel TIF files open, the macro will prompt you to merge them into a single multi-channel image. Select your channels or click `*None*` to skip.
5. **Select Output Directory:**
   - Choose or create a main output folder where your extracted ROI folders will be saved.
6. **Configure Channel Settings:**
   - In the pop-up dialog, assign custom names (e.g., `Protein_A`, `Protein_B`) and select display LUT colors for each channel.
   - Choose your image-saving preferences, such as **Auto-Enhance Contrast** or enabling **ROI-specific cropping**.
7. **Draw Lines (ROIs):**
   - Use the line tool from the ImageJ toolbar to draw line scans across your structures of interest.
   - Press **`t`** on your keyboard to add each line to the ROI Manager. Repeat for as many regions as needed.
   - Click **OK** on the prompt when finished.
8. **Output Generation:**
   - The macro automatically processes each ROI, saving individual raw intensity data (`.csv`), visual line profiles (`.tif`), and merged images into organized subfolders.

---


PART 2: RUNNING THE R SCRIPT (COLOCALIZATION & REPORTING)

**Prerequisites:**
- R and RStudio installed.
- Quarto CLI downloaded and installed from [quarto.org](https://quarto.org/) (required to compile HTML reports).
- The script contains an automated package check that will prompt you to install missing R packages (`tidyverse`, `openxlsx`, `quarto`) on its first run.

**Step-by-Step Instructions:**
1. Open the provided R script in RStudio. 
2. Select the whole code (`Ctrl + A` / `Cmd + A`) and run/source the entire script.
3. **Select Main Folder:**
   - A system window will pop up prompting you to select your main output folder (the same directory where the ImageJ macro saved your ROI subfolders).
4. **Configure GUI Settings:**
   - A scrollable configuration window will appear.
   - **Channel Colors & Line Widths:** Customize how each channel looks on graphs.
   - **Axes:** Verify your X-axis (`Distance`) and Y-axis (`Gray_Value`) column names.
   - **Global Settings:** Select channels to include in colocalization, toggle **"Normalize main graph"** on/off, and select image export formats (`png`, `pdf`, etc.).
   - Click **OK**.
5. **Automatic Processing & Report Compilation:**
   - The script will recursively scan all ROI subfolders, calculate Pearson's correlation coefficient ($r$) between selected channels, generate raw/normalized plots, and compile master CSV summaries.
   - Finally, it will automatically write and render an HTML report complete with a Table of Contents sidebar, mathematical formulas, and embedded results.
6. **Review Results:**
   - Check your ROI folders for individual plots and CSVs, and look inside the newly created **`Master Stats`** folder for aggregated summary tables and the compiled HTML report.


Feel free to edit as per your requirement and share with others.
---
> ⚠️ **Note:** This code has been tested, but please confirm using one or two controls manually to be sure, as the R script was developed via AI-assisted (vibe) coding.
