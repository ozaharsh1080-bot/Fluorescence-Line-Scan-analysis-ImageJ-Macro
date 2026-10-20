/*
 * Macro: Fluorescence Line Scan Extractor
 * Authors: Harsh Oza (developed with AI assistance)
 * Description: An ImageJ/Fiji macro designed to automate fluorescence line scan 
 *              extraction from multi-channel biological images (.lif composites, 
 *              z-stacks, single slices, or separate individual TIF files). 
 *              Features interactive channel merging, custom LUT and naming options, 
 *              ROI-specific cropping, auto-contrast enhancement, and automated 
 *              raw data (.csv) and visual profile (.tif) export.
 */

macro "Smart Universal Line Scan [F2]" {
    // --- 1. SETUP & PRE-CHECK ---
    roiManager("reset");
    run("Clear Results");
    
    if (nImages == 0) exit("Please open an image or stack first.");
    
    // Check what kind of image is currently active
    Stack.getDimensions(w, h, c, s, f);
    
    // --- 2. THE SMART MERGE PROMPT ---
    // If multiple images are open, and the active one is just a single channel, ask to merge!
    if (nImages > 1 && c == 1) {
        mergeChoice = getBoolean("Multiple separate images detected.\nDo you want to merge them into a single multi-channel image before starting?");
        
        if (mergeChoice) {
            imgList = getList("image.titles");
            titles = newArray(imgList.length + 1);
            titles[0] = "*None*";
            for (i=0; i<imgList.length; i++) titles[i+1] = imgList[i];
            
            Dialog.create("Merge Images");
            Dialog.addMessage("Select your open TIF images to combine them.\nLeave as '*None*' if you don't have a 3rd or 4th image.");
            Dialog.addChoice("Channel 1:", titles, titles.length>1 ? titles[1] : titles[0]);
            Dialog.addChoice("Channel 2:", titles, titles.length>2 ? titles[2] : titles[0]);
            Dialog.addChoice("Channel 3:", titles, titles.length>3 ? titles[3] : titles[0]);
            Dialog.addChoice("Channel 4:", titles, titles.length>4 ? titles[4] : titles[0]);
            Dialog.show();
            
            c1 = Dialog.getChoice();
            c2 = Dialog.getChoice();
            c3 = Dialog.getChoice();
            c4 = Dialog.getChoice();
            
            mergeCmd = "";
            if (c1 != "*None*") mergeCmd += "c1=[" + c1 + "] ";
            if (c2 != "*None*") mergeCmd += "c2=[" + c2 + "] ";
            if (c3 != "*None*") mergeCmd += "c3=[" + c3 + "] ";
            if (c4 != "*None*") mergeCmd += "c4=[" + c4 + "] ";
            if (mergeCmd == "") exit("No images selected for merging.");
            
            run("Merge Channels...", mergeCmd + " create keep");
        }
    }
    
    // --- 3. RE-CHECK DIMENSIONS ---
    Stack.getDimensions(width, height, channels, slices, frames);
    origID = getImageID();
    origTitle = getTitle();
    
    outputDir = getDirectory("Choose Main Output Directory");
    if (outputDir == "") exit("User cancelled.");
    
    // --- NEW: EXTRACT FOLDER NAME FOR PREFIXING ROIs ---
    tempDir = outputDir;
    if (endsWith(tempDir, File.separator)) {
        tempDir = substring(tempDir, 0, lengthOf(tempDir)-1);
    }
    baseFolderName = File.getName(tempDir);
    
    lutOptions = newArray("Magenta", "Yellow", "Green", "Blue", "Red", "Cyan", "Grays", "Fire");
    
    // --- 4. DYNAMIC DIALOG FOR SETTINGS ---
    Dialog.create("Channel Settings (" + channels + " Channels Detected)");
    Dialog.addMessage("Set names and colors (LUTs) for your channels:");
    
    for (ch = 1; ch <= channels; ch++) {
        Dialog.addString("Name Ch " + ch + ":", "Channel_" + ch);
        defaultLut = "Grays";
        if (ch == 1) defaultLut = "Magenta";
        else if (ch == 2) defaultLut = "Yellow";
        else if (ch == 3) defaultLut = "Green";
        else if (ch == 4) defaultLut = "Blue";
        Dialog.addChoice("LUT Ch " + ch + ":", lutOptions, defaultLut);
    }
    
    // --- NEW: IMAGE SAVING OPTIONS ---
    Dialog.addMessage("--- Image Saving Options ---");
    Dialog.addCheckbox("Auto-Enhance Contrast before saving (Saturated=0.1)", true);
    Dialog.addCheckbox("Save cropped images around ROI instead of full image", false);
    Dialog.addNumber("Crop Box Size (in image units, e.g., 50 um):", 50);
    
    Dialog.show();
    
    chNames = newArray(channels);
    luts = newArray(channels);
    for (ch = 0; ch < channels; ch++) {
        chNames[ch] = Dialog.getString();
        luts[ch] = Dialog.getChoice();
    }
    
    // Read the new options
    doAutoContrast = Dialog.getCheckbox();
    doCrop = Dialog.getCheckbox();
    cropSize = Dialog.getNumber();

    // --- 5. USER INTERACTION ---
    setTool("line");
    roiManager("Associate", "true");
    if (slices > 1) {
        waitForUser("Draw Lines", "1. Scroll through your Z-stack.\n2. Draw a line over your structure.\n3. Press 't' to add it to the ROI Manager.\nRepeat for as many lines as you want.\n4. Click OK when finished.");
    } else {
        waitForUser("Draw Lines", "1. Draw a line over your structure.\n2. Press 't' to add it to the ROI Manager.\nRepeat for as many lines as you want.\n3. Click OK when finished.");
    }
    
    nROIs = roiManager("count");
    if (nROIs == 0) exit("No ROIs were added to the manager.");
    
    // --- 6. AUTOMATED EXTRACTION ---
    setBatchMode(true);
    for (i = 0; i < nROIs; i++) {
        // --- MODIFIED: ADDED FOLDER NAME TO ROI DIRECTORY ---
        roiDir = outputDir + baseFolderName + "_ROI_" + (i+1) + File.separator;
        File.makeDirectory(roiDir);
        
        selectImage(origID);
        roiManager("select", i);
        Stack.getPosition(channel, slice, frame);
        
        print("Processing ROI " + (i+1) + " at Z-Slice: " + slice);
        
        // --- Process Each Channel ---
        for (ch = 1; ch <= channels; ch++) {
            
            selectImage(origID);
            run("Duplicate...", "title=tempC duplicate channels=" + ch + "-" + ch + " slices=" + slice + "-" + slice);
            
            selectWindow("tempC");
            roiManager("select", i); 
            
            // Extract the pure, raw data profile
            run("Plot Profile");
            Plot.getValues(xpoints, ypoints);
            run("Clear Results");
            
            for (j = 0; j < xpoints.length; j++) {
                setResult("Distance", j, xpoints[j]);
                setResult("Gray_Value", j, ypoints[j]);
            }
            updateResults();
            saveAs("Results", roiDir + chNames[ch-1] + ".csv");
            close("Plot*"); 
            
            // Formatting
            selectWindow("tempC");
            if (doAutoContrast) {
                run("Enhance Contrast", "saturated=0.1");
            }
            run(luts[ch-1]);
            run("RGB Color");
            
            // Draw ROI line overlay
            roiManager("select", i);
            Roi.setStrokeColor("white");
            Roi.setStrokeWidth(2);
            run("Add Selection..."); 
            
            // --- NEW: CROPPING LOGIC ---
            if (doCrop) {
                // Find the center of the drawn line
                getSelectionBounds(rx, ry, rw, rh);
                cx = rx + rw/2;
                cy = ry + rh/2;
                
                // Convert physical size (e.g. 50 um) to pixels
                getVoxelSize(pw, ph, pd, unit);
                cW = cropSize / pw;
                cH = cropSize / ph;
                
                startX = cx - cW/2;
                startY = cy - cH/2;
                makeRectangle(startX, startY, cW, cH);
                run("Crop");
            }
            
            run("Select None");
            run("Scale Bar...", "width=5 height=5 thickness=5 font=0 color=White background=None location=[Lower Right] bold overlay hide");
            run("Flatten");
            
            saveAs("Tiff", roiDir + chNames[ch-1] + ".tif");
            close(); 
            selectWindow("tempC");
            close(); 
        }
        
        // --- Process Merged Image (If > 1 channel) ---
        if (channels > 1) {
            selectImage(origID);
            run("Duplicate...", "title=tempMerge duplicate channels=1-" + channels + " slices=" + slice + "-" + slice);
            
            for (ch = 1; ch <= channels; ch++) {
                Stack.setChannel(ch);
                if (doAutoContrast) {
                    run("Enhance Contrast", "saturated=0.1");
                }
                run(luts[ch-1]);
            }
            
            Stack.setDisplayMode("composite");
            run("RGB Color"); 
            
            roiManager("select", i);
            Roi.setStrokeColor("white");
            Roi.setStrokeWidth(2);
            run("Add Selection...");
            
            // --- NEW: CROPPING LOGIC FOR MERGE ---
            if (doCrop) {
                getSelectionBounds(rx, ry, rw, rh);
                cx = rx + rw/2;
                cy = ry + rh/2;
                getVoxelSize(pw, ph, pd, unit);
                cW = cropSize / pw;
                cH = cropSize / ph;
                startX = cx - cW/2;
                startY = cy - cH/2;
                makeRectangle(startX, startY, cW, cH);
                run("Crop");
            }
            
            run("Select None");
            run("Scale Bar...", "width=5 height=5 thickness=5 font=0 color=White background=None location=[Lower Right] bold overlay hide");
            run("Flatten");
            
            saveAs("Tiff", roiDir + "Merged.tif");
            
            close(); 
            selectWindow("tempMerge");
            close(); 
        }
    }
    
    setBatchMode(false);
    selectImage(origID); 
    run("Clear Results");
    roiManager("show all"); 
    showMessage("Success", "Extracted " + nROIs + " ROIs successfully to:\n" + outputDir);
}