function img = readImage(roiName)

hdr = spm_vol(roiName);
img = spm_read_vols(hdr);
    
end