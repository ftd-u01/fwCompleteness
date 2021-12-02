library(tidyverse)

completeness = data.frame(SessionID = character(), SubjectLabel = character(), SessionLabel = character(), HasSMRI = logical(),
                          SMRIComplete = logical(), HasDMRI = logical(), DMRIComplete = logical(), HasRFMRI = logical(), 
                          RFMRIComplete = logical(), HasFMaps = logical(), FMapsComplete = logical(), HasASL = logical(), 
                          ASLComplete = logical(), HasTFMRI = logical(), TFMRIComplete = logical(), AnyIncomplete = logical()) 

acq_spec = spec_csv("HCPMultiCenterAcquisitionCompleteness.csv")
acq_spec$cols$SubjectLabel = col_character()
acq_spec$cols$NumDicomFiles = col_integer()
acq_spec$cols$StudyDate = col_integer()

asl_series = c("SPIRAL_V20_HCP_ASL","SPIRAL_V20_HCP_M0") # don't much care about MeanPerf
dmri_series = c("dMRI_dir98_AP","dMRI_dir98_AP_SBRef","dMRI_dir98_PA","dMRI_dir98_PA_SBRef","dMRI_dir99_AP","dMRI_dir99_AP_SBRef","dMRI_dir99_PA","dMRI_dir99_PA_SBRef")
fmap_series = c("SpinEchoFieldMap_AP","SpinEchoFieldMap_PA")
rfmri_series = c("rfMRI_REST_AP","rfMRI_REST_AP_SBRef","rfMRI_REST_PA","rfMRI_REST_PA_SBRef")
smri_series = c("T1w_MPR","T2w_SPC")
tfmri_series = c("tfMRI_GAMBLING_AP","tfMRI_GAMBLING_AP_SBRef","tfMRI_GAMBLING_PA","tfMRI_GAMBLING_PA_SBRef","tfMRI_WM_AP","tfMRI_WM_AP_SBRef","tfMRI_WM_PA","tfMRI_WM_PA_SBRef")

acquisitions = read_csv("HCPMultiCenterAcquisitionCompleteness.csv", col_types = acq_spec)

all_sessions = unique(acquisitions$SessionID)

for (i in 1:length(all_sessions)) {
    completeness[i,] = NA
    completeness$SessionID[i] = all_sessions[i]
    session_acquisitions = filter(acquisitions, SessionID == all_sessions[i], Ignore == FALSE)
    completeness$SubjectLabel[i] = session_acquisitions$SubjectLabel[1]
    completeness$SessionLabel[i] = session_acquisitions$SessionLabel[1]
    
    by_series = group_by(session_acquisitions, SeriesDescription)
    
    series_counts = summarize(by_series, n = n(), Complete = sum(Complete == TRUE))
    
    completeness$AnyIncomplete[i] = sum(series_counts$n - series_counts$Complete) > 0 
    
    complete_series = deframe(series_counts[,c(1,3)])
    total_series = deframe(series_counts[,1:2])
    
    # ASL
    
    completeness$HasASL[i] = any(total_series[asl_series] > 0, na.rm = T)
    
    complete_asl = complete_series[asl_series]
    
    if ( any(is.na(complete_asl)) ) {
        completeness$ASLComplete[i] = FALSE
    }
    else {
        completeness$ASLComplete[i] = all(complete_asl > 0)
    }
    
    # DMRI
    
    completeness$HasDMRI[i] = any(total_series[dmri_series] > 0, na.rm = T)
    
    complete_dmri = complete_series[dmri_series]
    
    if ( any(is.na(complete_dmri)) ) {
      completeness$DMRIComplete[i] = FALSE
    }
    else {
      completeness$DMRIComplete[i] = all(complete_dmri > 0)
    }
    
    # RFMRI
    
    completeness$HasRFMRI[i] = any(total_series[rfmri_series] > 0, na.rm = T)
    
    complete_rfmri = complete_series[rfmri_series]
    
    if ( any(is.na(complete_rfmri)) ) {
      completeness$RFMRIComplete[i] = FALSE
    }
    else {
      completeness$RFMRIComplete[i] = all(complete_rfmri > 1)
    }
    
    # Structural 

    completeness$HasSMRI[i] = any(total_series[smri_series] > 0, na.rm = T)
    
    complete_smri = complete_series[smri_series]
    
    if ( any(is.na(complete_smri)) ) {
      completeness$SMRIComplete[i] = FALSE
    }
    else {
      completeness$SMRIComplete[i] = all(complete_smri > 0)
    }
    
    # tfmri
    
    completeness$HasTFMRI[i] = any(total_series[tfmri_series] > 0, na.rm = T)
    
    complete_tfmri = complete_series[tfmri_series]
    
    if ( any(is.na(complete_tfmri)) ) {
      completeness$TFMRIComplete[i] = FALSE
    }
    else {
      completeness$TFMRIComplete[i] = all(complete_tfmri > 0)
    }
    
    # fmaps
    completeness$HasFMaps[i] = any(total_series[fmap_series] > 0, na.rm = T)
    
    complete_fmaps = complete_series[fmap_series]
    
    if ( any(is.na(complete_fmaps)) ) {
        completeness$FMapsComplete[i] = FALSE
    }
    else{ 
        if (completeness$HasTFMRI[i]) {
            completeness$FMapsComplete[i] = all(complete_series[fmap_series] > 2)
        }
        else {
            completeness$FMapsComplete[i] = all(complete_series[fmap_series] > 1)
        }
    }
}

write_csv(completeness, "HCPMultiCenterSessionCompleteness.csv")
