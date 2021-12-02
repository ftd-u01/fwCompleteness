# fwCompleteness
Scripts for checking completeness of MR sessions on Flywheel 

Run `tabulate_acquisitions.py` to query Flywheel for all acquisitions. This will
list what acquisitions exist, and whether they are complete.

The output from the tabulate script is combined at the session level in
`completeness.R`.

To do:

 - Show repeat scans for each modality
