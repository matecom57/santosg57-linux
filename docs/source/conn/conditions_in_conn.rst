conditions in conn
===================

In the CONN toolbox for fMRI functional connectivity, conditions define the time periods or 
experimental states modeled across each subject's functional runs in the Setup > Conditions tab.


**Key Types and Uses**

• Resting-state (rest): By default, CONN creates a rest condition spanning the entire scan (onset: 0, duration: inf).

• Task or block designs: You define each task or experimental state by entering onsets and durations (in seconds or scans, depending on your setup) per subject and session/run.
• Multi-session / longitudinal designs (e.g., pre vs. post): You can create separate conditions for each session and assign 0 / inf to the target session while leaving non-applicable sessions empty ([]).
• Dynamic / sliding-window connectivity: Under Setup > Conditions, selecting temporal decomposition (sliding-window) in the time-frequency decomposition option generates time-windowed conditions for dynamic connectivity analyses.


**How Conditions Connect to Analysis**

• Denoising: Conditions influence task-related effects/regressors and how potential task-evoked effects are handled alongside confound regressors (such as white matter, CSF, and motion outliers).
• 1st-Level Analysis: Connectivity measures (such as ROI-to-ROI, seed-to-voxel, or gPPI) are estimated per condition.
• 2nd-Level Analysis: Conditions become the within-subject factors you contrast at the group level (e.g., comparing TaskA > TaskB, or Post > Pre).

You can also import condition timing automatically when importing SPM.mat files (Setup > Import) 
or BIDS datasets, or define them programmatically via batch scripts. You can check details on 
sliding-window setup in the CONN toolbox documentation.

(Note: If you meant current weather conditions in Connecticut or Conn, Ontario, let me know.)

Are you setting up resting-state, task/gPPI, multi-session, or sliding-window conditions? Sharing 
your number of sessions/runs and experimental design can help me give exact setup steps or 
contrasts.


