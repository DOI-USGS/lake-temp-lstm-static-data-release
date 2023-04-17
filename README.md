# Daily water column temperature predictions for thousands of Midwest U.S. lakes between 1979-2022 and under future climate scenarios

This data release pipeline contains the recipes used to combine data from a variety of repositories and ultimately produce the data release, _"Daily water column temperature predictions for thousands of Midwest U.S. lakes between 1979-2022 and under future climate scenarios"_ ([doi:10.5066/P9EQQER7](https://doi.org/10.5066/P9EQQER7)). The data prep and modeling repositories that support this release are:

1. [`lake-temperature-model-prep`](https://github.com/USGS-R/lake-temperature-model-prep)
2. [`lake-temperature-process-models`](https://github.com/USGS-R/lake-temperature-process-models)
3. [`lake-temperature-lstm-static`](https://github.com/USGS-R/lake-temperature-lstm-static)
4. [`lake-temperature-out`](https://github.com/USGS-R/lake-temperature-out)

## Building this pipeline

This pipeline is being built on the USGS Tallgrass HPC system in order to facilitate the necessary connections to 4 other repositories with data built on Tallgrass and available through Caldera. Follow instructions in the DSP manual in order to start an R session on Tallgrass. If you are building this full pipeline, you will probably need to also connect to the Google Drive folder to ensure that files are updated (see `Setting up GD` below).

Note that building this full pipeline is lengthy because of the size of the various files and munging that takes place. It will take hours, so it might be best to let it go overnight. 

## Setting up GD

Since the code uses `scipiper::gd_get()` along with `lake-temperature-model-prep`, you will likely need to setup authorization to the Google Drive folder if you are to build targets the call `scipiper_freshen_files()`. To do so, follow these instructions.

To allow `gd_get()` to actually download files, you need to prep your credentials to avoid the browser-mediated authorization (does not work on the HPC systems). I used the "Project-level OAuth cache" section of [this vignette](https://cran.r-project.org/web/packages/gargle/vignettes/non-interactive-auth.html) to develop this workflow. You should only need to follow steps 1-3 one time:

* *Step 1:* Locally, run the following to authorize GoogleDrive and create a token file. Important: DON'T COMMIT THIS FILE ANYWHERE. You only need to do this the one time. Once you have this setup, you can skip to Step 4.

```
options(gargle_oauth_cache = ".secrets")
googledrive::drive_auth(cache = ".secrets")
```

* *Step 2:* Upload the file to the `.secrets/` directory in `lake-temperature-model-prep/` on Caldera. Be sure that `.secrets/*` appears in the gitignore (it already should, but please check!).

* *Step 3:* Verify that the authorization will work by running the following code. If it returns at least one file, then you can carry on with the build. The `options()` here will need to be run every time you are building the pipeline (unless everything has been "freshened" already).

```
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "YOUREMAIL@gmail.com"
)
googledrive::drive_find(n_max = 1)
```

* *Step 4:* The `.Rprofile` file in this repo is currently setup to load `scipiper` and set the gargle options described above. Update that file as needed so that these options are automatically set when you start R on Tallgrass.

The above authentication workflow stopped working when I returned to this in March 2023. To save time, I manually uploaded the freshened files to the appropriate location myself and the workflow is able to keep going. E.g. I downloaded the raw `lake-temperature-model-prep/7b_temp_merge/out/temp_data_with_sources.feather` file (not the `.ind`) and uploaded to Caldera manually. Then, I reran this workflow and it says "Freshening ..." but it skips the actual `sc_retreive()` step that downloads from GD since the file matches the ind hash.
