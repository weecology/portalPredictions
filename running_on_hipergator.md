## Running portalPredictions on HiperGator

Full runs of portalPredictions are computationally intensive so we run the
weekly forecasts on the University of Florida's High Performance Computer, the
HiPerGator. 

### Official Forecasting Runs

This should only be setup by one user at a time. Currently this is Ethan White.

To set this up follow these steps:

1. Copy `portalcasting_weekly.sh` into the home directory of the user under
   which the job will run.
2. `ssh` from `hpg2` into the `daemon2` server.
3. Create a cronjob by running `crontab -e` and pasting the contents of
   `crontab.txt` into the resulting editor and replacing
   `WEECOLOGYDEPLOYGITHUBPAT` with a GitHub Personal Access token that has
   "Repository" permissions on the `weecology/portalPredictions` and
   `weecology/forecast` repositories (e.g., the `weecologydeploy` bot).

### Testing Runs

For development purposes it may be useful to set up unofficial runs that do not
get archived as official forecasts.

To set this up follow these steps:

1. Copy `portalcasting_weekly.sh` into the home directory of the user under
   which the job will run.
2. Change this copy of `portalcasting_weekly.sh` to use the users email
   address and clone the `portalPredictions` repository from the users fork.
3. Change `archive_hipergator.sh` to push to the users fork. Optionally this
   change could push to a branch in the users fork instead of `main` and remove everything below line 47 (the code for archiving to `weecology/forecasts`).
4. `ssh` from `hpg2` into the `daemon2` server.
5. Create a cronjob by running `crontab -e` and pasting the contents of
   `crontab.txt` into the resulting editor and replacing
   `WEECOLOGYDEPLOYGITHUBPAT` with a GitHub Personal Access token that has
   "Repository" permissions on the users fork.