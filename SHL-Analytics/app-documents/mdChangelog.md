# Changelog

## 2.4.4 (2025-06-24)

-   Updated Casino tracker for 24 teams

## 2.4.3 (2025-06-03)

-   Updating tank standing tracker to account for new league structure
    in S83 and adding historical standing options.

## 2.4.2 (2025-04-30)

-   Updated audit tool to make use of snapshot Google Sheet to compare
    with Index data.

## 2.4.1

-   Added Tank Standings Tracker

## 2.4.0

-   Removed SVG logos for all teams to reduce loading time to around 5
    seconds.
-   Removed stat card visualization as the functionality was broken.
-   Removed draft and awards stats from career data as that is now
    present on the portal.
-   Added career data page that can be downloaded season by season.
-   Added page that shows current IIHF transfer limits with formatted
    outputs.

## v2.3.0

-   Scraper is now using SHL Portal API, so additional changes to tools
    need to be made.
    -   Draft Class Tracker now links to Player Portal, **Activity
        status is still tracked here.**
    -   Position Tracker changed to reflect only three lines in the
        SMJHL
    -   Team Tracker links to Player Portal Team Tracker.
    -   IIHF Tracker is updated to Portal IIHF Federation assignments.
    -   Fixed Russia’s move to Independent Russia in IIHF Rankings.

## v2.2.0

-   Updates to the scraper requires edits to most tools.

## v2.1.0

-   Adding a season scheduling tool for the SMJHL.

## v2.0.0

-   Updated the app to mostly make use of a database of the information
    instead of using csv and/or Google Sheets.

## v1.2.0

-   Fixed Career data for skaters.

## v1.1.9

-   Fixes to the position tracker

## v1.1.8

-   Fixes update to the IIHF Ranking calculations based on a new
    function for reading from the index.

## v1.1.7

-   Changed the regression percentages to the new rules after the two
    transitional seasons.

## v1.1.6

-   Added teams to the Audit Tool to more easily identify where a player
    plays.

## v1.1.5

-   Added an Audit tool that checks the FHM exports vs the forum
    attributes.

## v1.1.4

-   Changed the Regression percentages to correspond to S64 special
    groups as set in [this
    post](https://simulationhockey.com/showthread.php?tid=120657).

## v1.1.3

-   Changed the Regression percentages to correspond to S63 special
    groups as set in [this
    post](https://simulationhockey.com/showthread.php?tid=120657).
-   Added direct linked urls for every subpage.
-   Added dynamic max season for Visualize MDS Tool.

## v1.1.2

-   Added positional filters on the Player Draft Class Tracker similar
    to the IIHF Player Tracker.

## v1.1.1

-   Added IIHF ranking history, link to the raw data and interactive
    graph.

## v1.1.0

-   Added team pages to look at rosters and depth sheets
-   Revised column names for IIHF Rankings

## v1.0.2

-   Bugfix for the scheduler tool as it processed a shorter schedule
    than intended at times.
-   Corrected Player Card tool to enable buttons after first
    visualizations.
-   Changed the application logo.

## v1.0.1

-   Minor corrections in titles and text.
-   Grouped older tools into its own menu.
-   Added a Draft Lottery Tool that uses graphics from the relevant
    teams. Currently only four picks to conform to the SHL Lottery.
    SMJHL is in the works.
-   Corrected how the Regression Tool calculates age.

## v1.0.0

-   Restructured the career pages.
-   Corrected historical data. (Shooter McGavin in S49 is still weird)
-   IIHF rankings are correct, the error occurred in IIHF Google Sheet.
-   Changed default sorting in IIHF Eligibility tool.
-   Changed the columns shown in the Regression Tool as it only looks at
    claimed and updated TPE (not earned TPE).

## v0.99.2

-   Added first version of a Regression tool.
-   Created the IIHF as a dedicated menu that includes the previously
    created Eligibility tracker.
    -   Added IIHF Rankings that has some discrepancies with the IIHF
        Google Sheet used.

## v0.99.1

-   Added more QoL functions such as processing messages when data takes
    a while to load.

-   Added more descriptive text for all tools to make functions and
    output more understandable.

-   Added changelog for transparency of changes.

-   Added ALL to the Draft Class Tracker that allows for a look at all
    players at once.

## v0.99.0

The first publicly published build with the tools specified in [forum
post](https://simulationhockey.com/showthread.php?tid=117606).
