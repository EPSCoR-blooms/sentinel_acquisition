# sentinel_acquisition
automated gathering of sentinel 2 a&b acquisition schedule for the EPSCoR lakes

scripts by B. Steele (steeleb@caryinstitute.org)

Note, that this method of detecting flyovers does not mean that the data will (a) be usable or (b) provide complete coverage of the lake. The matching method employed in this script is that of a spatial intersect - so if the Sentinel path intersects with a lake in any form, that acquisition data is included here. 

## Output: 
The file 'EPSCoRBlooms_Sentinel_future_acquisition_v[last update date].csv' is the primary output from this workflow, which lists each of the project lakes and when future Sentinel acquisitions are scheduled.

File column definitions:
|   Column Name     |   Column Definition   |   Units   |   Notes   |
| ----  |   ----    |   ----    |   ----    |
|   ID	|   Sentinel acquisition ID     |   alpha-numeric string |  this is a unique identifier for each path in the ESA-provided Sentinel path file    |
|   acquisition_date	|   the date of planned acquisition |   yyyy-mm-dd  |   |
|   acquisition_hour	|   the *starting* hour of the planned acquisition  |   HH  |   because the paths extend from the lowest latitudes to the highest latitudes, this is very approximate, and may be off by a few hours |
|   OBJECTID	| ArcGIS identifier from the shapefiles in the provided .gdb    |   numeric |   |
|   Permanent_	|   NHD Permanent ID provided by the USGS NHD Best Resolution dataset   |   alpha-numeric string    |   aka NHDID, PermID   |
|   GNIS_Name	|   Name of the waterbody, as stored in the NHD |   character string    |   this may not be the same as the common name of the lake |
|   LakeName |  Common name of the waterbody, as used in this project   |   character string    |   |
|   LakeID  |   Short 3-letter idenitity of the lake, as used in this project   |   character string    |   |
|   source	|   source .kml file for the planned aquisition dates   |   character string    |   |
|   sat | satellite obtaining data  |   character string    |    either S2a (Sentinel 2a) or S2b (Sentinel 2b)  |


## Workflow:
 - 'check_source_save.R' - this script checks to see if there are new schedules available from the ESA website. If there are, it sources the 'download_extract.R' script. If not, nothing happens.
 - 'download_extract.R' - this script will download and extract the flyover dates for the EPSCoRBlooms lakes for Sentinel 2a and 2b if sourced by the 'check_source_save.R' script.


## Additional folder and file descriptions:
    - archive: this folder contains previous acquisition schedules that are outdated or updated
    - checkdates: this folder contains the RDS files of the current Sentinel acqusition .kml files to check for updates
    - shapefiles: this folder contains a .gdb of the EPSCoRBlooms lakes shapefiles. This is a copy of the file in DartFS.


