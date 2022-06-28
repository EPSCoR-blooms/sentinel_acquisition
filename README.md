# sentinel_acquisition
automated gathering of sentinel 2 a&b acquisition schedule for the EPSCoR lakes

scripts by B. Steele (steeleb@caryinstitute.org)

Note, that this method of detecting flyovers does not mean that the data will (a) be usable or (b) provide complete coverage of the lake. The matching method employed in this script is that of a spatial intersect - so if the Sentinel path intersects with a lake in any form, that acquisition data is included here. 

Workflow:

