## Project Description:

Quite awhile ago now, the Monitoring group put together a [5 year summary of our inland water data for the years 2006 – 2010](https://portal.ct.gov/-/media/DEEP/water/water_quality_management/monitoringpubs/coreelementsummaryreport20062010pdf.pdf?la=en) .  It contains information on the types of data we collect and a summary of the range of conditions we saw in CT inland waters over that period of time.  To date, we have not updated this report or completed another report like this for 2011 – 2015.  My goal is to create a pared down web-based report that can be automatically updated in the future as we collect more information.  We will also only focused on our ambient water collection in wadable rivers and streams.  This will include our water chemistry data (e.g. Total Phosphorus, Chloride, Turbidity etc.), biological data (e.g. bugs, fish, diatoms) and water temperature data.  In addition, some of the calculations we will put together will help inform some of our QA process going forward. 

We will be mostly be using R and SQL and then maybe a little HTML/CSS/Javascript to complete the project.

To start we will focus on the water chemistry data.  I have compiled and cleaned up data needed for this portion of the project.  Sample collection data can be found in the [data folder: monrpt.db](https://github.com/ctdeepwatermonitoring/5YrMonitoringRpt/tree/master/data) file.  This is a sqlite database which can be queried in R (see examples in ‘5YrMonRpt_Summary.R) or by using an SQLite database manager.  I suggest [SQLite studio](https://sqlitestudio.pl/index.rvt) [github link for latest release](https://github.com/pawelsalawa/sqlitestudio/releases) .  SQLite studio is a good place to start playing around with some SQL queries and getting familiar with the data.  [Below](#dbtables) is a description of the tables and fields in the tables.  You should have a better idea for the data and fields below after reading through the [2006 – 2010 report](https://portal.ct.gov/-/media/DEEP/water/water_quality_management/monitoringpubs/coreelementsummaryreport20062010pdf.pdf?la=en) .  If you are not familiar with our drainage basin system (i.e. watersheds) you can find some additional information on our CT eco page maintained by UCONN Clear:  [Major Basins](https://cteco.uconn.edu/guides/Major_Basin.htm) [Subregional Basins](https://cteco.uconn.edu/guides/Subregional_Basin.htm) . 

In addition to the database, there is also a csv file in the data folder called [site_env.csv](https://github.com/ctdeepwatermonitoring/5YrMonitoringRpt/tree/master/data) .  This contains landscape information for each of the sites such as drainage area, total upstream stream miles, percent impervious cover etc.  Included [below](#landscape) is a description of some of relevant fields we will use for the project.

See [below](#getStarted) for some ideas to get started.  We first want to get gain familiarity with the data and get comfortable with SQL, R and some programming foundations.

## <a name="getStarted"></a> Getting Started

1. Read through the [2006 – 2010 report](https://portal.ct.gov/-/media/DEEP/water/water_quality_management/monitoringpubs/coreelementsummaryreport20062010pdf.pdf?la=en)

2. Clone [5YrMonitoringRpt GitHub repository](https://github.com/ctdeepwatermonitoring/5YrMonitoringRpt) to a local repository where you will be working.

3. Practice some select SQL queries using SQLite in a SQLite database manager or R.

4.  Go through the examples in the [‘5YrMonRpt_Summary.R’ script](https://github.com/ctdeepwatermonitoring/5YrMonitoringRpt/blob/master/5YrMonRpt_Summary.R) .

5.  Try writing some code to answer the questions at the bottom of the script.

6. As you work, push changes to the repo frequently!  By doing so I can peak in to help as needed and collaborate.

7. Post your progress, changes, comments, questions, thoughts or anything else project related on the slack channel. 

### <a name="dbtables"></a> Description of data in DB Tables

---

chemdata 

* sta_seq – unique site identifier  (pk)
* lab_accession – accession number from the laboratory (pk)
* collect_date – data sample was collected
* chemparameter -  the chemical parameter that was collected (pk)
* value – the chemical parameter value
* uom – the chemical parameter value unit of measure
* relt_det_cond – the result detection condition (less than the minimum detection limit)
* method_det_limit – the method detection limit (not available for all data use the mdl table)
* activity_type – activity type
* activity_name – activity name
* station_type – the type of station
* duplicate – 0 or 1 (no, yes) notes if it is a duplicate sample (sample taken for QA purposes) (pk)

sites

* sta_seq – unique site identifier (pk)
* name – name of waterbody
* descript – description of waterbody location
* town – town waterbody is located in
* adb_seq – assessment database segment number
* ylat – site latitude
* xlong – site longitude
* sbasn – subregional basin number

basin

* sbasn – subregional basin number (pk)
* subregion – subregional basin name
* rbasn -  regional basin number
* regional – regional basin name
* mbasn – major basin number
* major – major basin name

mdl

* chemparameter -  the chemical parameter that was collected (pk)
* MDL – the minimum detection limit for a parameter

### <a name="landscape"></a> Landscape database (relevant fields we will use in this project below)

---

* sta_seq – unique site identifier (pk)
* name – name of waterbody
* ylat – site latitude
* xlong – site longitude
* Strahler – strahler stream order
* StrMi – total upstream stream miles
* SqMi – total upstream drainage basin in miles
* IC_Avg – Percent impervious cover in upstream drainage basin
* SD_Pct - Percent stratified drift
