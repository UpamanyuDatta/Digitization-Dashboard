# Digitization-Dashboard
This repository contains the steps from Oracle DB to Shiny dashboard

The codes provided here are under the maual import scenario, that is, csv files are created 
by manually exporting data via SQL commands.

However, <br>
a) If an ODBC connection is made,<br>
b) then after setting DSN and SQL Fetch commands<br>
the 'import' codes would be redundant. But we'd still need the manipulation codes to create our first R object.

##Import Codes
The file Import-R.R contains the relevant code to import, do some basic manipulations and set up the first R image which contains the imported objects, ready to be passed through the next set of codes.