# SpendDash v1.1  
Keep track of your spending habits over time.  
Access the live version of the app [***here***](https://spenddash.live/).  
A backup is hosted [***here***](https://mkranj.shinyapps.io/SpendDash/).

## About:
***SpendDash*** is an online dashboard designed to show how your expenses change over time, on a monthly or daily basis. At a glance, you can see whether your latest expenses are on the rise or on the decline when compared to your usual monthly average. Furthermore, if your expenses are assigned categories, you can compare the monthly averages for each category over a period, or look up the expenses for only certain categories.  

Found SpendDash useful? How about [**buying me a coffee**](https://www.buymeacoffee.com/mkranj61) and supporting development? After all, coffee makes the world go round :star_struck:

Written in R (4.3.2) with the Shiny framework. Tested on Google Chrome v121.

## Instructions:  
- [Open the app](https://mkranj.shinyapps.io/SpendDash/)
- The initial view uses sample data. Load your own via the **Read data from file** button

The data used for the initial layout is artificially generated.  
For using your own data, you can either construct the data yourself (you can use [this example Excel workbook](https://github.com/Mkranj/spend-dash/blob/master/example_spending.xlsx) as a starting point) or adapt data you acquired from other sources. E.g. if you can download your financial data from your bank's services or a finance tracking app, just make sure the column names are the same as described below and you're good to go!  
Supported filetypes: **.xlsx** and **.csv**.  

Valid data should look similar to this:  
![Data separated into Date, Amount, Category columns.](https://github.com/Mkranj/spend-dash/blob/master/www/expenses_ex.png?raw=true)

The file **must** contain columns named "*Date*" and "*Amount*" to be properly loaded. If a column called "*Category*" also exists, features related to charting and selecting individual categories will be enabled. The dashboard will accommodate **any category found in the data**, they are not restricted to those shown for sample data.  
Note that the Date column should contain dates, not datetimes (i.e. "*30.08.2023.*" and not "*30.08.2023. 11:55:30*").

## Privacy:  
Any data you upload can be viewed only by you and is deleted at the end of the session.  

## Running the app locally:  

- Download this repository
- Install the **renv** package if you haven't already
- Open the project folder and run `renv::restore()` to install all dependencies for this project
- Run `app.R`  

Alternatively, if you prefer using **Docker**,  

- Download this repository 
- Build an image using the provided `dockerfile`
- Create a container from that image, exposing the inside port 8180 to a port of your choosing

Example:  
`docker run --publish 8180:1234 mkranj/spenddash`
