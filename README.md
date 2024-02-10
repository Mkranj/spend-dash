# SpendDash v1.0  
Keep track of your spending habits over time.  
Live version of the app is [***here***](https://mkranj.shinyapps.io/SpendDash/).

## About:
***SpendDash*** is an online dashboard designed to show how your expenses change over time, on a monthly or daily basis. At a glance, you can see whether your latest expenses are on the rise or on the decline when compared to your usual monthly average. Furthermore, if your expenses are assigned categories, you can compare the monthly averages for each category over a period, or look up the expenses for only certain categories.  

Found SpendDash useful? How about [**buying me a coffee**](https://www.buymeacoffee.com/mkranj61) and supporting development? After all, coffee makes the world go round :star_struck:

Made in R language (4.3.2) with the Shiny framework. Tested on Google Chrome v121.

## Instructions:  
- [Open the app](https://mkranj.shinyapps.io/SpendDash/)
- The initial view uses sample data. Load your own via the **Read data from file** button

The data used for the initial layout is taken from [this Kaggle dataset](https://www.kaggle.com/datasets/tharunprabu/my-expenses-data).  
For using your own data, you can either construct the data yourself (you can use [this example Excel workbook](https://github.com/Mkranj/spend-dash/blob/master/example_spending.xlsx) as a starting point) or adapt data you acquired from other sources. E.g. if you can download your financial data from your bank's services, just make sure the column names are the same as described below and you're good to go!  

Valid data should look similar to this:  
![Data separated into Date, Amount, Category columns.](https://github.com/Mkranj/spend-dash/blob/master/www/expenses_ex.png?raw=true)

The file **must** contain columns named "*Date*" and "*Amount*" to be properly loaded. If a column called "*Category*" also exists, features related to charting and selecting individual categories will be enabled.  
Note that the Date column should contain dates, not datetimes (i.e. "*30.08.2023.*" and not "*30.08.2023. 11:55:30*").

## Privacy:  
Any data you upload can be viewed only by you and is deleted on the end of the session.  