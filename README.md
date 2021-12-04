# stockpredition-app

This application is an extension of thorough data gathering project, merging information from several different vendors and databases regarding trading of the ticker symbol SPY (S&P 500 SPYDR ETF) during a period from 2019-2021. The data is located in a .csv file which was used in the original research project that can be found on the main Github page in a pdf file. 

This application works to leverage exploratory data analysis through univariate and multiple variable visulizations, along with a reapproach to the final model dervied from the original research. This time the final model works in a quantitative (numerical) fashion with predictors and outputs, whereas the original model relied on ordinal outputs and inputs in addition to numerical data. The variables of interest are identical in the models, they have just been reduced back to thier non-discretized numerical form. 

# 1. Download the Final_Application.R File from github

		https://github.com/wildcatjayhawk/DATA-824/blob/8570be3b709d00c9ac24bbda15ff832db9372171/Final_Application.R

# 2. Launch R Studio 

# 3. Open the downloaded file with R Studio. It should automatically read it as an application into R. 

# 4. Make sure all libraries listed at top of the .R file are installed
	
	Any non-installed applications can be installed via the command:
		
			> install.packages("PackageName")
			
	Where, PackageName is the name of the library you are missing. 
		
# 5. Run the application within R Studio by either clicking the "Run" icon or by selecting all the code and pressing "cmd + enter". 

# 6. The application should download the source files from Github needed for the data analysis, and any related dependencies. If not, consider downloading the .csv data file found on Github and reading the data in manually from you local workspace. 

# 7. The application will launch into a broswer if deployed successfully where all inputs and controls are accessed there. 

# 8. Enjoy and let me know of any issues!

```
Disclaimer: this application and related datasets, functions, graphics, and other metrics and interpretations are strictly for entertainment purposes. Nothing about the results from this application or the models in the application should be interpreted as trading advice. Day trading is risky and these decisions should be left to a licensed broker or agent. These findings are in no way to be used for trading advice and decisions for individuals or entities. 
```
