## Welcome to the repository for the Bias Detection Showdown App
&nbsp;
&nbsp;
&nbsp;

Here is a link to the app:
https://hubertplisiecki.shinyapps.io/pcurve_vs_zcurve/


## Code Book:

1. app.R - A file with the main code for the shiny app.

2. simulation_functions - contains files used for data simulation

	2a. carter_sim.R - A file with the simulation code from the Carter and colleagues article (2019)

	2b. censor_func.R - A file with the simulation biasing functions from the Carter and colleagues article (2019)

	2c. renkewitz_sim.R - A file with the simulation functions from the Renkewitz and Keiner article (2019) - slightly modified
 
3. markdown_files - contain files used for text display
       
	3a. conclusion1.Rmd - An Rmarkdown file used to display text in the Shiny app

	3b. conclusion2.Rmd - Another Rmarkdown file with the same function

	3c. introduction.Rmd - Another Rmarkdown file with the same function
       
4. setup.R - An R file which is used to centralize package attachment

5. renv.lock - A file with the package library documentation





## Sources:

https://replicationindex.com/category/z-curve/  

https://journals.sagepub.com/doi/full/10.1177/2515245919847196
  
https://github.com/nicebread/meta-showdown  

https://psycnet.apa.org/fulltext/2019-80290-004.html
  
https://osf.io/h2k3t/  
  
The p-curve and z-curve articles:
  
https://psycnet.apa.org/record/2013-25331-001 
  
https://repository.upenn.edu/fnce_papers/62/    
  
https://osf.io/preprints/wr93f/ 
   
https://psyarxiv.com/urgtn/
  
The packages used to analyze the data:

p-curve - https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/dmetar.html

z-curve - https://cran.r-project.org/web/packages/zcurve/zcurve.pdf
