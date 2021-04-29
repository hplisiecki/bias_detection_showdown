## Welcome to the showdown between two bias detection techniques 
&nbsp;
&nbsp;
&nbsp;


This Shiny application allows direct comparison between two bias detection techniques:  
p-curve and z-curve. Bias detection research enables researchers to recover valuable  
information from botched studies. The application uses two different techniques of data  
simulation to allow for more robust generalizations. The simulated statistical test is  
the regular independent samples t-test.

&nbsp;
&nbsp;

#### The p-curve technique

The p-curve technique of bias detection was developed by Uri Simonsohn, Leif Nelson and Joseph  
Simmons as an attempt to distinguish between truly significant findings and false positives. The main  
assumption of their technique is that p values coming from studies that test a true null effect will  
conform to a uniform distribution. A right skewed distribution of p values should then signify the  
existence of an effect, while a left-skewed points towards the occurrence of questionable research  
practices like p-hacking. 

This technique takes into account only the significant studies and provides tests for whether evidential  
value is present, and whether it is absent alongside estimates of the statistical power for the batch of   
studies.


&nbsp;
&nbsp;

#### The z-curve technique

A technique developed by Brunner & Schimmack, and later refined with the help of Bartoš and František  
tackles the problem of bias in a slightly differently. First, it makes use of all of the available literature,  
not only the significant reports. Then it transforms the p-values into z-scores which allows it to  
incorporate more information about the significant outliers. 

Once the z-scores are plotted, their distribution allows for the inspection of the just insignificant results.  
This makes it possible to spot the effects of selection for significance, something that is not possible with  
the p-curve approach. 

Z-curve uses the finite mixture model to provide the estimated discovery rate (EDR), and the estimated  
replication rate (ERR). The former signifies the mean power before the selection for significance (the  
average power of all of the considered studies), while the latter the mean power after selection (the  
average power of all of the significant studies under consideration). The ERR can also be thought of as  
the probability that a direct replication study with the same sample size produces a significant result. 

The third metric provided by the z-curve technique is the false discovery risk, which is calculated on the  
basis of the EDR with the Soric's formula. 

All of the metrics include confidence intervals. 

&nbsp;
&nbsp;

#### The point of contention


Dr Schimmack has criticized the p-curve technique for providing inflated estimates of the average power  
when the power of the studies being considered is heterogeneous or when the sample sizes vary.  

&nbsp;
&nbsp;

#### The simulation techniques

The main problem with simulating such phenomena as publication bias or p-hacking is that it can be  
done in multiple ways. This means that the inferences made with one type of simulation could not  
generalize to the next. For that reason the application makes use of two separate data simulation  
techniques, that were already used to test the reliability of different bias detection techniques. The  
techniques and parts of the code needed to run them come from the works of Renkewitz and Keiner  
(2019) and Carter et al. (2019). The user can switch between the two using the tabs on the top of the  
page.

&nbsp;
&nbsp;

#### Before you start

This application could not have covered all of the different ways in which research bias can be simulated  
simply because there is so many of them. Propositions for additional data simulation workflows to  
include are greatly welcome.  

While the p-curve and the z-curve techniques are definitely one of the most discussed bias detection  
techniques in some circles, they are not the only ones (for an overview of the other techniques have a  
look at the simulation papers listed in the "Sources" section). This simulation should not be then viewed  
as an attempt to reveal a clear winner of the race for reliable meta-analyses but rather as a simple and fun  
way for comparing two existing methods. 

###### Author:  Hubert Plisiecki
###### You can reach me at hplisiecki@gmail.com


&nbsp;
&nbsp;
&nbsp;
##### Sources

The app and the information provided on this page were heavily supplemented by the article written by  
Dr Schimmack available here:

https://replicationindex.com/category/z-curve/  

The works of the people who came up with the simulation techniques used in this application are listed here:

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
