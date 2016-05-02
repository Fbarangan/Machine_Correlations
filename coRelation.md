# co-R-relation

                                                                                 


***                          
###  Concept
This application is an attempt to perform and show statistical comparison of two instruments in order to comply from the increase regulations by State and other regulatory bodies on total quality and risk management.
                
### Data loading and manipulations                                  
files added and processed for data minning and correlations     


```r
# loading file...
x <- read.csv("Instrument1.csv")
y <- read.csv("Instrument2.csv")
```
       

                

                                                                                                                                                           

       
                  
                                       
#### Plots and Diagrams

![](coRelation_files/figure-html/plots-1.png)<!-- -->![](coRelation_files/figure-html/plots-2.png)<!-- -->![](coRelation_files/figure-html/plots-3.png)<!-- -->

***
        
### Statistics                       
* Deming Regression

```
##                 EST SE        LCI       UCI
## Intercept 0.3010383 NA -0.1669637 0.4624579
## Slope     0.9092138 NA  0.8972101 0.9579512
```


* Pearson's product-moment correlation                            

```
## 
## 	Pearson's product-moment correlation
## 
## data:  mergeData_$WBCIntrument1 and mergeData_$WBC.Instrument2
## t = 167.23, df = 52, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.9983931 0.9994636
## sample estimates:
##       cor 
## 0.9990716
```
 
### Discussions
        
54 samples were run for both Instrument-X (Independent) and Instrument-Y(Dependent). The result were evaluated using the standard statistical analysis.      

* The following sample failed the recommended [allowable Total error of 15%][1]
        

```
##   sample WBCIntrument1 WBC.Instrument2 actualpercentBias
## 1     36           2.5             2.1         -16.00000
## 2     46           3.9             3.3         -15.38462
```

                            
            
This site is Under Construction....
        
Felix Barangan, MS,RN,MLS(ASCP)
        
[1]: https://www.westgard.com/clia.htm
