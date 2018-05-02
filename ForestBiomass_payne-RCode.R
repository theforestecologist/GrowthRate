
#Load Data:
  dat <- read.csv(url("http://www.theforestecologist.com/wp-content/uploads/2018/04/bfd2.csv"), head = T)
    dat$Plot <- as.factor(dat$Plot)
    dat$Plot <- as.factor(dat$Plot)

#########################
#Description of data (COPIED FROM EMAIL):

    # 37 Plots sampled at irregular (though typically ~5 Years) intervals for 80 years resulting in ~15 samples/Plot
        # 2 PlotTypes:
           # 28 Plots = successional pine forests (w/ known initial stand age)
           # 9 Plots = hardwood forests (10,35:37,123:124 = upland O-H; 43 & 44 = low mesic hardwood)
    # Variables of interest:
        # Biomass: sum of all present woody biomass in given Plot & Year divided by Plot Area --> units: Mg/ha
        # Biomass.lost: sum of biomass from trees marked as dead/missing/broken in a given Plot & Year --> Units: Mg/ha
        # GrowthRate:  ((BiomassT + Biomass.lostT) - BiomassT-1 ) / (YearT - YearT-1) , where T is the time period of interest
            # For the initial sample: BiomassT-1 is set as 0 and (YearT - YearT-1) = Age of plot (but I'm not sure this is valid!!)
            # Units: Mg/ha/Year
        # We can ignore the other environmental (soil + topogrpahy) data for now...

#########################
#Visualize Data by plot:

  #View annual trend for GrowthRate, Biomass.lost and Biomass for each plot

  #Instal gridExtra package:
    if(!require('gridExtra')) install.packages('gridExtra')

  #Functions to plot:
    biomass.ggplotter <- function(variable = c('Year','Age'),BV = Biomass, data = dat, YTitle = 'Biomass (Mg/ha)') {
      gp <- ggplot(data, aes(x = get(variable),y = get(BV),color = as.factor(Plot))) + theme_bw() +
        ylab(YTitle) + xlab('Plot Age') +
        geom_point(aes(group = as.factor(data$Plot))) +
        geom_line(aes(group = as.factor(data$Plot))) +
        scale_colour_manual(values='black') +
        labs(colour = "PSP") 
      if(match.arg(variable) == 'Year') {
        gp <- gp + xlab('Year') +
          geom_vline(aes(xintercept = 1978), linetype = 'dotted') + 
          geom_vline(aes(xintercept = 1996), linetype = 'dashed',color='grey') +
          geom_vline(aes(xintercept = 1954), linetype = 'dashed',color='grey')
      }
      gp
    }

    plot.biom.plotter <- function(z,dat,variable = c('Year','Age'),...) {
      bGain0 <- biomass.ggplotter(variable,BV = 'GrowthRate',data = dat[dat$Plot %in% z,], YTitle = 'Growth rate (Mg/ha/year)')
        bGain <- bGain0 + theme(legend.key.width=unit(0.6,'cm'))
      bLoss0 <- biomass.ggplotter(variable,BV = 'LossRate',data = dat[dat$Plot %in% z,], YTitle = 'Loss rate (Mg/ha/year)')
        bLoss <- bLoss0 + theme(legend.key.width=unit(0.48,'cm'))
      bTotal0 <- biomass.ggplotter(variable,BV = 'Biomass',data = dat[dat$Plot %in% z,])
        bTotal <- bTotal0 + theme(legend.key.width=unit(0.6,'cm'))
      library(gridExtra)
      grid.arrange(
        bGain,
        bLoss,
        bTotal
      )
    }

  library(ggplot2)
  #Print to pdf:
    pdf(file="PlotBiomassTrends.pdf" ,onefile=T)
      for(i in sort(unique(dat$Plot))) { 
        plot.biom.plotter(i,dat) 
      }
    dev.off()

  #Graphics output notes:
    #Vertical dashed lines (from left>right): 
    #Hurricane Hazel (1954), Change in sampling protocol (1978), Hurricane Fran (1996)  

#########################
#Model Fitting

  mod1 <- lm(GrowthRate ~ I(Year - 1933), data = dat)
  mod2 <- lm(GrowthRate ~ I(Year - 1933) + Plot, data = dat)
  mod3 <- lm(GrowthRate ~ I(Year - 1933) * Plot, data = dat) 
    #worse AIC, but was suggested to me to use so that slope could vary by plot...

  #The following models improve the AIC, but I don't think they're valid because I'm adding predictors that were used to calculate the dependent variable...
    mod4 <- lm(GrowthRate ~ I(Year - 1933) * Plot + Biomass, data = dat)   ## plots with higher biomass will result in higher growth
    mod5 <- lm(GrowthRate ~ I(Year - 1933) * Plot + Biomass + Biomass.lost, data = dat)  ## plots with lower biomass.lost have more trees to allow more growth

  AIC(mod1,mod2,mod3,mod4,mod5)

  #Mixed effects modelling:
    if(!require('nlme')) install.packages('nlme')
    library(nlme)

    mmod1 <- lme(GrowthRate ~ 1 + I(Year - 1933), random = ~ 1|Plot, data = dat, method = "ML") 
    mmod2 <- lme(GrowthRate ~ 1 + I(Year - 1933), random = ~ 1+I(Year - 1933)|Plot, data = dat, method = "ML") 
   
    AIC(mmod1,mmod2)
      #Worse than fixed effects models above...


#Temporal Autocorrelation
  #Not sure how to do this given non-uniform sampling schedule....


##############################################################################

#########################
#Three additional thoughts:

  #1. Odum Institute (OI) folks suggested I do not have a lot of data and so am very limited in my analyses. 


  #2. OI folks suggested I shift my question to be plot-focused due to small data. 
      #They suggestI use Multiple Comparisons approach to examine per-plot trends:

    #Install packages:
      invisible(lapply(c('multcomp','lsmeans'), function(x) if(!require(x,character.only=T)) install.packages(x)))
      library(multcomp)
      library(lsmeans) 

    #Run multiple comparisons on mod3 from above:
      fit.mult <- glht(mod3, linfct = lsm( ~ (Year - 1933) | Plot))
        #summary(fit.mult)

    #Not sure if this is something that ecologists do??? (I've never seen this approach...)


  #3. I also want to examine how forest damage (i.e., "Biomass.lost") impacts GrowthRate
    #Ex: what kind of lag exists?
    #Ex: How does magnitude of loss impact growth rate?)
    #Can I incorporate that into my models above or do I address separately?
    