# TFG: Bayesian models for climate time series

The main objective of this work is to develop statistical models and fit the series of local daily maximum temperature (T.máx.) in three Spanish observatories. The interest lies not only in characterizing the effect of global warming in a daily scale on the mean temperature but also in reflecting that effect on its variability. The methodology employed is based on the work by Castillo-Mateo et al. https://doi.org/10.1007/s13253-022-00493-3 who suggested using the Bayesian framework, and more specifically hierarchical models, to make inferences about the distribution of T.máx. in Aragón.


Regarding the characteristics that our model must include, we highlight:
* Seasonality to capture the effect of variation in solar radiation depending on the day within year. This fact can affect the mean, the variance and even the serial correlation.
* Serial correlation to explain the dependence of the temperature with respect to the previous day, that is, the persistence. Its effect on the mean and variance will change influenced by seasonality.
* A linear trend to represent climate warming. It will be shown that it is not uniform throughout the year in both the mean and the variance.


Firstly, we start with an exploratory data analysis in order to decide which terms we should include in the model for the mean and the variance. At the beginning we must filter our series because they may contain not available values. After this step we proceed to extract all the amount of knowledge about our subject that we can discuss about them. Linear models will play an important role in the exploratory task.  They will help us to identify the effects of the different potential predictors on the mean and the variance. We will seek a `final' model for the mean which includes all the profitable terms  for posterior inference. Moreover, we will take advantage of the residuals of this model so that we can use its squares to estimate the predictors for the variance.


According to Castillo-Mateo et al., we have decided to set our framework in the  Bayesian paradigm due to the fact that it will allow us a more flexible fitting and inference. We also make use of R packages that provide Bayesian inference such as 'jagsUI' or 'bamlss'. We finally decided to employ the last one because of the great range of functions that it provides and its capability to be user-friendly.


We focus in three Spanish observatories located in Soria, Zaragoza, and Bilbao. The chosen period is from 1951 to 2020. The data series are provided by the European database ECA\&D (https://www.ecad.eu/). We have estimated an average increase in the T.máx. in Soria of 1.57ºC, in Zaragoza of 2.07ºC and in Bilbao of 1.58ºC, the same day of the year but after a period of 60 years.

Nevertheless, there are several differences between this work and the one of Castillo-Mateo et al. They considered a spatio-temporal model including terms which vary because of the location such us the elevation and meanwhile we will restrict us just to time terms. Another difference we can observe is that we will take into consideration all the year long whereas they have focused in the warm days of summer period. Due to this fact, we will identify more variability among our data and we will consider the necessity of introducing interactions among the autoregressive term and seasonality and between the trend and seasonality both to explain better their effect during the year. Another feature to add is that we will care about modeling the heteroscedasticity of T.máx.
 
To summarize, Chapter 1 suggests us an introduction with the aim to aid the reader to understand the baseline and the main goals of the project. It will show the incentives that have motivate us to conduct this study and it will present a starting point of the model equations.

Furthermore Chapter 2 will present us an accurate revision of Bayesian basis that will lead us to the Bayesian approach and in addition it will present us general knowledge about hierarchical models,  time series, autoregressive models and so on, with the incorporation of some easily comprehensible examples to illustrate this concepts. This overview is essential in the sense of setting a solid base that it will encourage us to operate in a Bayesian framework or within an autoregressive model.

In addition, Chapter 3 will establish the procedure to carry out in order to conduct, initially the exploratory analysis and latter the Bayesian inference. This chapter will describe  the proposed methodology in order to ensure we will fulfill our objectives stated beforehand. At the end of this chapter it will be presented a brief description of the R packages chosen to make Bayesian inference.

Then Chapter 4 gathers the main results obtained by applying to our time series the method exposed in the previous chapter following a parallel work with the paper of Castillo-Mateo et al. We thoroughly try to express the conclusions that might arise from the auxiliary plots and tables supported by a closing chapter containing the supplementary material. Needless to say we will make great efforts on showing to such extent is essential to include the interaction effects or not considering a constant variance, therefore gaining in importance figures summarizing their respective posterior distributions.

Finally, in Chapter 5 we discuss about the achieved results in Chapter 4 and we put them into comparison with those of the pioneer works. Besides we would like to suggest forward guidelines to include interactions among model terms and to model the variance. 
