# maRketingscience
The maRketingscience package was built for faster, better building of marketing mix models (MMM). 
It grew out of and replaced the MarSci package (2019, no longer available).

The main goal of maRketingscience is offering reliable functions to make composing MMMs easier, faster and smoother.

### Updates in v0.2:
- the model_stats() function has replaced the depcrated r_squared and now shows the model's Durbin-Watson test statistic as well 
- the mdl() wrapper function can be used instead of lm() for the modelling task. It saves the model output, shows the model object summary and plots actuals vs. predicted
- the adbanker() function is available for computing carry-over effects on a media data-frame (was fused from the adbankr() and adbankr_df() functions in v0.2.2)
- the holdout() function will compute a holdout test and show the respective R2s

### look out for these improvements in the future:
- introduction of "tsibble"s for the entire modelling workflow
- new tidy modelling approach using "broom" to come
