# maRketingscience
The maRketingscience package was built for faster, better building of marketing mix models (MMM). 
It grew out of and replaced the MarSci package (2019, no longer available).

The main goal of maRketingscience is the offer of functions

### Updates in v0.2:
--- the model_stats() function has replaced the depcrated r_squared and now shows the model's Durbin-Watson test statistic as well
--- the mdl() wrapper function can be used instead of lm() for the modelling task. It saves the model output, shows the model object summary and plots actuals vs. predicted
--- the adbankr and adbankr_df functions are available for computing carry-over effects

### look out for these improvements in the future:
--- introduction of "tsibble"s for the entire modelling workflow
--- new tidy modelling approach using "broom" to come
