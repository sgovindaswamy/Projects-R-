# Simpson's Paradox

Simpson's paradox : This is a statistical phenomenon when the trend that appears in different subgroups reverses or disappears when the different sub-groups are combined together. Identifying and modeling simpson's paradox plays a crucial role in the clinical trials decision making

The below plot is an illustration of simpson's paradox using a dataset that includes variables such treatment ( A or B ), success ( yes or no ), and kidney stone size ( small or large ). When modeling the relationship using multinomial logistic regression between treatment and success without including the kidney stone size, we can observe that the predicted probabilities for treatment B is higher than treatment A. While considering the stone size as confounder variable, treatment A is found to be significantly better than treatment B ( presence of simpson's paradox ) 

## Interpretation of results

Treatment B:

In Model 1, treatmentB has a positive coefficient (0.2899) but a non-significant p-value (0.129), indicating no clear relationship between treatment and success without considering stone_size. In Model 2, after adding stone_size, treatmentB becomes negative (-0.3572), but it still has a non-significant p-value (0.119), suggesting no strong evidence of treatment's effect once stone size is considered.

Stone Size:

Model 2 includes stone_size, and it is highly significant (p-value = 1.33e-07), indicating that stone_size has a strong effect on the probability of success. Specifically, the small stone size significantly increases the odds of success (coefficient = 1.2606).

Model Fit:

The residual deviance in Model 2 (662.87) is lower than in Model 1 (692.67), suggesting that including stone_size improves the model fit. The AIC for Model 2 (668.87) is lower than for Model 1 (696.67), which further suggests that adding stone_size improves the model's ability to predict the outcome.
