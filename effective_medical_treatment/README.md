# Simpson's Paradox

Simpson's paradox : This is a statistical phenomenon when the trend that appears in different subgroups reverses or disappears when the different sub-groups are combined together. Identifying and modeling simpson's paradox plays a crucial role in the clinical trials decision making

The below plot is an illustration of simpson's paradox using a dataset that includes variables such treatment ( A or B ), success ( yes or no ), and kidney stone size ( small or large ). When modeling the relationship using multinomial logistic regression between treatment and success without including the kidney stone size, we can observe that the predicted probabilities for treatment B is higher than treatment A. While considering the stone size as confounder variable, treatment A is found to be significantly better than treatment B ( presence of simpson's paradox ) 

## Interpretation of results
