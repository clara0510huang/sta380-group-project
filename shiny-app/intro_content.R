intro_text <- HTML("
    <h4>Dataset Background:</h4> The Boston Housing Dataset, first published in 1978 by Harrison Jr and Rubinfeld, compiles 506 observations of housing-related metrics across neighborhoods in Boston, Massachusetts. The dataset was originally used to study the relationship between housing prices and environmental factors.

    <h4>Key Definition:</h4> OLS (Ordinary Least Squares) refers to the standard method used to estimate the coefficients of simple linear regression (SLR) models – it minimizes the sum of the squared differences between observed and predicted values of the response variable.

    <h4>Key variables:</h4>
    <strong>medv</strong>: Median value of owner-occupied residential homes (measured in thousands of US dollars)<br/>
    <strong>lstat</strong>: Percentage of the population with lower socioeconomic status<br/>
    <strong>rm</strong>: Average number of rooms per residential dwelling unit<br/>
    <strong>crim</strong>: Per capita crime rate by town<br/>
    <strong>zn</strong>: Proportion of residential land zoned for lots larger than 25,000 square feet<br/>
    <strong>indus</strong>: Proportion of non-retail commercial acres per town<br/>
    <strong>chas</strong>: Charles River dummy variable (1 = tract borders the river; 0 = otherwise)<br/>
    <strong>nox</strong>: Nitric oxide concentration (parts per 10 million)<br/>
    <strong>age</strong>: Proportion of owner-occupied housing units built before 1940<br/>
    <strong>dis</strong>: Weighted distances to five major Boston employment centers<br/>
    <strong>rad</strong>: Index of accessibility to radial highways<br/>
    <strong>tax</strong>: Full-value property tax rate per $10,000 of property value<br/>
    <strong>ptratio</strong>: Pupil-to-teacher ratio by town<br/>
    <strong>b</strong>: Calculated as 1000(Bk - 0.63)^2, where Bk is the proportion of Black residents by town</p>

     <strong style='color: #e74c3c;'>Note:</strong> Variables <strong>chas</strong>, <strong>rad</strong>, and <strong>b</strong> have been excluded from the simple linear regression analysis for the following reasons<br/>
    <strong>· chas</strong>: Binary dummy variable (0/1) – simple linear regression is less suitable for categorical predictors with only two levels, as it cannot capture or explain non-linear relationships effectively.<br/>
    <strong>· rad</strong>: Ordinal index variable (representing highway accessibility levels) – its ordinal nature means linear regression may misinterpret the equal spacing between levels that does not exist in practice.<br/>
    <strong>· b</strong>: Transformed composite variable (1000(Bk - 0.63)^2) – the non-linear transformation makes it inappropriate for simple linear regression, which assumes a linear relationship between predictor and response.
")
