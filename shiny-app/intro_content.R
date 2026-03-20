intro_text <- HTML('
  <h4 style="color: #2C3E50; margin-bottom: 15px;">Dataset Background</h4>
  <p style="font-size: 16px; line-height: 1.6;">The Boston Housing Dataset, first published in 1978 by Harrison Jr and Rubinfeld, compiles 506 observations of housing-related metrics across neighborhoods in Boston, Massachusetts. The dataset was originally used to study the relationship between housing prices and environmental factors.</p>
  
  <h4 style="color: #2C3E50; margin-top: 25px; margin-bottom: 15px;">Key Definition</h4>
  <p style="font-size: 16px; line-height: 1.6;"><strong>OLS (Ordinary Least Squares)</strong> refers to the standard method used to estimate the coefficients of simple linear regression (SLR) models – it minimizes the sum of the squared differences between observed and predicted values of the response variable.</p>
  
  <h4 style="color: #2C3E50; margin-top: 25px; margin-bottom: 15px;">Key variables</h4>
  <ul style="font-size: 15px; line-height: 1.8; list-style-type: none; padding-left: 0;">
    <li><strong>medv</strong>: Median value of owner-occupied residential homes (measured in thousands of US dollars)</li>
    <li><strong>lstat</strong>: Percentage of the population with lower socioeconomic status</li>
    <li><strong>rm</strong>: Average number of rooms per residential dwelling unit</li>
    <li><strong>crim</strong>: Per capita crime rate by town</li>
    <li><strong>zn</strong>: Proportion of residential land zoned for lots larger than 25,000 square feet</li>
    <li><strong>indus</strong>: Proportion of non-retail commercial acres per town</li>
    <li><strong>chas</strong>: Charles River dummy variable (1 = tract borders the river; 0 = otherwise)</li>
    <li><strong>nox</strong>: Nitric oxide concentration (parts per 10 million)</li>
    <li><strong>age</strong>: Proportion of owner-occupied housing units built before 1940</li>
    <li><strong>dis</strong>: Weighted distances to five major Boston employment centers</li>
    <li><strong>rad</strong>: Index of accessibility to radial highways</li>
    <li><strong>tax</strong>: Full-value property tax rate per $10,000 of property value</li>
    <li><strong>ptratio</strong>: Pupil-to-teacher ratio by town</li>
    <li><strong>b</strong>: Calculated as 1000(Bk - 0.63)^2, where Bk is the proportion of Black residents by town</li>
  </ul>

  <div style="background-color: #f8f9fa; border-left: 5px solid #e74c3c; padding: 20px; margin-top: 30px; border-radius: 4px;">
    <p style="margin-bottom: 10px; font-size: 15px;">
      <strong style="color: #c0392b;">Note:</strong> Variables <strong>chas</strong>, <strong>rad</strong>, and <strong>b</strong> have been excluded from the simple linear regression analysis for the following reasons:
    </p>
    <ul style="font-size: 14.5px; line-height: 1.6; margin-bottom: 0; padding-left: 20px;">
      <li><strong>chas</strong>: Binary dummy variable (0/1) – simple linear regression is less suitable for categorical predictors with only two levels, as it cannot capture or explain non-linear relationships effectively.</li>
      <li><strong>rad</strong>: Ordinal index variable (representing highway accessibility levels) – its ordinal nature means linear regression may misinterpret the equal spacing between levels that does not exist in practice.</li>
      <li><strong>b</strong>: Transformed composite variable (1000(Bk - 0.63)^2) – the non-linear transformation makes it inappropriate for simple linear regression, which assumes a linear relationship between predictor and response.</li>
    </ul>
  </div>
')