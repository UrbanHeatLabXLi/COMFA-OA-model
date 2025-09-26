**COMFA-OA Thermal Comfort Index for Older Adults**

COMFA-OA is a thermal comfort estimation tool for older adult group. The output index calculate the thermal comfort level as **energy budget** in watt/m2, threshold can be find in following table<sup>1</sup>.

A optional equavalent temperature output was provided representating the air temperature that reach the same energy budget at a constant meteorological condtion<sup>2</sup>. 

**About this repository**
This repository provides the code implementation of the COMFA-OA (Outdoor Thermal Comfort Model for Older Adults), as described in the following paper:
Li, X., Zhang, Y., Sang, H., Lee, C., Sullivan, W. C., Maddock, J., Li, D., & Brown, R. D. (2025). A novel thermal comfort model for older adults–development and validation of the COMFA-OA model. Building and Environment, 113758. https://doi.org/10.1016/j.buildenv.2025.113758

**🔧 Features**
- The COMFA-OA model extends the original COMFA framework to account for the specific thermal comfort and heat exposure of older adults (55+) in urban environments.
- Required microclimate inputs: air temperature (Ta), mean radiant temperature (Tmrt), wind speed, relative humidity
- Required human paramters: height(m), weight(kg),
- Incorporation of energy balance components: radiation, convection, evaporation, conduction

**Calculation**

**🔧Option 1: Calculator through coding file**

- 1_COMFA_OA_Rmarkdown
- 1_COMFA_OA_Python

**[🔧Option 2: Calculator through web app](https://comfa.shinyapps.io/10_shinnyapp/)**


**🔧 Citation**
@article{Li2025_COMFAOA,
  title   = {A novel thermal comfort model for older adults–development and validation of the COMFA-OA model},
  author  = {Li, Xiaoyu and Zhang, Y. and Sang, H. and Lee, C. and Sullivan, W. C. and Maddock, J. and Brown, R. D.},
  journal = {Building and Environment},
  year    = {2025},
  pages   = {113758},
  doi     = {https://doi.org/10.1016/j.buildenv.2025.113758
}

**🔧 Notes**

<sup>1</sup>The COMFA-OA threshold is:
| Predicted thermal sensation | COMFA-OA (W/m²)   |
|-----------------------------|-------------------|
| -3 (Cold)                   | < -192            |
| -2 (Cool)                   | -192 to -121      |
| -1 (Slightly cool)          | -120 to -50       |
| 0 (Neutral)                 | -49 to 22         |
| 1 (Slightly warm)           | 23 to 93          |
| 2 (Warm)                    | 94 to 165         |
| 3 (Hot)                     | > 165             |


<sup>2</sup>The constant indoor condition is defined as:

***a). For meteorological condition***
- Air temperature = Mean Radiant Temperature
- Relative humidity = 50%
- Wind speed = 0.3 m/s (~0.5m/s at 10m)

***b). For human parameters***
- Age (yr)= 65
- Heigh (m)= 75kg
- Weight (kg)= 1.75m
- Sex (male, female) = Male
- Standard metabolism rate (METs) = 2.3MET
