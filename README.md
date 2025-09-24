**COMFA-OA Thermal Comfort Index for Older Adults**

COMFA-OA is a thermal comfort estimation tool for older adult group. The output index calculate the thermal comfort level as **energy budget** in watt/m2, threshold can be find in following table<sup>1</sup>.

A optional equavalent temperature output was provided representating the air temperature that reach the same energy budget at a constant meteorological condtion<sup>2</sup>. 


This repository provides the code implementation of the COMFA-OA (Outdoor Thermal Comfort Model for Older Adults) as described in:
[A Novel Thermal Comfort Model for Older Adults–Development and Validation of the COMFA-OA Model]
Xiaoyu Li, [co-authors]
Building & Environment, 2025
DOI: [Xiaoyu Li, Yue Zhang, Huiyan Sang, Chanam Lee, William C Sullivan, Jay E Maddock, Dongying Li, Robert D Brown]

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
If you use this code, please cite both the paper and this repository:
@article{A Novel Thermal Comfort Model for Older Adults–Development and Validation of the COMFA-OA Model,
  title   = {COMFA-OA: A Thermal Comfort Model for Older Adults in Urban Environments},
  author  = {Li, Xiaoyu and [co-authors]},
  journal = {Building & Environment},
  year    = {2025},
  doi     = {DOI link pending}
}

**🔧 Notes**

<sup>1</sup>The COMFA-OA threshold is:

<sup>2</sup>The constant indoor condition is defined as:
a). For meteorological condition
- Air temperature = 
- Relative humidity =
- Mean radiant temperature = 
- Wind speed =
b). For human parameters
- Age (yr)=
- Heigh (m)=
- Weight (kg)=
- Sex (male, female) =
- Standard metabolism rate (METs, MET) =
