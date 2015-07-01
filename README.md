# Options Portfilio Analyser

## Intro
The package for structured products portfolio calculation. The portfolio includes options, base asset and fixed income. The risk profile is option-style.

## Input
For input use:

1. Options with parameters:
* type = (call, put)
* strike = numeric
* expdate = Date
* days = numeric - til expiration, overrides expdate
* r = numeric - market interest rate
* iv = numeric - implied volatility 
* price = option premium (overrides days, rate, iv)
* q = numeric - face value


2. Base asset with parameters:
* strike = numeric - trade price
* expdate = Date
* days = numeric - til expiration, overrides expdate
* r = numeric - market interest rate
* price = current market price (overrides days, rate)
* q = numeric - face value


3. Fixed income:
* expdate = Date - expiration date
* days = numeric - til expiration, overrides expdate
* r = numeric - coupon 
* price = current market price (overrides days, rate)
* q = numeric - face value


## File list

* - chartsGvis.R - Charts for base asset dynamics with target and strikes projected
* - chartsGvis.R - Structured product profile chart (old)
* 
