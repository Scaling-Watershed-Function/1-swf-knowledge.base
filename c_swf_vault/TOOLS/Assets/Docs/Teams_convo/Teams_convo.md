# Teams_convo 
[[Francisco J. Guerrero]]: Hi [[Kyongho Son]] I'd like to know about model outputs in the Yakima basin that could be used for data visualization. I'm curious about spatial and temporal resolution of the predictions, and the time span covered by the model (months? years?). Today is a bit busy for me until 3:30 pm. Please feel free to ping me after that time. Thanks a lot in advance.

[[Kyongho Son]]: The current river corridor model is based on the long-term annual averaged inputs (three substrate concentrations (DOC, NO3, and DO) and exchange flux/residence time). even though model simulates at the hourly time, we consider the model outputs are long-term averaged estimates. Spatial resolution is nhdplus stream reach scale. please let me know if you have any further question. we can chat when you are free. I will be available today's afternoon.

[[Kyongho Son]]: [[James Stegen]],  here are two figures exploring the relationship between total drainage (km2) and modeled respiration (aerobic+anaerobic respiration) with the channel size, and dominant landuse. [[Francisco J. Guerrero]],  I haven't read the paper yet, but if they assume that the nutrinent inputs are homogenous, then it may be linear. as you see the figures, we can only find the linear relationship by subsetting the modelled result with the dominant landuse.

Landuse

![[scaling_land_use.png]]

Stream size
![[scaling_stream_size.png]]

[[Francisco J. Guerrero]]: Question: these are local rates, not cumulative right?

[[Kyongho Son]]: It is a little tricky. Each stream/reach computes the respiration without interacting with other reaches. 

[[Francisco J. Guerrero]]: Hi [[Stephanie Fulton]] and [[Peishi Jiang]] Welcome to this conversation. I just had a meeting with [[Kyongho Son]] to discuss how to play around with data available from model outputs. Although we still need to sort out some logistics, generating plots with similar axes to those in the paper, at least for respiration rates, seems quite feasible (Yeah!). Could you both share what you feel more excited about this topic? And what would you like to share in this space? Thanks a lot in advance!