# Coldwater alcove habitat selection
2021 coastal cutthroat trout microhabitat selection in two coldwater alcoves on the Willamette River

## Depth interpolation equation
IButton depth calculated using interpolation between bounding sensor temperatures using the slope equation 
$$y=mx+b$$ as $$d? = D1 + (D2-D1)/(X2-X1)*(T1-X1)$$
#' where d? is the depth of the fish that we are trying to determine, D1 and D2 
#' the known depths of the bounding sensors, X2 and X1 are the known temperatures
#' of those sensors, and T1 is the temperature of the fish, x is T1-X1 (pretending that
#' X1 is the y intercept for these two points...)

### Data analysis to do
- [ ] Netpen habitat selection model using clogit
  - [ ]  Prep norwood netpen data
  - [ ]  Per alcove
    - [ ]  Day vs night
    - [ ]  Quarterly
    - [ ]  Hourly 
  - [ ]  Pool ibuttons from both alcoves
    - [ ]  Day vs night
    - [ ]  Quarterly
    - [ ]  Hourly   
 - [ ]  AIC
 - [ ]  Figures
