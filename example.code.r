#'analytical workflows: codoing demo
#'oct 11, 2022
#'
#'this script will demonstrate some coding practices by generating some data and 
#'fitting a model to it

rm(list = ls()) #clear your workspace
graphics.off() #close open graphics devices

#'control+shift+r - section label

# Generate data for linear regression -------------------------------------
# define constant at top then create space before calculation

n.obs <- 100                          # number of observations
slope<- 0.1
intercept <- -1
predictor_ll <- 0                    #lower limit
predictor_ul <- 100                  #upper limit
sd_noise <- 1

predictor <- runif(n = n.obs, 
                   min = predictor_ll,
                   max = predictor_ul)

noise <- rnorm(n.obs, 
               mean = 0, 
               sd = sd_noise)

response <- intercept + slope * predictor + noise

plot(predictor, response)

dat <- data.frame(predictor = predictor, # after comma put on different line
                  response = response)


# Fit a linear model ------------------------------------------------------

is_first_time <- TRUE              #boolean variable (binary; 0,1)

if(is_first_time){
  
  fit <- glm(response ~ predictor, data = dat)  #' if this line of code is 'expensive' or takes a long time
  save(fit, file = "my_expensive_data.Rdata")   #' to load then save this glm as its own file, turn boolean variable
                                               #' on or off to switch ifelse statement
}else{
  
  load(my_expensive_Data.Rdata)
  
}



# Add fit to plot ---------------------------------------------------------

plot(predictor, response)    # Ben is not comfortable with ggplot yet so doing old school
#abline(fit, col = 2)         # draw line with certain slope and intercept

npred<- 10
predictor_seq <- seq(predictor_ll, predictor_ul, length.out = npred)
newdat <- data.frame(predictor =  predictor_seq)

pred <-predict(fit, newdata = newdat, se.fit = TRUE)
points(newdat$predictor, pred, col = 2, pch = 19, cex = 2)
lines(newdat$predictor, newdat$pred, col = 2, pch = 19, cex = 2)

# command + s will run all if you have source on save checked
# use spaces between operators and variables to help read code
# order of script should be locally relevant, just like reading...
# can make single script of hard coded variables and then source it into
# new script code

