source("4. matching.R") #importing data and packages

pacman::p_load(sjPlot)

# ANALYSIS 

simple <- lm(cum_p_frac ~type*post_move + month, data = y);summary(simple)

test <- lm(cum_p_frac ~month*type*post_move, data = y);summary(test)

sjPlot::plot_model(test, type = "int")

