library(tidyverse)
library(ggplot2)
library(GGally)
library(MASS)
bb_stats <- read_csv("data/baseballStats.csv",
                     col_names = c("team",
                                   "league",
                                   "wins",
                                   "era",
                                   "runs_scored",
                                   "hits_allowed",
                                   "walks_allowed",
                                   "saves",
                                   "errors",
                                   "year"),
                     col_types = cols(team = col_factor(levels = NULL),
                                      league = col_factor(levels = NULL),
                                      wins = col_double(),
                                      era = col_double(),
                                      runs_scored = col_double(),
                                      hits_allowed = col_double(),
                                      walks_allowed = col_double(),
                                      saves = col_double(),
                                      errors = col_double(),
                                      year = col_factor(levels = NULL)),
                     skip = 1)

df <- bb_stats %>%
        filter(year == "2011") 
glimpse(droplevels(df))


# scatter plots
library(ggplot2)
library(GGally)
g <- ggpairs(df, lower = list(continuous = "smooth") + 
            wrap(params = c(method = "loess")))




# add a 'fake' column
df$residual <- seq_len(nrow(df))

# calculate all residuals prior to display
residuals <- lapply(df[c(2, 4:9)], function(x) {
        summary(lm(wins ~ x, data = df))$residuals
})

# calculate a consistent y range for all residuals
y_range <- range(unlist(residuals))

# custom function to display continuous data. If the y variable is "Residual", do custom work.
lm_or_resid <- function(data, mapping, ..., line_color = "red", line_size = 1) {
        if (as.character(mapping$y) != "Residual") {
                return(ggally_smooth_lm(data, mapping, ...))
        }
        
        # make residual data to display
        resid_data <- data.frame(
                x = data[[as.character(mapping$x)]],
                y = residuals[[as.character(mapping$x)]]
        )
        
        ggplot(data = data, mapping = mapping) +
                geom_hline(yintercept = 0, color = line_color, size = line_size) +
                ylim(y_range) +
                geom_point(data = resid_data, mapping = aes(x = x, y = y), ...)
        
}

# plot the data
g <- ggduo(df,
        c(2,4:9), c(2,4:9,11),
        types = list(continuous = lm_or_resid))


ggpairs(df)
glimpse(df)
df1 <- df[c(3,4:9)]
g1 <- ggpairs(df1, 
        lower = list(
        continuous = "smooth",
        combo = "facetdensity",
        color = "blue"))
g1
glimpse(z)
summary(lm(wins ~ . , data = df1))

### Methodology and steps to get final regression equation
## Option 1
# Run step-wise analysis
full.model <- lm(wins ~ . , data = df1)
step.model <- stepAIC(full.model, 
                      direction = "both",
                      trace = "false")

## Option 2
# Look at all variables
summary(lm(wins ~ . , data = df1))
# Look at only runs_scored
summary(lm(wins ~ runs_scored, data = df1))
# Look at only era
summary(lm(wins ~ era, data = df1))
# Look at only saves
summary(lm(wins ~ saves, data = df1))
# Look at only runs_scored & era (best)
summary(lm(wins ~ runs_scored + era, data = df1))
# Look at only runs_scored & saves
summary(lm(wins ~ runs_scored + saves, data = df1))
# Look at only era & saves
summary(lm(wins ~ era + saves, data = df1))
# Look at only runs_scored & era & saves
summary(lm(wins ~ runs_scored + era + saves, data = df1))


## Final regression equation output
model <- lm(wins ~ era + 
                  runs_scored +
                  hits_allowed +
                  walks_allowed + 
                  saves, 
           data = df1)
summary(model) 
library(dplyr)
# Residual Analysis
d <- df1
fit <- lm(wins ~ ., data = d)
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values

# Quick look at the actual, predicted, and residual values
d[1:5,c(1,8:9)]
d$team <- df$team
g <- ggplot(d, aes(x = team, y = wins)) +  # Set up canvas with outcome variable on y-axis
        geom_point()  # Plot the actual points
print(g)
g_res <- ggplot(d, aes(x = team, y = wins)) +
         geom_segment(aes(xend = team, yend = predicted)) +
         geom_point() +
         geom_point(aes(y = predicted), shape = 1) +
         theme(axis.text.x = element_text(angle = 45, 
                                          vjust = 1,
                                          hjust = 1))
print(g_res)
d <- df1
fit <- lm(wins ~ era + 
                  runs_scored +
                  hits_allowed +
                  walks_allowed + 
                  saves, 
          data = d) # fit the model
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values
g <- ggplot(d, aes(x = predicted, y = residuals)) +
        geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
        geom_segment(aes(xend = predicted, yend = 0), alpha = .2) +      # draw line from point to line
        geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
        scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
        guides(color = FALSE, size = FALSE) +                             # Size legend removed
        # geom_point(aes(y = residuals), shape = 1) +
        theme_bw()
print(g)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit)

d %>% 
        gather(key = "iv", value = "x", -wins, -predicted, -residuals) %>%  # Get data into shape
        ggplot(aes(x = x, y = wins)) +  # Note use of `x` here and next line
        geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
        geom_point(aes(color = abs(residuals), size = abs(residuals))) +
        scale_color_gradient2(low = "blue", mid = "white", high = "red") +
        guides(color = FALSE) +
        geom_point(aes(y = predicted), shape = 1) +
        facet_grid(~ iv, scales = "free") +  # Split panels here by `iv`
        theme_bw()


d <- df1
fit <- lm(wins ~ ., data = d)
d$predicted <- predict(fit)  
d$residuals <- residuals(fit) 
d$team <- df$team
d[d$team == "Tampa Bay", 10:9]

df12 <- bb_stats %>%
        filter(year == "2012") %>%
        select_("era", "runs_scored", "hits_allowed", "walks_allowed", "saves", "team")
glimpse(droplevels(df))
fit <- lm(wins ~ era + 
                  runs_scored +
                  hits_allowed +
                  walks_allowed + 
                  saves, 
          data = d) # fit the model
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit)

predictions = predict.lm(fit, df12)
df12$predictions <- predictions
df12[28,6:7]

df12 %>% arrange(predictions) %>% select_("team", "predictions") %>% print(n=30)
