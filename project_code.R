data <- read.csv('insurance.csv')
head(data)

#insurance charges vs each independent variable
options(repr.plot.width=16, repr.plot.height=8)
par(mfrow = c(2, 3), mar=c(4,4,2,1), cex=1.1)
plot(as.factor(data$age), data$charges, xlab = 'age', ylab = 'charges', main = 'age vs charges')
plot(as.factor(data$sex), data$charges, xlab = 'sex', ylab = 'charges', main = 'sex vs charges')
plot(data$bmi, data$charges, xlab = 'bmi', ylab = 'charges', main = 'bmi vs charges')
plot(as.factor(data$children), data$charges, xlab = 'children', ylab = 'charges', main = 'children vs charges')
plot(as.factor(data$smoker), data$charges, xlab = 'smoker', ylab = 'charges', main = 'smoker vs charges')
plot(as.factor(data$region), data$charges, xlab = 'region', ylab = 'charges', main = 'region vs charges')

#density plot of charges
ggplot(data, aes(x = charges)) + geom_density(fill = 'grey') + labs(x = 'charges', y = 'density', title = 'density of charges')

#finding the 25th, 50th and 75th quantile for each feature
taus <- c(0.25, 0.5, 0.75)
num_vars <- c('age', 'bmi', 'children')#numerical features
cat_vars <- c('sex', 'smoker', 'region')#categorical features

#scatter plot of charges vs each numerical feature
num_plot <- data %>% pivot_longer(all_of(num_vars), names_to = 'variable', values_to = 'value')

ggplot(num_plot, aes(value, charges)) + 
  geom_point(alpha = 0.25, color = 'grey50', size = 1) +
  geom_quantile(aes(color = factor(after_stat(quantile))),
                quantiles = taus, formula = y ~ x, linewidth = 1) +
  facet_wrap(~ variable, scales = 'free_x') +
  labs(title = 'charges vs numerical features')

#bar chart of charges vs each categorical feature
cat_plot <- data %>%
  mutate(across(all_of(cat_vars), as.character)) %>%
  pivot_longer(all_of(cat_vars), names_to = 'variable', values_to = 'level') %>%
  group_by(variable, level) %>%
  reframe(quantile = paste0(taus * 100, 'th'),
          charges = quantile(charges, taus))

ggplot(cat_plot, aes(level, charges, fill = quantile)) +#bars are colored by quantile
  geom_col(position = 'dodge') + #puts each bar side by side instead of stacked
  facet_wrap(~ variable, scales = 'free_x') + #one panel for each feature and levels (looks neater)
  labs(title = 'charges vs categorical features')

