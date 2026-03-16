# psy8812paper

r code:

title: "prospectus for psy8812" format: html editor: visual
Name: Siran Yang

## Quarto

```{r}
library(DiagrammeR)

grViz("
digraph conceptual_model {

  graph [layout = dot, rankdir = LR]

  node [shape = box, style = filled, fillcolor = LightBlue, fontname = Helvetica]

  ADHD   [label = 'ADHD-related traits\\n(ASRS)']
  INATT  [label = 'Inattention']
  HYPIMP [label = 'Hyperactive-impulsive\\ntraits']
  ORN    [label = 'Ornament-like display\\nbehavior\\n(Conspicuous consumption orientation)']

  AGE    [label = 'Age', fillcolor = LightGray]
  SEX    [label = 'Sex', fillcolor = LightGray]
  SES    [label = 'Socioeconomic\\nstatus', fillcolor = LightGray]

  ADHD -> ORN
  ADHD -> INATT
  ADHD -> HYPIMP

  AGE -> ORN
  SEX -> ORN
  SES -> ORN
}
")
```

```{r}
library(tidyverse)
library(psych)
library(lavaan)

set.seed(2026)

# --------------------------------------------------
# 1. Simulate sample
# --------------------------------------------------
n <- 400

dat <- tibble(
  id = 1:n,
  age = round(rnorm(n, mean = 24, sd = 4)),
  sex = sample(c("female", "male"), n, replace = TRUE),
  ses = rnorm(n, mean = 0, sd = 1)
)

# latent ADHD dimensions
dat <- dat %>%
  mutate(
    inatt_lat = rnorm(n, 0, 1),
    hypimp_lat = 0.35 * inatt_lat + rnorm(n, 0, 1)
  )

# overall ADHD latent tendency
dat <- dat %>%
  mutate(
    adhd_lat = 0.6 * inatt_lat + 0.7 * hypimp_lat + rnorm(n, 0, 0.5)
  )

# ornament-like display latent factor
# assume stronger relation with hyperactive/impulsive traits
dat <- dat %>%
  mutate(
    sex_num = if_else(sex == "male", 1, 0),
    display_lat =
      0.20 * scale(inatt_lat)[, 1] +
      0.45 * scale(hypimp_lat)[, 1] +
      0.20 * scale(ses)[, 1] +
      0.10 * sex_num +
      0.05 * scale(age)[, 1] +
      rnorm(n, 0, 1)
  )

# --------------------------------------------------
# 2. Helper function to make Likert items
# --------------------------------------------------
make_likert <- function(latent, noise_sd = 0.9) {
  raw <- latent + rnorm(length(latent), 0, noise_sd)
  cut(
    raw,
    breaks = c(-Inf, -1.2, -0.4, 0.3, 1.0, Inf),
    labels = 0:4,
    ordered_result = TRUE
  ) %>%
    as.character() %>%
    as.numeric()
}

# --------------------------------------------------
# 3. Simulate ASRS items
# 18 items total:
# 9 inattentive + 9 hyperactive/impulsive
# --------------------------------------------------
for (i in 1:9) {
  dat[[paste0("asrs", i)]] <- make_likert(dat$inatt_lat, noise_sd = 0.85)
}

for (i in 10:18) {
  dat[[paste0("asrs", i)]] <- make_likert(dat$hypimp_lat, noise_sd = 0.85)
}

# ADHD scores
dat <- dat %>%
  mutate(
    asrs_inatt = rowSums(across(asrs1:asrs9)),
    asrs_hypimp = rowSums(across(asrs10:asrs18)),
    asrs_total = asrs_inatt + asrs_hypimp
  )

# --------------------------------------------------
# 4. Simulate conspicuous consumption items
# 11 items
# --------------------------------------------------
for (i in 1:11) {
  dat[[paste0("cco", i)]] <- make_likert(dat$display_lat, noise_sd = 0.80)
}

dat <- dat %>%
  mutate(
    cco_total = rowSums(across(cco1:cco11))
  )

# --------------------------------------------------
# 5. Quick check
# --------------------------------------------------
glimpse(dat)

# --------------------------------------------------
# 6. Descriptive statistics
# --------------------------------------------------
dat %>%
  select(age, ses, asrs_inatt, asrs_hypimp, asrs_total, cco_total) %>%
  psych::describe()

# --------------------------------------------------
# 7. Reliability
# --------------------------------------------------
psych::alpha(dat %>% select(asrs1:asrs18))
psych::alpha(dat %>% select(cco1:cco11))

# --------------------------------------------------
# 8. Correlations
# --------------------------------------------------
dat %>%
  select(asrs_inatt, asrs_hypimp, asrs_total, cco_total, age, ses) %>%
  cor(use = "pairwise.complete.obs")

# --------------------------------------------------
# 9. CFA
# Two-factor ADHD + one-factor CCO
# --------------------------------------------------
model_cfa <- "
Inattention =~ asrs1 + asrs2 + asrs3 + asrs4 + asrs5 + asrs6 + asrs7 + asrs8 + asrs9
HypImp =~ asrs10 + asrs11 + asrs12 + asrs13 + asrs14 + asrs15 + asrs16 + asrs17 + asrs18
CCO =~ cco1 + cco2 + cco3 + cco4 + cco5 + cco6 + cco7 + cco8 + cco9 + cco10 + cco11
"

fit_cfa <- cfa(model_cfa, data = dat, std.lv = TRUE)

summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

# optional factor scores
fscores <- lavPredict(fit_cfa)

dat <- dat %>%
  mutate(
    f_inattention = fscores[, "Inattention"],
    f_hypimp = fscores[, "HypImp"],
    f_cco = fscores[, "CCO"]
  )

# --------------------------------------------------
# 10. Regression using observed scores
# --------------------------------------------------
fit1 <- lm(cco_total ~ asrs_total + age + sex + ses, data = dat)
summary(fit1)

fit2 <- lm(cco_total ~ asrs_inatt + asrs_hypimp + age + sex + ses, data = dat)
summary(fit2)

fit3 <- lm(cco_total ~ asrs_total * sex + age + ses, data = dat)
summary(fit3)

# --------------------------------------------------
# 11. Regression using factor scores
# --------------------------------------------------
fit4 <- lm(f_cco ~ f_inattention + f_hypimp + age + sex + ses, data = dat)
summary(fit4)

# --------------------------------------------------
# 12. Simple plots
# --------------------------------------------------
ggplot(dat, aes(x = asrs_total, y = cco_total, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    x = "ASRS Total Score",
    y = "Conspicuous Consumption Orientation",
    title = "Simulated association between ADHD traits and ornament-like display"
  )

ggplot(dat, aes(x = asrs_hypimp, y = cco_total, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    x = "Hyperactive-Impulsive Symptoms",
    y = "Conspicuous Consumption Orientation",
    title = "Simulated association between hyperactive-impulsive traits and display"
  )

# --------------------------------------------------
# 13. Save simulated dataset if needed
# --------------------------------------------------
write_csv(dat,"simulated_adhd_ornament_data.csv")
```
