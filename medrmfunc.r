source("check_install_deps.r")
library(nlme)
library(drc)
library(medrc)
library(magrittr)
library(gridExtra)
library(ggpubr)

## FUNCTIONS
## ---------

# input df needs columns 'genotype', 'clone', 'dose', and 'measurement'
# it will add a new column with the adjusted measurements

adjust_drc <- function(input_data) {
  input_data$genotype <- factor(input_data$genotype)
  input_data$clone <- factor(input_data$clone)

  # Fit the 4PL model using 'medrm'
  #
  # b, c, d, e are 4PL parameters
  # b slope; c min; d max; e ED50
  # 'genotype' and 'dose' are the fixed effects, and 'clone' is the random
  # effect for each of the 4PL parameters
  model <- medrm(
    measurement ~ dose,
    curveid = b + c + d + e ~ genotype,
    data = input_data,
    fct = LL.4(),
    random = b + c + d + e ~ 1 | clone,
    weights = varExp(form = ~dose),
    control = lmeControl(opt = "optim", maxIter = 1000)
  )

  # make full predictions w/ fixed + random effects
  full_model_pred <- predict(model) %>% as.numeric()

  # make predictions w/ only fixed effects (i.e., dose and genotype)
  fe_only_pred <- predict(model, level = 0) %>% as.numeric()

  # get clone-specific deviance from expected values
  clone_deviation <- full_model_pred - fe_only_pred

  # subtract clone effect to get values absent clone-specific effects
  input_data$no_clone_effect_pred <- input_data$measurement - clone_deviation

  return(input_data)
}

adjust_binary <- function(input_data) {
  input_data$genotype <- factor(input_data$genotype)
  input_data$clone <- factor(input_data$clone)

  model <- lme(
    measurement ~ genotype * dose,
    random = ~ dose | clone,
    data = input_data,
    control = lmeControl(opt = "optim", maxIter = 1000)
  )

  full_model_pred <- predict(model)
  fe_only_pred <- predict(model, level = 0)
  clone_deviation <- full_model_pred - fe_only_pred
  input_data$no_clone_effect_pred <- input_data$measurement - clone_deviation

  return(input_data)
}


## EXAMPLE USAGE AND PLOTTING
## --------------------------

result_drc <- readRDS("il6_0323-hcwtko.rds")
pred_drc <- adjust_drc(result_drc)

result_single <- result_drc[result_drc$dose %in% c(0, 100), ] # for bar graph
pred_single <- adjust_binary(result_single)
pred_single$dose <- factor(pred_single$dose, levels = c(0, 100))

source("plots.r")

drc_orig <- drc_base(pred_drc) +
  aes(y = measurement) +
  labs(y = "Original Absorbance (OD450)")

drc_adj <- drc_base(pred_drc) +
  aes(y = no_clone_effect_pred) +
  labs(y = "Adjusted Absorbance (OD450)")

bar_orig <- bar_base(pred_single) +
  aes(x = dose, y = measurement) +
  labs(y = "Original Absorbance (OD450)", x = "Pam2Csk4 (100 µg/ml)")

bar_adj <- bar_base(pred_single) +
  aes(x = dose, y = no_clone_effect_pred) +
  labs(y = "Adjusted Absorbance (OD450)", x = "Pam2Csk4 (100 µg/ml)")

# Arrange
grid <- ggarrange(
  plotlist = list(drc_orig, bar_orig, drc_adj, bar_adj),
  nrow = 2,
  ncol = 2
)

ggsave(
  "il6_0323-hcwtko.pdf",
  grid,
  width = 10,
  height = 10,
  dpi = 300,
  device = cairo_pdf
)
