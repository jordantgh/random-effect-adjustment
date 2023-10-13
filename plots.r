library(ggplot2)
library(ggprism)
library(ggbeeswarm)

drc_base <- function(data) {

  max_y <- max(data$measurement, na.rm = TRUE)

  ggplot(
    data,
    aes(x = dose, color = genotype, fill = genotype)
  ) +
    geom_line(
      stat = "smooth",
      method = drm,
      method.args = list(fct = LL.4()),
      linewidth = 2, se = FALSE
    ) +
    geom_point(
      color = "black",
      pch = 21,
      size = 3,
      alpha = 0.8,
      stroke = 1.2
    ) +
    scale_color_manual(
      labels = c("Wild-type", "IFITM3 KO"),
      values = c("#5278ec", "#bb0c40")
    ) +
    scale_fill_manual(
      labels = c("Wild-type", "IFITM3 KO"),
      values = c("#5278ec", "#bb0c40")
    ) +
    theme_prism(base_size = 14) +
    scale_y_continuous(limits = c(NA, max_y * 1.1)) +
    scale_x_continuous(
      trans = scales::pseudo_log_trans(0.1),
      labels = scales::label_number(drop0trailing = TRUE),
      breaks = c(0, 0.3, 1, 3, 10, 30, 100)
    ) +
    labs(x = "Pam2Csk4 (µg/ml)") +
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))
}

bar_base <- function(data) {

  max_y <- max(data$measurement, na.rm = TRUE)

  ggplot(data) +
    facet_grid(. ~ genotype, scales = "fixed") +
    stat_summary(
      aes(fill = genotype, alpha = dose),
      fun = mean,
      geom = "bar",
      colour = "black",
      size = 1.3,
      width = 0.8,
      position = position_dodge(width = 0.9)
    ) +
    geom_beeswarm(
      size = 2,
      stroke = 0.9,
      dodge.width = 0.9
    ) +
    scale_shape_manual(values = c(16, 16), guide = "none") +
    scale_alpha_manual(values = c(0.4, 1), guide = "none") +
    scale_fill_manual(
      labels = c("Wild-type", "IFITM3 KO"),
      values = c("#5278ec", "#bb0c40")
    ) +
    theme_prism(base_size = 14) +
    scale_y_continuous(expand = c(0, 0), limits = c(NA, max_y * 1.1)) +
    scale_x_discrete(labels = c("-", "+")) +
    theme(strip.text.x = element_blank()) +
    theme(panel.spacing.x = unit(0, "cm")) +
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))
}