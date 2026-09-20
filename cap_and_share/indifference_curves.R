# Figures of Exercise 1 (single-country indifference curves), from the CSVs
# written by src/indifference_curves.jl into cap_and_share/output/indifference/.
#   Rscript cap_and_share/indifference_curves.R
# Writes, into cap_and_share/paper/figures/:
#   heatmap_<country>.pdf         welfare gain of the uniform regime, log rho axis
#   heatmap_linear_<country>.pdf  same, linear rho axis
#   indifference_curves.pdf       all simulated curves (linear axes) with the
#                                 first-order prediction rho_hat(pi)
library(ggplot2)

args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
dir_in  <- file.path(here, "output", "indifference")
dir_out <- file.path(here, "paper", "figures")
countries <- c("USA", "RUS", "CHN", "TUR", "EU27", "IND", "NGA", "COD")
labels <- c(USA = "United States", RUS = "Russia", CHN = "China", TUR = "Turkey",
            EU27 = "European Union (EU27)", IND = "India", NGA = "Nigeria", COD = "DR Congo")
metric <- "welfare"                     # NPV of EDE consumption
has <- function(cc) all(file.exists(file.path(dir_in, paste0(c("uniform_", "autarky_"), cc, ".csv"))))
countries <- Filter(has, countries)

edges <- function(x) {                  # tile boundaries for an uneven grid
  mid <- (head(x, -1) + tail(x, -1)) / 2
  c(x[1] - (mid[1] - x[1]), mid, tail(x, 1) + (tail(x, 1) - tail(mid, 1)))
}

grid_for <- function(cc) {
  u <- read.csv(file.path(dir_in, paste0("uniform_", cc, ".csv")))
  a <- read.csv(file.path(dir_in, paste0("autarky_", cc, ".csv")))
  g <- expand.grid(i = seq_len(nrow(u)), j = seq_len(nrow(a)))
  g$rho <- u$rho[g$i]; g$pi <- a$pi[g$j]
  g$gain <- (u[[metric]][g$i] - a[[metric]][g$j]) / abs(a[[metric]][g$j]) * 100
  g$row_neg <- as.logical(a$row_price_negative[g$j])
  ex <- edges(a$pi); ey <- edges(u$rho)
  g$xmin <- ex[g$j]; g$xmax <- ex[g$j + 1]; g$ymin <- ey[g$i]; g$ymax <- ey[g$i + 1]
  g
}

curves <- read.csv(file.path(dir_in, "indifference_curves.csv"))
curves <- curves[is.finite(curves$rho_welfare), ]

# colours clipped at +/- LIM%: a few cells (very large allocations to small
# emitters) reach hundreds of percent and would flatten the whole scale
LIM <- 5
lim <- LIM

plot_heat <- function(cc, log_y) {
  g <- grid_for(cc)
  cv <- curves[curves$country == cc, ]
  if (log_y) {                          # rho = 0 cannot sit on a log axis
    g <- g[g$rho > 0, ]
    g$ymin <- pmax(g$ymin, min(g$rho) / 1.5)
  }
  r1 <- cv$rho_welfare[cv$pi == 1]
  p <- ggplot(g) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = pmax(pmin(gain, lim), -lim)),
              colour = "white", linewidth = 0.15) +
    scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-lim, lim),
                         name = "Welfare gain of\nuniform price\n(% of NPV,\nclipped at 5)") +
    geom_line(data = cv, aes(pi, rho_welfare), linewidth = 0.9) +
    geom_point(data = g[g$row_neg & g$i == 1, ], aes(pi, ymin), shape = 4, size = 1.3) +
    annotate("point", x = 1, y = r1, shape = 18, size = 3) +
    annotate("text", x = 1.08, y = r1, hjust = 0, vjust = -0.6, size = 3,
             label = sprintf("rho[1] == %.2f", r1), parse = TRUE) +
    labs(x = expression("Autarky price factor " * pi[i] * "  (" * p[i] == pi[i] * p^"*" * ")"),
         y = expression("Rights factor " * rho[i]), title = labels[[cc]]) +
    coord_cartesian(expand = FALSE) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(), plot.title = element_text(size = 10, face = "bold"))
  if (log_y) {
    p <- p + scale_y_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10))
  } else {
    # zoom on the curve: on a 0-10 axis the curves of most countries are flat
    top <- max(1.3 * max(cv$rho_welfare[cv$pi <= 0.5], na.rm = TRUE), 0.2)
    p <- p + coord_cartesian(ylim = c(0, min(top, max(g$ymax))), expand = FALSE)
  }
  p
}

for (cc in countries) {
  ggsave(file.path(dir_out, paste0("heatmap_", cc, ".pdf")), plot_heat(cc, TRUE), width = 5.2, height = 3.2)
  ggsave(file.path(dir_out, paste0("heatmap_linear_", cc, ".pdf")), plot_heat(cc, FALSE), width = 5.2, height = 3.2)
}

cv <- curves[curves$country %in% countries, ]
cv$label <- factor(labels[cv$country], levels = labels[countries])
p <- ggplot(cv, aes(pi)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_vline(xintercept = 1, colour = "grey80", linewidth = 0.3, linetype = "dotted") +
  geom_line(aes(y = rho_welfare, colour = "Simulated")) +
  geom_point(aes(y = rho_welfare, colour = "Simulated"), size = 0.9) +
  geom_line(aes(y = rho_hat, colour = "First-order prediction"), linetype = "dashed") +
  facet_wrap(~label, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = c("Simulated" = "black", "First-order prediction" = "#c0392b"), name = NULL) +
  labs(x = expression("Autarky price factor " * pi[i]), y = expression("Equivalent rights factor " * rho[i])) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(dir_out, "indifference_curves.pdf"), p, width = 8, height = 4.6)
cat("figures written to", dir_out, "\n")
