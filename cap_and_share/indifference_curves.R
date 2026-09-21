# Figures of Exercise 1 (single-country indifference curves), from the CSVs
# written by src/indifference_curves.jl into cap_and_share/output/indifference/.
#   Rscript cap_and_share/indifference_curves.R
# Writes, into cap_and_share/paper/figures/:
#   heatmap_<country>.pdf         welfare gain of the uniform regime, log rho axis
#                                 (the paper's Figure 1; same layout as the
#                                 figures it replaces, with pi = 0 and 0.25 added)
#   heatmap_linear_<country>.pdf  same on a linear rho axis (not used in the paper)
#   indifference_curves.pdf       all simulated curves with the first-order prediction
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

# The cells shown are those of the figures this replaces: pi in steps of 0.25,
# rho on the log-spaced ladder -- plus pi = 0. The runs cover more rho values;
# they serve the indifference curve, not the cells.
PI_SHOW  <- seq(0, 4.75, by = 0.25)
RHO_SHOW <- c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10)
EX <- edges(PI_SHOW)                               # identical for every country
# the rho ladder is drawn on a log axis, so its cell boundaries are the
# midpoints in log space: taking them in levels would stretch the bottom cell
# from 0.02 down to 0.005 and make the axis look as if it started at zero
EY     <- 10^edges(log10(RHO_SHOW))
EY_LIN <- edges(RHO_SHOW)

read_pair <- function(cc) list(
  u = read.csv(file.path(dir_in, paste0("uniform_", cc, ".csv"))),
  a = read.csv(file.path(dir_in, paste0("autarky_", cc, ".csv"))))

grid_for <- function(cc, log_y = TRUE) {
  d <- read_pair(cc)
  u <- d$u[d$u$rho %in% RHO_SHOW, ]; a <- d$a[d$a$pi %in% PI_SHOW, ]
  u <- u[order(u$rho), ]; a <- a[order(a$pi), ]
  g <- expand.grid(i = seq_len(nrow(u)), j = seq_len(nrow(a)))
  g$rho <- u$rho[g$i]; g$pi <- a$pi[g$j]
  g$gain <- (u[[metric]][g$i] - a[[metric]][g$j]) / abs(a[[metric]][g$j]) * 100
  # viability flags, as in the figures this replaces
  g$row_price_neg  <- as.logical(a$row_price_negative[g$j])
  g$row_rights_neg <- as.logical(u$row_rights_negative[g$i])
  ey <- if (log_y) EY else EY_LIN
  g$xmin <- EX[g$j]; g$xmax <- EX[g$j + 1]; g$ymin <- ey[g$i]; g$ymax <- ey[g$i + 1]
  g
}

# The indifference curve, traced as pi*(rho): for each rho on a dense ladder,
# the autarky price at which the country is indifferent. Tracing it this way
# (rather than as rho*(pi)) gives a point for every y value of the panel, as the
# contour of the figures this replaces did, and it does not break where the
# equivalent allocation leaves the plotted range.
curve_for <- function(cc) {
  d <- read_pair(cc)
  u <- d$u[order(d$u$rho), ]; a <- d$a[order(d$a$pi), ]
  rho_dense <- exp(seq(log(min(RHO_SHOW)), log(max(RHO_SHOW)), length.out = 400))
  wu <- approx(u$rho, u[[metric]], xout = rho_dense, rule = 1)$y   # welfare under the uniform price
  # autarky welfare falls with pi, so invert it on the dense welfare values
  pi_star <- approx(a[[metric]], a$pi, xout = wu, rule = 1)$y
  data.frame(pi = pi_star, rho = rho_dense)[is.finite(pi_star), ]
}

curves <- read.csv(file.path(dir_in, "indifference_curves.csv"))
curves <- curves[is.finite(curves$rho_welfare), ]

# Colour limits: symmetric, at the 95th percentile of |gain| over all countries,
# the rule used by the figures this replaces (a few cells -- very large
# allocations to small emitters -- reach hundreds of percent).
all_gain <- unlist(lapply(countries, function(cc) grid_for(cc)$gain))
lim <- as.numeric(quantile(abs(all_gain), 0.95, na.rm = TRUE))
message(sprintf("colour limit (95th percentile of |gain|): %.2f", lim))

xbreaks <- seq(0, 4.75, by = 0.5)
ybreaks <- c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10)

plot_heat <- function(cc, log_y) {
  g  <- grid_for(cc, log_y)
  ey <- if (log_y) EY else EY_LIN
  cv <- curve_for(cc)
  r1 <- curves$rho_welfare[curves$country == cc & curves$pi == 1]
  p <- ggplot(g) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = pmax(pmin(gain, lim), -lim)),
              colour = "white", linewidth = 0.15) +
    scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-lim, lim),
                         name = "Welfare in Uniform relative to Autarky\n(% of EDE consumption NPV)") +
    # crosshair at pi = 1, then the indifference curve on top
    annotate("segment", x = 1, xend = 1, y = min(ey), yend = r1,
             colour = "black", linetype = "dotted", linewidth = 0.4) +
    annotate("segment", x = min(EX), xend = 1, y = r1, yend = r1,
             colour = "black", linetype = "dotted", linewidth = 0.4) +
    geom_line(data = cv, aes(pi, rho), linewidth = 0.9, colour = "black") +
    annotate("point", x = 1, y = r1, shape = 18, size = 2.6, colour = "black") +
    annotate("text", x = 1.12, y = r1, hjust = 0, vjust = -0.7, size = 4, colour = "black",
             label = sprintf("rho[1] == %.2f", r1), parse = TRUE) +
    labs(x = expression("Autarky: price factor " * pi[i] * "  (" * p[i] == pi[i] %.% p^"*" * ")"),
         y = expression(atop("Uniform price:", "rights factor " * rho[i] * "  (" * r[i] == rho[i] %.% bar(e) * ")"))) +
    theme_bw(base_size = 14) +
    theme(panel.grid = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.ticks = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.position = "right",
          legend.title = element_text(angle = 90, hjust = 0.5, size = 10),
          legend.text = element_text(size = 9),
          legend.key.height = unit(1.1, "cm"),
          legend.spacing.y = unit(0.1, "cm")) +
    guides(fill = guide_colorbar(title.position = "right"))
  # viability flags, as before: circles where the rest of the world would need a
  # negative price, crosses where it would be left with negative rights
  fx <- g[g$row_price_neg & !g$row_rights_neg, ]
  fr <- g[g$row_rights_neg, ]
  if (nrow(fx)) p <- p + geom_point(data = fx, aes(x = pi, y = rho), shape = 21, size = 1.1,
                                    fill = "white", colour = "black", stroke = 0.3)
  if (nrow(fr)) p <- p + geom_point(data = fr, aes(x = pi, y = rho), shape = 4, size = 1.3,
                                    colour = "black", stroke = 0.4)
  # the key of the figures this replaces, drawn last so the flags do not cross it;
  # the second entry appears only where the flag itself does
  if (log_y) {
    two <- nrow(fr) > 0
    p <- p +
      annotate("rect", xmin = 2.62, xmax = 4.72, ymin = if (two) 3.1 else 5.0, ymax = 13.6,
               fill = "white", colour = "black", linewidth = 0.3) +
      annotate("point", x = 2.82, y = 8.6, shape = 18, size = 2.6, colour = "black") +
      annotate("text", x = 2.98, y = 8.6, hjust = 0, size = 3.4, colour = "black",
               label = "rho[i]~at~pi[i]==1~(rho[1])", parse = TRUE)
    if (two) p <- p +
      annotate("point", x = 2.82, y = 4.6, shape = 4, size = 1.9, colour = "black", stroke = 0.6) +
      annotate("text", x = 2.98, y = 4.6, hjust = 0, size = 3.4, colour = "black",
               label = "RoW~rights < 0", parse = TRUE)
  }
  # identical axes for every country
  if (log_y) {
    p <- p + scale_y_log10(breaks = RHO_SHOW, labels = as.character(RHO_SHOW)) +
      scale_x_continuous(breaks = xbreaks) +
      coord_cartesian(xlim = range(EX), ylim = range(EY), expand = FALSE)
  } else {
    p <- p + scale_x_continuous(breaks = xbreaks) +
      coord_cartesian(xlim = range(EX), ylim = c(0, max(EY_LIN)), expand = FALSE)
  }
  p
}

for (cc in countries) {
  ggsave(file.path(dir_out, paste0("heatmap_", cc, ".pdf")), plot_heat(cc, TRUE),
         width = 6.6, height = 3.5)
  ggsave(file.path(dir_out, paste0("heatmap_linear_", cc, ".pdf")), plot_heat(cc, FALSE),
         width = 6.6, height = 3.5)
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
