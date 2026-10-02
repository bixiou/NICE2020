# Figures of Exercise 1 (single-country indifference curves), from the CSVs
# written by src/indifference_curves.jl into cap_and_share/output/indifference/.
#   Rscript cap_and_share/indifference_curves.R
# Writes, into cap_and_share/paper/figures/:
#   heatmap_<country>.pdf         consumption gain of the uniform regime, log rho axis
#                                 (the paper's Figure 1; same layout as the
#                                 figures it replaces, with pi = 0 and 0.25 added)
#   heatmap_linear_<country>.pdf  same on a linear rho axis (not used in the paper)
#   indifference_curves.pdf       all simulated curves with the first-order prediction
library(ggplot2)

args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
# NICE_RECYCLING=equal_pc: the runs with an equal per capita dividend inside
# each country (Online Appendix). Their grid lives in output/equal_pc/ and their
# figures carry the "eqpc" tag, leaving the main figures untouched.
recycling <- Sys.getenv("NICE_RECYCLING", "negishi")
if (!recycling %in% c("negishi", "equal_pc")) stop("NICE_RECYCLING must be negishi or equal_pc")
dir_in  <- if (recycling == "equal_pc") file.path(here, "output", "equal_pc", "indifference") else
                                        file.path(here, "output", "indifference")
dir_out <- file.path(here, "paper", "figures")
tag     <- if (recycling == "equal_pc") "eqpc_" else ""
countries <- c("USA", "RUS", "CHN", "TUR", "EU27", "IND", "NGA", "COD")
labels <- c(USA = "United States", RUS = "Russia", CHN = "China", TUR = "Turkey",
            EU27 = "European Union (EU27)", IND = "India", NGA = "Nigeria", COD = "DR Congo")
# The two benchmarks of the paper: NPV of total consumption with c^eta (Negishi)
# recycling (Figure 1), and NPV of EDE consumption (welfare) with an equal per
# capita dividend within countries (its Online Appendix analogue).
metric <- if (recycling == "equal_pc") "welfare" else "cons"
rho_col <- if (metric == "welfare") "rho_welfare" else "rho_cons"
fill_title <- if (metric == "welfare") "Welfare in Uniform relative to Autarky\n(% of EDE consumption NPV)" else
                                       "Consumption in Uniform relative to Autarky\n(% of consumption NPV)"
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
# Top of the rho ladder shown (NICE_RHO_TOP): 5 for Figure 1, 10 for its
# welfare-variant analogue (Figure A1).
RHO_TOP  <- as.numeric(Sys.getenv("NICE_RHO_TOP", if (recycling == "equal_pc") "10" else "5"))
RHO_SHOW <- c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10)
RHO_SHOW <- RHO_SHOW[RHO_SHOW <= RHO_TOP + 1e-9]
# Largest autarky price factor drawn (NICE_PI_MAX): 2 for Figure 1, the whole
# grid (4.75) for its welfare-variant analogue. The colour scale is computed on
# the whole grid either way, so a cell has the same colour whatever the range.
PI_MAX <- as.numeric(Sys.getenv("NICE_PI_MAX", if (recycling == "equal_pc") "4.75" else "2"))
PI_FIG <- PI_SHOW[PI_SHOW <= PI_MAX + 1e-9]
EX <- edges(PI_FIG)                                # identical for every country
# the rho ladder is drawn on a log axis, so its cell boundaries are the
# midpoints in log space: taking them in levels would stretch the bottom cell
# from 0.02 down to 0.005 and make the axis look as if it started at zero
EY     <- 10^edges(log10(RHO_SHOW))
EY_LIN <- edges(RHO_SHOW)
# top of the y-axis of the linear-scale panels (Figure 1a), NICE_RHO_MAX_LIN
RHO_MAX_LIN <- as.numeric(Sys.getenv("NICE_RHO_MAX_LIN", "6"))

read_pair <- function(cc) list(
  u = read.csv(file.path(dir_in, paste0("uniform_", cc, ".csv"))),
  a = read.csv(file.path(dir_in, paste0("autarky_", cc, ".csv"))))

grid_for <- function(cc, log_y = TRUE, pis = PI_FIG) {
  d <- read_pair(cc)
  u <- d$u[d$u$rho %in% RHO_SHOW, ]; a <- d$a[d$a$pi %in% pis, ]
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
  # Inverting autarky welfare in pi requires it to fall with pi. It does for
  # consumption, but not always for welfare with an equal per capita dividend:
  # a higher domestic price then also funds a progressive dividend, and autarky
  # welfare is flat, even slightly non-monotonic, at low pi. The curve is then
  # traced as rho*(pi) instead, from the indifference rho solved at each pi of
  # the grid (interpolated in rho, in which uniform welfare is monotonic), and
  # stopped where it leaves the bottom of the panel.
  if (any(diff(a[[metric]]) >= 0)) {
    cv <- curves[curves$country == cc, c("pi", rho_col)]
    names(cv) <- c("pi", "rho"); cv <- cv[order(cv$pi), ]
    ymin <- min(RHO_SHOW); keep <- cv$rho >= ymin
    k <- which(!keep)[1]
    if (!is.na(k) && k > 1 && keep[k - 1]) {       # where the curve crosses the bottom edge
      x0 <- cv$pi[k - 1] + (cv$pi[k] - cv$pi[k - 1]) * (cv$rho[k - 1] - ymin) / (cv$rho[k - 1] - cv$rho[k])
      cv <- rbind(cv[seq_len(k - 1)[keep[seq_len(k - 1)]], ], data.frame(pi = x0, rho = ymin))
    } else cv <- cv[keep, ]
    return(cv)
  }
  rho_dense <- exp(seq(log(min(RHO_SHOW)), log(max(EY)), length.out = 400))   # up to the panel top
  wu <- approx(u$rho, u[[metric]], xout = rho_dense, rule = 1)$y   # welfare under the uniform price
  # autarky welfare falls with pi, so invert it on the dense welfare values
  pi_star <- approx(a[[metric]], a$pi, xout = wu, rule = 1)$y
  data.frame(pi = pi_star, rho = rho_dense)[is.finite(pi_star), ]
}

curves <- read.csv(file.path(dir_in, "indifference_curves.csv"))
curves <- curves[is.finite(curves[[rho_col]]), ]

# Colour limits. Figure 1 (consumption variant): symmetric, +/- NICE_FILL_LIM
# (5 by default), larger gains taking the end colour. Figure A1 (welfare variant):
# symmetric, at the 95th percentile of |gain| over the whole grid (a few cells,
# very large allocations to small emitters, reach hundreds of percent).
FIG1 <- c("USA", "RUS", "CHN", "EU27", "IND", "NGA", "COD")
if (recycling == "equal_pc") {
  all_gain <- unlist(lapply(countries, function(cc) grid_for(cc, pis = PI_SHOW)$gain))
  lim <- as.numeric(quantile(abs(all_gain), 0.95, na.rm = TRUE))
  lim_lo <- -lim; lim_hi <- lim
} else {
  shown  <- unlist(lapply(intersect(FIG1, countries), function(cc) grid_for(cc)$gain))
  message(sprintf("gains shown in Figure 1: %.2f to %.2f", min(shown, na.rm = TRUE), max(shown, na.rm = TRUE)))
  lim_hi <- as.numeric(Sys.getenv("NICE_FILL_LIM", "5")); lim_lo <- -lim_hi
}
message(sprintf("colour limits: %.2f to %.2f", lim_lo, lim_hi))
RDBU <- rev(c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
              "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"))   # red -> white -> blue
z <- (0 - lim_lo) / (lim_hi - lim_lo)                                  # position of zero
fill_continuous <- scale_fill_gradientn(colours = RDBU, limits = c(lim_lo, lim_hi), name = fill_title,
                                        breaks = if (recycling == "equal_pc") waiver() else seq(lim_lo, lim_hi, length.out = 5),
                                        values = c(seq(0, z, length.out = 6), seq(z, 1, length.out = 6)[-1]))
# Discrete version (Figure 1 only): five classes on each side of zero, mirrored,
# so that the sign of every cell is unambiguous (10-class ColorBrewer RdBu, no
# neutral class); the end classes are open.
BREAKS <- c(-Inf, -2, -1, -0.5, -0.1, 0, 0.1, 0.5, 1, 2, Inf)
RDBU10 <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
            "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
fmt_b  <- function(x) format(x, trim = TRUE, drop0trailing = TRUE)   # plain hyphen: the pdf device cannot encode a Unicode minus
BINLAB <- sprintf("%s to %s", fmt_b(head(BREAKS, -1)), fmt_b(tail(BREAKS, -1)))
BINLAB[1] <- sprintf("below %s", fmt_b(BREAKS[2]))
BINLAB[length(BINLAB)] <- sprintf("above %s", fmt_b(BREAKS[length(BREAKS) - 1]))
bin_of <- function(g) factor(BINLAB[findInterval(g, BREAKS, rightmost.closed = TRUE, all.inside = TRUE)],
                             levels = BINLAB)
fill_discrete <- scale_fill_manual(values = setNames(RDBU10, BINLAB), drop = FALSE, name = fill_title,
                                   breaks = rev(BINLAB))

xbreaks <- seq(0, PI_MAX, by = 0.5)
# x position at a given fraction of the panel width (places the key whatever PI_MAX)
xat <- function(f) min(EX) + f * diff(range(EX))
ybreaks <- c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10)

plot_heat <- function(cc, log_y, discrete = FALSE) {
  g  <- grid_for(cc, log_y)
  g$fillv <- pmax(pmin(g$gain, lim_hi), lim_lo)
  ey <- if (log_y) EY else EY_LIN
  cv <- curve_for(cc)
  r1 <- curves[[rho_col]][curves$country == cc & curves$pi == 1]
  p <- ggplot(g) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = if (discrete) bin_of(gain) else fillv),
              colour = "white", linewidth = 0.15) +
    (if (discrete) fill_discrete else fill_continuous) +
    # crosshair at pi = 1, then the indifference curve on top
    annotate("segment", x = 1, xend = 1, y = min(ey), yend = r1,
             colour = "black", linetype = "dotted", linewidth = 0.4) +
    annotate("segment", x = min(EX), xend = 1, y = r1, yend = r1,
             colour = "black", linetype = "dotted", linewidth = 0.4) +
    geom_line(data = cv, aes(pi, rho), linewidth = 0.9, colour = "black") +
    annotate("point", x = 1, y = r1, shape = 18, size = 2.6, colour = "black") +
    # a translucent white backing keeps the label legible over dark cells
    annotate("label", x = 1.12, y = r1, hjust = 0, vjust = -0.45, size = 4, colour = "black",
             fill = scales::alpha("white", 0.75), label.size = 0, label.padding = unit(0.12, "lines"),
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
    guides(fill = if (discrete) guide_legend(title.position = "right", keyheight = unit(0.42, "cm"))
                  else guide_colorbar(title.position = "right"))
  # viability flags, as before: circles where the rest of the world would need a
  # negative price, crosses where it would be left with negative rights
  fx <- g[g$row_price_neg & !g$row_rights_neg, ]
  fr <- g[g$row_rights_neg, ]
  if (nrow(fx)) p <- p + geom_point(data = fx, aes(x = pi, y = rho), shape = 21, size = 1.1,
                                    fill = "white", colour = "black", stroke = 0.3)
  if (nrow(fr)) p <- p + geom_point(data = fr, aes(x = pi, y = rho), shape = 4, size = 1.3,
                                    colour = "black", stroke = 0.4)
  # the key of the figures this replaces, drawn last so the flags do not cross it;
  # the second entry appears only where the flag itself does. On a narrower x-axis
  # (Figure 1, pi up to 2) it would cover the rho_1 label, so it is left out: the
  # figure note explains the diamond and the crosses.
  if (log_y && PI_MAX >= 4) {
    two <- nrow(fr) > 0
    p <- p +
      annotate("rect", xmin = xat(0.549), xmax = xat(0.969), ymin = if (two) 3.1 else 5.0, ymax = 13.6,
               fill = "white", colour = "black", linewidth = 0.3) +
      annotate("point", x = xat(0.589), y = 8.6, shape = 18, size = 2.6, colour = "black") +
      annotate("text", x = xat(0.621), y = 8.6, hjust = 0, size = 3.4, colour = "black",
               label = "rho[i]~at~pi[i]==1~(rho[1])", parse = TRUE)
    if (two) p <- p +
      annotate("point", x = xat(0.589), y = 4.6, shape = 4, size = 1.9, colour = "black", stroke = 0.6) +
      annotate("text", x = xat(0.621), y = 4.6, hjust = 0, size = 3.4, colour = "black",
               label = "RoW~rights < 0", parse = TRUE)
  }
  # identical axes for every country
  if (log_y) {
    p <- p + scale_y_log10(breaks = RHO_SHOW, labels = as.character(RHO_SHOW)) +
      scale_x_continuous(breaks = xbreaks) +
      coord_cartesian(xlim = range(EX), ylim = range(EY), expand = FALSE)
  } else {
    p <- p + scale_x_continuous(breaks = xbreaks) +
      coord_cartesian(xlim = range(EX), ylim = c(0, RHO_MAX_LIN), expand = FALSE)
  }
  p
}

for (cc in countries) {
  ggsave(file.path(dir_out, paste0("heatmap_", tag, cc, ".pdf")), plot_heat(cc, TRUE),
         width = 6.6, height = 3.5)
  ggsave(file.path(dir_out, paste0("heatmap_linear_", tag, cc, ".pdf")), plot_heat(cc, FALSE),
         width = 6.6, height = 3.5)
  if (recycling != "equal_pc") {     # Figure 1 with discrete colour classes
    ggsave(file.path(dir_out, paste0("heatmap_discrete_", cc, ".pdf")), plot_heat(cc, TRUE, TRUE),
           width = 6.6, height = 3.5)
    ggsave(file.path(dir_out, paste0("heatmap_discrete_linear_", cc, ".pdf")), plot_heat(cc, FALSE, TRUE),
           width = 6.6, height = 3.5)
  }
}

cv <- curves[curves$country %in% countries, ]
cv$label <- factor(labels[cv$country], levels = labels[countries])
p <- ggplot(cv, aes(pi)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_vline(xintercept = 1, colour = "grey80", linewidth = 0.3, linetype = "dotted") +
  geom_line(aes(y = .data[[rho_col]], colour = "Simulated")) +
  geom_point(aes(y = .data[[rho_col]], colour = "Simulated"), size = 0.9) +
  geom_line(aes(y = rho_hat, colour = "First-order prediction"), linetype = "dashed") +
  facet_wrap(~label, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = c("Simulated" = "black", "First-order prediction" = "#c0392b"), name = NULL) +
  labs(x = expression("Autarky price factor " * pi[i]), y = expression("Equivalent rights factor " * rho[i])) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(dir_out, if (tag == "") "indifference_curves.pdf" else "indifference_curves_eqpc.pdf"), p, width = 8, height = 4.6)
cat("figures written to", dir_out, "\n")
