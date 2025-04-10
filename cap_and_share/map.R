# Code extrait de https://github.com/bixiou/global_tax_attitudes/blob/main/code_global/.Rprofile

library(utils)
chooseCRANmirror(ind = 1)

# options(download.file.method = "wget"); # For Ubuntu 14.04
package <- function(p, version = NULL, remove = FALSE, github = '') {
  if (remove) {
    detach(paste0("package:", p), unload = T)
    remove.packages(p)
  }
  if (!is.element(p, installed.packages()[,1])) {
    if (missing(version)) {
      if (github != '') {
        package("devtools")
        install_github(paste0(github, '/', p))
      } else install.packages(p) # , repos='https://cran.rstudio.com/', type = 'source' may help in case of bug
    } else {
      try({detach("package:devtools", unload = T)})
      package("remotes")
      install_version(p, version = version, repos = "http://cran.us.r-project.org", upgrade = "never", dependencies = TRUE)
      package("devtools")
    }
  }
  else { if(!missing(version)) warning(paste("'", p, "' is already installed with a (potentially) newer version. You may want to install the required version (", version, ") to avoid bugs.", sep=""))}
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package("ggalt") # maps
package("sf") # merge boundaries in maps
package("ggpattern") # stripes in maps
package("ggplot2") # maps

agg_thresholds <- function(vec, thresholds, labels = NULL, sep = " - ", begin = "", end = "", shift = 0, strict_ineq_lower = T, return = "vec", RTL = FALSE # min = 0, max = Inf,
) {
  # strict_ineq_lower == T means intervals 50,60 are of type ];] while == F means [;[.
  # shift = 1 (with strict_ineq_lower == T) means levels ]50;60] will be displayed as "[begin]51[sep]60[end]".
  # thresholds <- c(min, thresholds, max)
  min <- thresholds[1]
  max <- thresholds[length(thresholds)]
  shift_left <- ifelse(strict_ineq_lower, shift, 0)
  shift_right <- ifelse(strict_ineq_lower, 0, shift)
  vec_agg <- rep(NA, length(vec))
  values <- c()
  if (missing(labels)) levels <- c()
  else levels <- labels
  for (i in 2:length(thresholds)) {
    values <- c(values, (thresholds[i] + thresholds[i-1])/2)
    min_i <- ifelse(i > 2, thresholds[i-2], min)
    max_i <- ifelse(i <= length(thresholds) - 2, thresholds[i+2], max)
    next_i <- ifelse(i <= length(thresholds) - 1, thresholds[i+1], max)
    if (missing(labels)) {
      level_i <- c(begin, thresholds[i-1] + (shift_left*(i < length(thresholds)) + shift_right*(thresholds[i-1] == min_i))*(i > 2), sep,
        thresholds[i] - (shift_right*(i > 2) + shift_left*(thresholds[i] == next_i))*(i < length(thresholds)), end)
      if (RTL) level_i <- rev(level_i)
      levels <- c(levels, if (thresholds[i-1]==thresholds[i]) paste0(begin, thresholds[i], end) else paste0(level_i, collapse = ""))
    }
    if (strict_ineq_lower) vec_agg[(vec <= thresholds[i] & vec < max_i & vec > thresholds[i-1]) | (vec == max_i & i == length(thresholds)) | (vec == thresholds[i] & i < length(thresholds) & vec < max_i) | (i == 2 & vec == min)] <- (thresholds[i] + thresholds[i-1])/2
    else vec_agg[(vec < thresholds[i] & vec >= thresholds[i-1] & vec > min_i) | (vec == min_i & i == 2) | (vec == thresholds[i-1] & i > 2 & vec > min_i) | (i == length(thresholds) & vec == max)] <- (thresholds[i] + thresholds[i-1])/2
  }
  if (!RTL) { 
    if (min == -Inf & strict_ineq_lower) levels[1] <- sub(paste0("-Inf", sep), "≤ ", levels[1])
    if (min == -Inf & !strict_ineq_lower) levels[1] <- sub(paste0("-Inf", sep), "< ", levels[1])
    if (max == Inf & strict_ineq_lower) levels[length(levels)] <- sub(paste0("(.*)", sep, "Inf"), "> \\1", levels[length(levels)]) # sub(" ", "", sep)
    if (max == Inf & !strict_ineq_lower) levels[length(levels)] <- sub(paste0("(.*)", sep, "Inf"), "≥ \\1", levels[length(levels)]) # sub(" ", "", sep)
  } else { # To manage Arabic
    if (min == -Inf & strict_ineq_lower) levels[1] <- sub(paste0(sep, "-Inf"), " ≥", levels[1])
    if (min == -Inf & !strict_ineq_lower) levels[1] <- sub(paste0(sep, "-Inf"), " >", levels[1])
    if (max == Inf & strict_ineq_lower) levels[length(levels)] <- sub(paste0("Inf", sep, "(.*)"), "\\1 <", levels[length(levels)]) # sub(" ", "", sep)
    if (max == Inf & !strict_ineq_lower) levels[length(levels)] <- sub(paste0("Inf", sep, "(.*)"), "\\1 ≤", levels[length(levels)]) # sub(" ", "", sep)
  }
  levels <- gsub("000 ", ",000 ",  gsub("-", "–", levels))
  vec_agg[is.na(vec)] <- NA
  vec_agg <- as.item(vec_agg, labels = structure(values, names = levels), missing.values = c("",NA), annotation=Label(vec))
  if (return == "vec") return(vec_agg)
  else if (return %in% c("levels", "labels")) return(levels)
  else if (return == "values") return(values)
}

plot_world_map <- function(var, condition = "", df = sm, on_control = FALSE, save = T, continuous = FALSE, width = dev.size('px')[1], height = dev.size('px')[2], legend_x = .05, rev_color = FALSE, colors = NULL, folder = '../figures/maps/', base_family = NULL, RTL = FALSE,
                           breaks = NULL, labels = NULL, legend = NULL, limits = NULL, fill_na = FALSE, format = "png", trim = T, na_label = "NA", parties = NULL, filename = NULL, negative_stripes = FALSE, stripe_codes = NULL) {
  # /!\ plot_world_map may sometimes fail (due to processor overload): in that case, either close all other windows or check the pdf/png export (which renders generally fine). When testing the function, remove most countries to speed the rendering (cf. example code below).
  if (!is.null(parties)) {
    if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")
    df[[var]][!df$code %in% parties] <- NA
    if (na_label == "NA") na_label <- "Non Parties" }
  table <- heatmap_table(vars = var, data = df, along = "country_map", conditions = c(condition), on_control = on_control, remove_na = FALSE)
  # df_countries <- c(Country_Names[colnames(table)], "Wallis and Futuna", "Vatican", "Tobago", "Trinidad", "Sint Maarten", "Liechtenstein", "Saint Kitts", "Nevis", "Monaco", "Jersey", "Barbuda", "Antigua", "Saint Barthelemy", "Reunion", "Grenadines", "Virgin Islands", "Turks and Caicos Islands", "Saint Pierre and Miquelon", "Saint Helena", "Ascension Island", "Niue", "Palau", "Pitcairn Islands", "South Sandwich Islands")
  # df <- data.frame(country = df_countries, mean = c(as.vector(table), seq(-1.84, 1.94, 0.2), seq(0.06, 0.86, 0.2))) # For oecd_climate
  df_countries <- df$country_map
  df <- data.frame(country_map = df_countries, code = df$code, mean = as.vector(table))
  
  if (condition != "") {
    if (is.null(breaks)) breaks <- c(-Inf, .2, .35, .5, .65, .8, Inf)
    if (is.null(labels)) labels <- c("0-20%", "20-35%", "35-50%", "50-65%", "65-80%", "80-100%")
    if (is.null(legend)) legend <- paste("Share", condition)
    if (is.null(limits)) limits <- c(0, 1)
  } else {
    if (is.null(breaks)) breaks <- c(-Inf, -1.2, -.8, -.4, 0, .4, .8, 1.2, Inf) # c(-Inf, -1, -.5, -.25, 0, .25, .5, 1, Inf)
    if (is.null(labels)) labels <- c("< -1.2", "-1.2 - -0.8", "-0.8 - -0.4", "-0.4 - 0", "0 - 0.4", "0.4 - 0.8", "0.8 - 1.2", "> 1.2")
    if (is.null(legend)) legend <- "Mean"
    if (is.null(limits)) limits <- c(-2, 2)
  }
  if (continuous) df$mean <- pmax(pmin(df$mean, limits[2]), limits[1])

  world_map <- map_data(map = "world") # ggplot2
  world_map <- world_map[world_map$region != "Antarctica",] #
  world_map <- world_map[!world_map$region %in% c("Antarctica", "American Samoa", "Micronesia", "Guam", "Niue", "Pitcairn Islands", "Cook Islands", "Tonga", "Kiribati", "Marshall Islands", "French Polynesia", "Fiji", "Samoa", "Wallis and Futuna", "Vanuatu"),]
  # world_map$region <- iso.alpha(world_map$region)

  if ("Dem USA" %in% parties) {
    us_states <- map_data(map = "state")
    blue_states <- tolower(c("California", "Illinois", "New York", "New Jersey", "Washington", "Massachusetts", "Oregon", "Connecticut", "Delaware", "Rhode Island", "District of Columbia", "Vermont", "Maryland", "Hawaii"))
    non_blue_states <- setdiff(us_states$region, blue_states) #  and Alaska missing from the map
    us_states$region[us_states$region %in% blue_states] <- "USA" #"Dem USA"
    us_states$region[us_states$region %in% non_blue_states] <- "Non-Dem USA"
    world_map$region[world_map$subregion == "Alaska"] <- "Non-Dem USA"
    world_map <- world_map[world_map$region != "USA",] # | world_map$subregion == "Alaska",]
    world_map <- merge_maps(world_map, us_states)
  }

  df_na <- data.frame(country_map = setdiff(world_map$region, df_countries), mean = if (fill_na) breaks[2] else NA)
  df <- merge(df, df_na, all = T)
  df$group <- cut(df$mean, breaks = breaks, labels = labels)
  
  if (!continuous) {
    if (is.null(colors)) colors <- setNames(c(color(length(breaks)-1, rev_color = rev_color), "#7F7F7F"), c(rev(labels), na_label))
    if (negative_stripes) {
      pattern <- setNames(c(rep("none", ceiling((length(breaks)-1)/2)), rep("stripe", floor((length(breaks)-1)/2)), "none"), c(rev(labels), na_label))
      (plot <- ggplot(df) + geom_map(aes(map_id = country_map, fill = group), map = world_map, show.legend=TRUE) + coord_proj("+proj=robin", xlim = c(-135, 178.5), ylim = c(-56, 84)) +
          geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', size = 0,  fill = NA) +
          expand_limits(x = world_map$long, y = world_map$lat) + theme_void(base_family = base_family) + theme(legend.position = c(legend_x, .29)) +
          scale_fill_manual(name = legend, drop = FALSE, values = colors, labels = c(rev(labels), na_label)) +
          geom_map_pattern(data = df, map = world_map, aes(map_id = country_map, pattern = fct_rev(group)), pattern_fill = "black", fill = NA, show.legend=TRUE,
                           pattern_size = 0.01, pattern_density = 0.05, pattern_angle = 45, pattern_spacing = 0.015) +
          scale_pattern_manual(name = legend, values = pattern, drop = FALSE, labels = c(rev(labels), na_label)) + guides(fill = "none", pattern = guide_legend(override.aes = list(fill = colors))))
    } else if (!is.null(stripe_codes)) { # When certain df$code need to be stripped
      df$pattern <- paste0(df$group, ifelse(df$code %in% stripe_codes, "stripe", ""))
      df$pattern[is.na(df$group)] <- na_label
      colors_pattern <- setNames(c(colors[1:(length(colors)-1)], colors[1:(length(colors)-1)], "#7F7F7F"), c(rev(labels), paste0(rev(labels), "stripe"), na_label))
      colors_pattern <- stripe_pattern <- colors_pattern[names(colors_pattern) %in% df$pattern]
      stripe_pattern <- setNames(ifelse(grepl("stripe", names(stripe_pattern)), "stripe", "none"), names(stripe_pattern))
      plot <- ggplot(df) + geom_map(aes(map_id = country_map, fill = pattern), map = world_map, show.legend=TRUE) + # coord_proj("+proj=robin", xlim = c(-135, 178.5), ylim = c(-56, 84)) +
          geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', size = 0,  fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void(base_family = base_family) + theme(legend.position = c(legend_x + .1*("RUS" %in% stripe_codes), .29 + .03*("RUS" %in% stripe_codes) + 0.42*RTL)) +
          scale_fill_manual(name = legend, drop = FALSE, values = colors_pattern[1:(length(colors_pattern)-1)], labels = function(breaks) {breaks[is.na(breaks)] <- na_label; breaks}) +
          geom_map_pattern(data = df, map = world_map, aes(map_id = country_map, pattern = pattern), 
                           pattern_fill = "#7F7F7F", fill = NA, show.legend = FALSE, pattern_density = 0.5, pattern_angle = 45, pattern_spacing = 0.015, pattern_linetype = 0) +
          scale_pattern_manual(values = stripe_pattern, labels =  c(rev(labels), na_label), breaks = c(rev(labels), na_label), name = legend, drop = FALSE) +
          guides(fill = "none", pattern = guide_legend(label.position = if (RTL) "left", override.aes = list(fill = colors_pattern[!grepl("stripe", names(colors_pattern))])))
      if (RTL) plot <- plot + theme(legend.title = element_text(family = "Arial", hjust = 1), legend.text = element_text(family = "Arial", hjust = 1))
      if (!"RUS" %in% stripe_codes) plot <- plot + coord_proj("+proj=robin", xlim = c(-135, 178.5), ylim = c(-56, 84)) # /!\ Bug for Russia when proj_coord() is present ("There is a MULTIPOLYGON with length greater than 1")
    } else {
      (plot <- ggplot(df) + geom_map(aes(map_id = country_map, fill = fct_rev(group)), map = world_map, show.legend=TRUE) + coord_proj("+proj=robin", xlim = c(-135, 178.5), ylim = c(-56, 84)) +
         geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', size = 0,  fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void(base_family = base_family) + theme(legend.position = c(legend_x, .29)) +
         scale_fill_manual(name = legend, drop = FALSE, values = colors[1:(length(colors)-1)], labels = function(breaks) {breaks[is.na(breaks)] <- na_label; breaks})) #, na.value = "grey50" +proj=eck4 (equal area) +proj=wintri (compromise) +proj=robin (compromise, default) Without ggalt::coord_proj(), the default use is a sort of mercator
    }} else {
      (plot <- ggplot(df) + geom_map(aes(map_id = country_map, fill = mean), map = world_map, show.legend=TRUE) + coord_proj("+proj=robin", xlim = c(-135, 178.5), ylim = c(-56, 84)) +
         geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void(base_family = base_family) + theme(legend.position = c(legend_x, .29)) +
         scale_fill_gradientn(name = legend, limits = limits, colours = color(9, rev_color = !rev_color))) # scale_fill_manual(palette = "RdBu", limits = limits, direction = 1, na.value = "grey50")) #scale_fill_viridis_c(option = "plasma", trans = "sqrt"))
    }
  
  print(plot)
  if (save) for (f in format) save_plot(plot, filename = ifelse(!is.null(filename), filename, ifelse(continuous, paste0(var, "_cont"), ifelse(negative_stripes, paste0(var, "_stripes"), var))), folder = folder, width = width, height = height, format = f, trim = trim)
  # return(plot)
}

# df is a dataframe that contains columns country_map (country name), code (ISO3 country name) and variable_name (to be plotted)
plot_world_map(variable_name, df = df, breaks = c(-Inf, -.02, -.005, -1e-10, 0, .005, .02, .05, Inf), format = c('png', 'pdf'), legend_x = .073, trim = T, folder = "../figures/",
               labels = sub("≤", "<", agg_thresholds(c(0), c(-Inf, -.02, -.005, 0, 0, .005, .02, .05, Inf)*100, sep = " to ", return = "levels")), colors = color(11)[2:10],
               legend = paste0("Legend\nover several lines"), 
               save = T)

# df$country_map: Afghanistan Angola Albania United Arab Emirates Argentina Armenia Australia Austria Azerbaijan Burundi Belgium Benin Burkina Faso Bangladesh Bulgaria Bahrain Bahamas Bosnia and Herzegovina Belarus Belize Bolivia Brazil Barbados Brunei Bhutan Botswana Central African Republic Canada Switzerland Chile China Ivory Coast Cameroon Democratic Republic of the Congo Republic of Congo Colombia Comoros Cape Verde Costa Rica Cuba Cyprus Czech Republic Germany Djibouti Denmark Dominican Republic Algeria Ecuador Egypt Eritrea Spain Estonia Ethiopia Finland Fiji France Gabon UK Georgia Ghana Guinea Gambia Guinea-Bissau Equatorial Guinea Greece Guatemala Guyana Hong Kong Honduras Croatia Haiti Hungary Indonesia India Ireland Iran Iraq Iceland Israel Italy Jamaica Jordan Japan Kazakhstan Kenya Kyrgyzstan Cambodia South Korea Kuwait Laos Lebanon Liberia Libya Sri Lanka Lesotho Lithuania Luxembourg Latvia Morocco Moldova Madagascar Maldives Mexico North Macedonia Mali Malta Myanmar Montenegro Mongolia Mozambique Mauritania Mauritius Malawi Malaysia Namibia Niger Nigeria Nicaragua Netherlands Norway Nepal New Zealand Oman Pakistan Panama Peru Philippines Papua New Guinea Poland North Korea Portugal Paraguay Qatar Romania Russia Rwanda Saudi Arabia Sudan Senegal Singapore Solomon Islands Sierra Leone El Salvador Somalia Serbia South Sudan Suriname Slovakia Slovenia Sweden Swaziland Syria Chad Togo Thailand Tajikistan Turkmenistan Timor Trinidad and Tobago Tunisia Turkey Taiwan Tanzania Uganda Ukraine Uruguay USA Uzbekistan Venezuela Vietnam Vanuatu Samoa Yemen South Africa Zambia Zimbabwe

# regions_union <- c("AFR", "CHI", "IND", "CSA", "MEX", "ODA", "EEU", "WEU", "JPN", "SKO") 
# df$code: AFG AGO ALB ARE ARG ARM AUS AUT AZE BDI BEL BEN BFA BGD BGR BHR BHS BIH BLR BLZ BOL BRA BRB BRN BTN BWA CAF CAN CHE CHL CHN CIV CMR COD COG COL COM CPV CRI CUB CYP CZE DEU DJI DNK DOM DZA ECU EGY ERI ESP EST ETH FIN FJI FRA GAB GBR GEO GHA GIN GMB GNB GNQ GRC GTM GUY HKG HND HRV HTI HUN IDN IND IRL IRN IRQ ISL ISR ITA JAM JOR JPN KAZ KEN KGZ KHM KOR KWT LAO LBN LBR LBY LKA LSO LTU LUX LVA MAR MDA MDG MDV MEX MKD MLI MLT MMR MNE MNG MOZ MRT MUS MWI MYS NAM NER NGA NIC NLD NOR NPL NZL OMN PAK PAN PER PHL PNG POL PRK PRT PRY QAT ROU RUS RWA SAU SDN SEN SGP SLB SLE SLV SOM SRB SSD SUR SVK SVN SWE SWZ SYR TCD TGO THA TJK TKM TLS TTO TUN TUR TWN TZA UGA UKR URY USA UZB VEN VNM VUT WSM YEM ZAF ZMB ZWE
# df$region_tiam <- c("ODA", "AFR", "EEU", "MEA", "CSA", "FSU", "AUS", "WEU", "FSU", "AFR", "WEU", "AFR", "AFR", "ODA", "EEU", "MEA", "CSA", "EEU", "FSU", "CSA", "CSA", "CSA", "CSA", "MEA", "ODA", "AFR", "AFR", "CAN", "WEU", "CSA", 
#                    "CHI", "AFR", "AFR", "AFR", "AFR", "CSA", "AFR", "AFR", "CSA", "CSA", "EEU", "EEU", "WEU", "AFR", "WEU", "CSA", "AFR", "CSA", "AFR", "AFR", "WEU", "EEU", "AFR", "WEU", "AUS", "WEU", "AFR", "WEU", "FSU", "AFR", 
#                    "AFR", "AFR", "AFR", "AFR", "WEU", "CSA", "CSA", "AUS", "CSA", "EEU", "CSA", "WEU", "ODA", "IND", "WEU", "MEA", "MEA", "WEU", "MEA", "WEU", "CSA", "MEA", "JPN", "FSU", "AFR", "FSU", "ODA", "SKO", "MEA", "ODA", 
#                    "MEA", "AFR", "AFR", "ODA", "AFR", "WEU", "WEU", "WEU", "AFR", "WEU", "AFR", "ODA", "MEX", "AUS", "AFR", "WEU", "ODA", "AUS", "ODA", "AFR", "AFR", "AFR", "AFR", "ODA", "AFR", "AFR", "AFR", "CSA", "WEU", "WEU", 
#                    "ODA", "AUS", "MEA", "ODA", "CSA", "CSA", "ODA", "ODA", "EEU", "WEU", "CSA", "MEA", "EEU", "FSU", "AFR", "MEA", "AFR", "AFR", "ODA", "AUS", "AFR", "CSA", "AFR", "EEU", "AFR", "CSA", "EEU", "EEU", "WEU", "AFR", 
#                    "MEA", "AFR", "AFR", "ODA", "FSU", "FSU", "ODA", "CSA", "AFR", "MEA", "CHI", "AFR", "AFR", "FSU", "CSA", "USA", "FSU", "CSA", "ODA", "AUS", "AUS", "MEA", "AFR", "AFR", "AFR", "ODA")
