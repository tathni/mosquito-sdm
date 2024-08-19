library(tidyverse)
library(magrittr)
library(cowplot)
library(ggpubr)
library(sf)
library(raster)
library(layer)

sdm_df <- read.csv("./output_revision/updated-mos-sdm-results-jun19/SDM_Data_June122024.csv") %>% 
  rename(species = Species)

# define function to make panels for the different species 
species_panels <- function(plot_fun, 
                           theme_leg = theme(legend.box.margin = margin(120, 0, 0, 0), 
                                             legend.key.size = unit(1.8, "lines"), 
                                             legend.text = element_text(size = 10)), 
                           plot_grid_label_x = 0, 
                           plot_grid_label_y = 1, 
                           plot_grid_rel_heights = c(1, -0.05, 1, -0.05, 1), 
                           ...){
  plot_grid(plot_fun("Aedes.aegypti", ...) + 
              theme(legend.position =  "none") + 
              xlab(""),
            plot_fun("Aedes.albopictus", ...) + 
              theme(legend.position =  "none") + 
              xlab("") + ylab(""),
            get_legend(
              plot_fun("Aedes.aegypti", ...) + 
                theme_leg
            ), 
            NULL, NULL, NULL,
            plot_fun("Anopheles.gambiae", ...) + 
              theme(legend.position =  "none") + 
              xlab(""),
            plot_fun("Anopheles.stephensi", ...) + 
              theme(legend.position =  "none") + 
              xlab("") + ylab(""),
            NULL, 
            NULL, NULL, NULL,
            plot_fun("Culex.pipiens", ...) + 
              theme(legend.position =  "none"), 
            plot_fun("Culex.quinquefasciatus", ...) + 
              theme(legend.position =  "none") + ylab(""), 
            plot_fun("Culex.tarsalis", ...) + 
              theme(legend.position =  "none") + ylab(""), 
            nrow = 5, ncol = 3, 
            rel_heights = plot_grid_rel_heights,
            align = "hv", 
            axis = "rlbt",
            label_x = plot_grid_label_x, 
            label_y = plot_grid_label_y,
            labels = c("A) ", "B)", "", 
                       "", "", "", 
                       "C)", "D)", "", 
                       "", "", "", 
                       "E)", "F)", "G)"))
}

# fig 2: VIP + AUC ----
auc <- list.files("./output_revision/updated-mos-sdm-results-jun19", pattern = "AUC", 
                  full.names = T) %>% 
  purrr::map(~readRDS(.x) %>% 
               mutate(across(everything(),
                            ~set_attributes(.x, NULL)))) %>%
  list_rbind()
  

my_round <- function(x, dig, upper_ineq = F){
  out <- round(x, dig) 
  if(upper_ineq){
    out <- ifelse(x < 1 & out == 1, 
                    paste0("> 0.", 10^dig - 1), 
                    format(out, nsmall = dig))
    } else{out %<>% format(nsmall = dig)}
    return(out)
}
auc_sum <- auc %>% 
  group_by(species) %>% 
  summarise(across(contains("auc"), 
                   list(min =~ min(unclass(.x)), 
                        max =~ max(unclass(.x)), 
                        mid =~ median(unclass(.x))))) %>% 
  mutate(lab = paste0("Train AUC: ", my_round(in_sample_auc_mid, 3, T), 
                      # " (", my_round(in_sample_auc_min, 3, F), 
                      # " - ", my_round(in_sample_auc_max, 3, F), 
                      "\n",
                      "Test AUC: ", my_round(out_sample_auc_mid, 3, T)#,
                      # " (", my_round(out_sample_auc_min, 3, F),
                      # " - ", my_round(out_sample_auc_max, 3, F), ")"
                      ))

vip <- list.files("./output_revision/updated-mos-sdm-results-jun19", pattern = "vimp", 
                  full.names = T) %>% purrr::map(readRDS) %>% list_rbind
# readRDS("./from caroline/mosquito-sdms-vimp.rds")

vip_sum <- vip %>% 
  summarise(med = median(Gain), max = max(Gain), min = min(Gain), 
            .by = c(species, Feature))  %>% 
  mutate(Feature_col = case_match(Feature, 
                                  c("TAM", "PhotoASTM", "PrecipASTM") ~ "temperature mean", 
                                  c("TASD", "PhotoASTSD", "PrecipASTSD") ~ "temperature variation", 
                                  # c("PDQ", "PWQ", "SW") ~ "water availability",
                                  # c("FC", "EVISD", "EVIM") ~ "vegatation",
                                  .default = "other")) %>% 
  mutate(Feature_lab = case_match(gsub("Photo|Precip", "", Feature),
                                  "FC" ~ "forest cover", 
                                  "CD" ~ "cattle density", 
                                  "PDQ" ~ "driest quarter", 
                                  "PWQ" ~ "wettest quarter", 
                                  "EVISD" ~ "vegetation index variation", 
                                  "EVIM" ~ "vegetation index mean",
                                  c("TAM", "ASTM") ~ "temperature mean",
                                  c("TASD", "ASTSD") ~ "temperature variation", 
                                  "SW" ~ "surface water seasonality", 
                                  "HPD" ~ "human density", 
                                  "WS" ~ "wind speed", 
                                  "mean_rh" ~ "relative humidity") %>% 
           {paste0(species, ".", .)}) %>% 
  arrange(species, desc(med)) %>% 
  mutate(Feature_lab = factor(Feature_lab, levels = rev(.$Feature_lab), ordered = T)) 

vip_plot <- function(species_regex, vip_df, auc_df){
  vip_df %>% 
    filter(grepl(species_regex, species)) %>% 
    ggplot(aes(y = Feature_lab, color = Feature_col)) + 
    geom_point(aes(x = med)) + 
    geom_linerange(aes(xmin = min, xmax = max)) + 
    geom_text(data = left_join(auc_df %>% filter(grepl(species_regex, species)), 
                               vip_df %>% 
                                 filter(grepl(species_regex, species)) %>% 
                                 summarise(max = max(max), 
                                           .by = species)),
              aes(label = lab, x = 0.7*max, y = 2.05),
              size = 4.2,
              inherit.aes = FALSE) + 
    theme_classic() + 
    ggtitle(gsub("\\.", " ", species_regex)) +
    scale_y_discrete(labels = function(x){strsplit(x, ".", fixed = T) %>% 
        map_chr(~.x[2])}) + 
    scale_color_manual(name = "", 
                       values = c("black", "red",  "blue")) +  # "purple", "green3",
    xlab("Gain") + ylab("") + 
    theme(plot.margin = unit(c(5.5, 0, 5.5, 0), "pt"),
          axis.text = element_text(size = 11),
          plot.title = element_text(face = "italic"), 
          legend.title = element_blank())
  }

vip_out <- species_panels(function(species_str) vip_plot(species_str, vip_sum, auc_sum), 
                          theme_leg = theme(legend.box.margin = margin(120, 0, 0, 0), 
                                            legend.key.size = unit(1.8, "lines"), 
                                            legend.text = element_text(size = 12)),
                          plot_grid_label_x = 0.27)
ggsave(vip_out, 
       filename = "./figures/figure2.png", 
       width = 9.5*1.5, height = 6*1.5, units = "in")

# fig 3: responses ----
m_t <- readRDS("./mosq_abundance_quantiles.rds") %>% 
  mutate(species = case_match(species, 
                              "AeAegypti" ~ "Aedes_aegypti", 
                              "AeAlbopictus" ~ "Aedes_albopictus", 
                              "AnGambiae" ~ "Anopheles_gambiae", 
                              "AnStephensi" ~ "Anopheles_stephensi", 
                              "CxPipiens" ~ "Culex_pipiens", 
                              "CxQuinquefasciatus" ~ "Culex_quinquefasciatus", 
                              "CxTarsalis" ~ "Culex_tarsalis")) %>% 
  mutate(q50_scale = q50/max(q97p5), 
         q97p5_scale = q97p5/max(q97p5), 
         q2p5_scale = q2p5/max(q97p5), 
         .by = species) 

ct <- readRDS("./thermal_limit_quantiles.rds") %>% 
  mutate(species = case_match(species, 
                              "AeAegypti" ~ "Aedes_aegypti", 
                              "AeAlbopictus" ~ "Aedes_albopictus", 
                              "AnGambiae" ~ "Anopheles_gambiae", 
                              "AnStephensi" ~ "Anopheles_stephensi", 
                              "CxPipiens" ~ "Culex_pipiens", 
                              "CxQuinquefasciatus" ~ "Culex_quinquefasciatus", 
                              "CxTarsalis" ~ "Culex_tarsalis"))


# pdp <- readRDS("./from caroline/mosquito-sdms-univariate-pdps.rds")
pdp <- list.files("./output_revision/updated-mos-sdm-results-jun19", pattern = "univariate_pdp", 
                  full.names = T) %>% 
  purrr::map(readRDS) %>% list_rbind
  
  
mt_pdp_plot <- function(species_regex, 
                        pdp_df, mt_df, occ_df, 
                        density_height = 0.25, 
                        y_offset = 0.23, 
                        mt_color = "blue", 
                        pdp_color = "black", 
                        occ_bg_colors = c( "grey40", "red")){
  pdp_df %>%   
    filter(grepl(species_regex, species)) %>% 
    filter(feature %in% c("TAM", "PhotoASTM", "PrecipASTM")) %>% 
    mutate(yhat_scaled = (yhat - min(yhat))/(max(yhat) - min(yhat)), 
           .by = c(species, feature, iteration)) %>% 
    ggplot(aes(x = value, y = yhat_scaled + y_offset, group = iteration)) + 
    geom_density(data = occ_df %>% 
                   filter(grepl(species_regex, species)) %>% 
                   mutate(Occ1_or_Bg0 = as.factor(Occ1_or_Bg0)), 
                 aes(group = Occ1_or_Bg0, 
                     color = Occ1_or_Bg0, 
                     fill = Occ1_or_Bg0, 
                     x = TAM, 
                     y = density_height*after_stat(density)/max(after_stat(density))), 
                 inherit.aes = FALSE, 
                 color = NA, alpha = 0.6) + 
    geom_ribbon(data = mt_df %>% filter(grepl(species_regex, species)), 
                aes(x = Temp, 
                    ymin = q2p5_scale + y_offset, 
                    ymax = q97p5_scale + y_offset), 
                color = NA, fill = mt_color, alpha = 0.4, 
                inherit.aes = FALSE) + 
    geom_line(data = mt_df %>% filter(grepl(species_regex, species)),
              aes(x = Temp, y = q50_scale + y_offset, 
                  color = "mechanistic mosquito\nabundance"), 
              # color = mt_color,
              inherit.aes = FALSE) + 
    geom_line(aes(color = "statistical Pr(occurrence)"), 
              # color = pdp_color, 
              alpha = 0.4) + 
    scale_y_continuous(labels = function(x){x - y_offset},
                       breaks = function(yrange) {
                         labeling::extended(0, 1, 5) + y_offset}) +
    scale_fill_manual(name = "Temperature distribution", 
                      values = occ_bg_colors, 
                      labels = function(x){
                        case_match(x, 
                                   "0" ~ "background", 
                                   "1" ~ "occurrence")
                      }) + 
    scale_color_manual(name = "Scaled temperature response", 
                       values = c(mt_color, pdp_color)) + 
    ggtitle(gsub("\\.", " ", species_regex)) +
    theme_classic() + xlab("Temperature (°C)") + ylab("scaled Pr(occurrence)") + 
    theme(plot.margin = unit(c(5.5, 0, 5.5, 0), "pt"),
          text = element_text(size = 14),
          legend.title = element_text(size = 18),
          plot.title = element_text(face = "italic"))
}

mt_pdp_out <- species_panels(function(species_str, ...) mt_pdp_plot(species_str, pdp, m_t %>% filter(Temp <= 37.5), sdm_df, ...), 
                             theme_leg = theme(legend.box.margin = margin(120, 0, 0, 0), 
                                               legend.key.size = unit(1.8, "lines"), 
                                               legend.text = element_text(size = 14)), 
                             plot_grid_label_x = 0.02)

ggsave(mt_pdp_out, 
       filename = "./figures/figure3.png", 
       width = 9*1.5, height = 6*1.5, units = "in")

# fig S9: topt + tmin identification -----
topt_tmin <- pdp %>%   
  filter(feature %in% c("TAM", "PhotoASTM", "PrecipASTM")) %>% 
  arrange(species, feature, iteration, value) %>% 
  mutate(deriv = (lead(yhat) - yhat)/(lead(value) - value),
         deriv_2pos = (deriv > 0) & lead(deriv, 1) >= 0,
         tmin = deriv_2pos & cumsum(deriv_2pos) == 1,
         # deriv3 = (lead(yhat, 5) - lag(yhat, 5))/(lead(value, 5) - lag(value, 5)),
         # tmin3 = cumsum(replace_na((deriv3 > 0) == 1, 0)), 
         # tmin_comb = pmin(tmin, tmin3),
         topt = (yhat == max(yhat)) & (lag(deriv) > 0 & deriv < 0), 
         yhat_scale = (yhat - min(yhat))/(max(yhat) - min(yhat)),
         .by = c(species, feature, iteration)) 

topt_tmin %>% 
  pivot_longer(cols = c(yhat, deriv, yhat_scale),
               values_to = "est") %>% 
  mutate(species = gsub("_", " ", species), 
         name = case_match(name, 
                           "deriv" ~ "d Pr(occurrence)/dT", 
                           "yhat_scale" ~ "scaled Pr(occurrence)", 
                           "yhat" ~ "Pr(occurrence)",
                           .default = name) %>% 
           factor(levels = c("Pr(occurrence)", "scaled Pr(occurrence)", "d Pr(occurrence)/dT"), 
                  ordered = T),
         .by = c(species, feature, iteration))%>% 
  filter(name != "Pr(occurrence)") %>% 
  {ggplot(data = ., 
          aes(x = value, y = est, 
              group = iteration)) + 
      geom_line(alpha = 0.4, color = "grey30") + 
      geom_point(data = pivot_longer(., c(tmin, topt),
                                     values_to = "critical_point", 
                                     names_to = "type") %>% 
                   filter(critical_point),
                 aes(color = type),
                 alpha = 0.9, 
                 show.legend = FALSE,
                 size = 1) + 
      geom_label(data = pivot_longer(., c(tmin, topt),
                                    values_to = "critical_point", 
                                    names_to = "type") %>% 
                  filter(name == "d Pr(occurrence)/dT") %>% 
                  summarise(mid = round(median(value[critical_point], na.rm = T), 1),
                            min = round(min(value[critical_point], na.rm = T), 1),
                            max = round(max(value[critical_point], na.rm = T), 1),
                            xmin = min(value),
                            .by = c(species, type, name)) %>% 
                  mutate(lab = paste0(gsub("^t", "T[", type), "]", ": ", mid, "~(", min, " - ", max, ")")), 
                aes(x = xmin, y = 0.32, label = lab, color = type, 
                    vjust = ifelse(type == "tmin", 0, 1)), 
                hjust = 0, parse = T, size = 3, 
                label.size = 0, label.padding = unit(0.1, "lines"),
                inherit.aes = FALSE, show.legend = FALSE) + 
      facet_grid(name ~ species, scales = "free", 
                 switch = "y") + 
      ylab("") + xlab("Temperature (°C)") + 
      scale_color_manual(name = "", values = c("#1295b5", "#ed8b00")) +
      theme_classic() + 
      coord_cartesian(clip = 'off') +
      theme(strip.background = element_blank(), 
            strip.placement = "outside", 
            strip.text.x = element_text(size = 9.7, face = "italic"),
            plot.margin = unit(c(0, 0, 0, -10), "pt"))} %>% 
  ggsave(filename = "./figures/figureS9.png", 
         width = 15*0.75, height = 5*0.75)


# fig 4: topt + tmin comparison ----
topt_tmin %>% 
  summarise(Tmin = first(value[tmin]), 
            Topt = first(value[topt]),
            .by = c(species, feature, iteration)) %>%  
  pivot_longer(starts_with("T")) %>% 
  summarise(sdm_mid = median(value, na.rm = T), 
            sdm_min = min(value, na.rm = T), 
            sdm_max = max(value, na.rm = T),
            .by = c(species, feature, name)) %>% 
  left_join(ct %>% 
              rename(name = CT) %>% 
              rename_with(~ paste0("lab_", .x), starts_with("q"))) %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  mutate(name = gsub("T", "T[", name) %>% paste0(., "]")) %>% 
  {ggplot(data = .,
          aes(color = species, x = lab_q50, y = sdm_mid)) + 
      geom_point() + 
      geom_linerange(aes(xmin = lab_q2p5, xmax = lab_q97p5), show.legend = FALSE) + 
      geom_linerange(aes(ymin = sdm_min, ymax = sdm_max), show.legend = FALSE) + 
      geom_abline(slope = 1, intercept = 0, color = "grey40", linetype = "dashed") + 
      geom_text(data = summarise(.,
                                 # r2 = summary(lm(sdm_mid ~ lab_q50))$r.squared,
                                 r = cor(sdm_mid, lab_q50),
                                 min = min(c(lab_q2p5, sdm_min)), 
                                 max = max(c(lab_q97p5, sdm_max)), 
                                 .by = name) %>%
                  mutate(label = paste0("r == ", round(r, 3))),
                aes(x = min + (max - min)*0.2, 
                    y = max - (max - min)*0.05, 
                    label = label), 
                hjust = "outwards", 
                inherit.aes =  FALSE, parse = T) + 
      facet_wrap(~name, scales = "free", 
                 labeller = label_parsed) + 
      theme_classic() + 
      theme(strip.background = element_blank(), 
            strip.text = element_text(size = 14, face = "bold"), 
            legend.text = element_text(face = "italic")) + 
      scale_color_manual(values = c("#b695bc", "#574571", 
                                             "#7fa074", "#2c4b27", 
                                             "#f2c88f", "#d37750", "#92351e")) + 
                                               guides(color = guide_legend(override.aes = aes(label = ""))) + 
      xlab("mechanistic lab-based estimate") + 
      ylab("statistical SDM-based estimate") + 
      ggh4x::facetted_pos_scales(
        x = list(scale_x_continuous(limits = filter(., name == "T[min]") %>%
                                      {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
                 scale_x_continuous(limits = filter(., name == "T[opt]") %>%
                                      {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))})),
        y = list(scale_y_continuous(limits = filter(., name == "T[min]") %>%
                                      {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
                 scale_y_continuous(limits = filter(., name == "T[opt]") %>%
                                      {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}))
      )} %>%
  ggsave(filename = "./figures/figure4.png",
         width = 8.5*0.8, height = 4*0.8)

# fig S10: tmin based on peak derivative ----
topt_tmin2 <- pdp %>%   
  filter(feature %in% c("TAM", "PhotoASTM", "PrecipASTM")) %>% 
  arrange(species, feature, iteration, value) %>% 
  mutate(deriv = (lead(yhat) - yhat)/(lead(value) - value),
         deriv_2pos = (deriv > 0) & lead(deriv, 1) >= 0,
         tmin = deriv == max(deriv, na.rm = T),
         topt = (yhat == max(yhat)) & (lag(deriv) > 0 & deriv < 0), 
         yhat_scale = (yhat - min(yhat))/(max(yhat) - min(yhat)),
         .by = c(species, feature, iteration))

plot_grid(topt_tmin2 %>% 
            pivot_longer(cols = c(yhat, deriv, yhat_scale),
                         values_to = "est") %>% 
            mutate(species = gsub("_", " ", species), 
                   name = case_match(name, 
                                     "deriv" ~ "d Pr(occurrence)/dT", 
                                     "yhat_scale" ~ "scaled Pr(occurrence)", 
                                     "yhat" ~ "Pr(occurrence)",
                                     .default = name) %>% 
                     factor(levels = c("Pr(occurrence)", "scaled Pr(occurrence)", "d Pr(occurrence)/dT"), 
                            ordered = T),
                   .by = c(species, feature, iteration))%>% 
            filter(name != "Pr(occurrence)") %>% 
            {ggplot(data = ., 
                    aes(x = value, y = est, 
                        group = iteration)) + 
                geom_line(alpha = 0.4, color = "grey30") + 
                geom_point(data = pivot_longer(., c(tmin, topt),
                                               values_to = "critical_point", 
                                               names_to = "type") %>% 
                             filter(critical_point),
                           aes(color = type),
                           alpha = 0.9, 
                           show.legend = FALSE,
                           size = 1) + 
                geom_label(data = pivot_longer(., c(tmin, topt),
                                               values_to = "critical_point", 
                                               names_to = "type") %>% 
                             filter(name == "d Pr(occurrence)/dT") %>% 
                             summarise(mid = round(median(value[critical_point], na.rm = T), 1),
                                       min = round(min(value[critical_point], na.rm = T), 1),
                                       max = round(max(value[critical_point], na.rm = T), 1),
                                       xmin = min(value),
                                       .by = c(species, type, name)) %>% 
                             mutate(lab = paste0(gsub("^t", "T[", type), "]", ": ", mid, "~(", min, " - ", max, ")")), 
                           aes(x = xmin, y = 0.32, label = lab, color = type, 
                               vjust = ifelse(type == "tmin", 0, 1)), 
                           hjust = 0, parse = T, size = 3, 
                           label.size = 0, label.padding = unit(0.1, "lines"),
                           inherit.aes = FALSE, show.legend = FALSE) + 
                facet_grid(name ~ species, scales = "free", 
                           switch = "y") + 
                ylab("") + xlab("Temperature (°C)") + 
                scale_color_manual(name = "", values = c("#1295b5", "#ed8b00")) +
                theme_classic() + 
                coord_cartesian(clip = 'off') +
                theme(strip.background = element_blank(), 
                      strip.placement = "outside", 
                      strip.text.x = element_text(size = 9.7, face = "italic"),
                      plot.margin = unit(c(15.5, 0, 0, -10), "pt"))}, 
          topt_tmin2 %>%
            summarise(Tmin = first(value[tmin]), 
                      Topt = first(value[topt]),
                      .by = c(species, feature, iteration)) %>%  
            pivot_longer(starts_with("T")) %>% 
            summarise(sdm_mid = median(value, na.rm = T), 
                      sdm_min = min(value, na.rm = T), 
                      sdm_max = max(value, na.rm = T),
                      .by = c(species, feature, name)) %>%  
            left_join(ct %>% 
                        rename(name = CT) %>% 
                        rename_with(~ paste0("lab_", .x), starts_with("q"))) %>% 
            mutate(species = gsub("_", " ", species)) %>% 
            mutate(name = gsub("T", "T[", name) %>% paste0(., "]")) %>% 
            {ggplot(data = .,
                    aes(color = species, x = lab_q50, y = sdm_mid)) + 
                geom_point() + 
                geom_linerange(aes(xmin = lab_q2p5, xmax = lab_q97p5), show.legend = FALSE) + 
                geom_linerange(aes(ymin = sdm_min, ymax = sdm_max), show.legend = FALSE) + 
                geom_abline(slope = 1, intercept = 0, color = "grey40", linetype = "dashed") + 
                geom_text(data = summarise(.,
                                           # r2 = summary(lm(sdm_mid ~ lab_q50))$r.squared,
                                           r = cor(sdm_mid, lab_q50),
                                           min = min(c(lab_q2p5, sdm_min)), 
                                           max = max(c(lab_q97p5, sdm_max)), 
                                           .by = name) %>%
                            mutate(label = paste0("r == ", round(r, 3))),
                          aes(x = min + (max - min)*0.2, 
                              y = max - (max - min)*0.05, 
                              label = label), 
                          hjust = "outwards", 
                          inherit.aes =  FALSE, parse = T) + 
                facet_wrap(~name, scales = "free", 
                           labeller = label_parsed) + 
                theme_classic() + 
                theme(strip.background = element_blank(), 
                      strip.text = element_text(size = 14, face = "bold"), 
                      legend.text = element_text(face = "italic"), 
                      plot.margin = unit(c(15.5, 5.5, 5.5, 5.5), "points")) + 
                scale_color_manual(values = c("#b695bc", "#574571", 
                                              "#7fa074", "#2c4b27", 
                                              "#f2c88f", "#d37750", "#92351e")) + 
                guides(color = guide_legend(override.aes = aes(label = ""))) + 
                xlab("mechanistic lab-based estimate") + 
                ylab("statistical SDM-based estimate") + 
                ggh4x::facetted_pos_scales(
                  x = list(scale_x_continuous(limits = filter(., name == "T[min]") %>%
                                                {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
                           scale_x_continuous(limits = filter(., name == "T[opt]") %>%
                                                {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))})),
                  y = list(scale_y_continuous(limits = filter(., name == "T[min]") %>%
                                                {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
                           scale_y_continuous(limits = filter(., name == "T[opt]") %>%
                                                {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}))
                )}, 
          labels = c("a) alternative Tmin identified from greatest change in Pr(occurrence)", 
                     "b) comparison of alternative Tmin to mechanistic estimate"),
          hjust = 0, label_x = 0.01, label_size = 12,
          nrow = 2) %>% 
  ggsave(filename = "./figures/figureS10.png", 
         width = 15*0.75, height = 10*0.80)

# fig S1-S7: species occurrence + bg maps ----

# sf_use_s2(FALSE)
# 
# bg_shapes <- list.files("./Shapefiles", full.names = T) %>%
#   purrr::map_dfr(function(x){
#     readRDS(x) %>%
#       mutate(species = substr(x, 24, 1000) %>%
#                gsub(".RDS", "", fixed = T, .) %>%
#                gsub("([a-z])([A-Z])","\\1 \\2",.)) %>%
#       st_make_valid %>%
#       st_simplify(preserveTopology = T, dTolerance = 0.1) %>%
#       return()
#   })
# 
# 
# sf_use_s2(TRUE)
# 
# saveRDS(bg_shapes, "./simplified_bg_shapes.rds")
bg_shapes <- readRDS("./simplified_bg_shapes.rds")  
coast <- giscoR::gisco_get_coastallines(resolution = 20)

# scale alpha as inverse of # of occ / area 
purrr::map(bg_shapes$species, 
           function(spec){
             print(spec)
             bg <- bg_shapes %>% 
               filter(species == spec)
             bbox_lim = st_bbox(bg)
             bg_occ <- sdm_df %>% 
               filter(species == str_to_sentence(spec)) %>% 
               st_as_sf(coords = c("Centroid_Longitude", "Centroid_Latitude")) %>% 
               st_set_crs(4326)
             #print(bbox_lim)
             #print(nrow(bg_occ))
             sf_use_s2(FALSE)
             spec_alpha = (st_area(bg)/1e6)/nrow(bg_occ)*3e-5
             sf_use_s2(TRUE)
             {ggplot() + 
                 geom_sf(data = coast, 
                         fill = "grey93") + 
                 geom_sf(data = bg, 
                         fill = "#A9B4C9", #"#606B84", #"steelblue4", "#607B8B"
                         color = NA) +  #"steelblue4", 
                 geom_sf(data = bg_occ, 
                         alpha = spec_alpha,
                         aes(color = as.factor(Occ1_or_Bg0), alpha = as.factor(Occ1_or_Bg0)), 
                         shape = 16, size = 0.5) + 
                 scale_color_manual(name = "", 
                                    values = c("grey20", "red"), 
                                    labels = c("background", "occurrence")) + 
                 scale_alpha_manual(name = "", 
                                    values = c(0.05, 0.1), 
                                    labels = c("background", "occurrence")) + 
                 theme_void() + 
                 xlim(bbox_lim[1], bbox_lim[3]) + 
                 ylim(bbox_lim[2], bbox_lim[4]) + 
                 ggtitle(str_to_sentence(spec)) +
                 guides(color = guide_legend(override.aes = list(alpha = 1, 
                                                                 size = 1.5))) + 
                 theme(legend.position = "inside", 
                       legend.position.inside = c(0.15, 0.25), 
                       title = element_text(face = "italic"),
                       legend.title = element_blank(),
                       legend.margin = margin(t = -2, r = 5, b = 0, l = 0, 
                                              unit = "pt"),
                       legend.background = element_rect(fill = "white"))} %>% 
               ggsave(filename = paste0("./figures/", 
                                        gsub(" ", "_", str_to_sentence(spec)), 
                                        "_bg_occ_map.png"), 
                      width = 5, height = 3)
           })
# fig S8: ROC ----
# roc <- readRDS("./from caroline/mosquito-sdms-roc.rds")
roc <- list.files("./output_revision/updated-mos-sdm-results-jun19", pattern = "ROC", 
                  full.names = T) %>% 
  purrr::map(readRDS) %>% list_rbind


roc_plot <- function(species_regex, roc_df){
  ggplot(roc_df %>% filter(grepl(species_regex, species)), 
         aes(x = 1 - specificities,
             y = sensitivities,
             group = interaction(iteration, set),
             color = set)) + 
    geom_line(alpha = 0.7, linewidth = 0.5) + 
    scale_color_manual(name = "", 
                       values = c("#2b614e", "#96410e"), 
                       labels = c("Testing", "Training")) + 
    theme_classic() + 
    ggtitle(gsub("\\.", " ", species_regex)) +
    theme_classic() + xlab("1 - specificity") + ylab("sensitivity") + 
    theme(plot.margin = unit(c(5.5, 0, 5.5, 0), "pt"),
          plot.title = element_text(face = "italic")) + 
    guides(colour = guide_legend(override.aes = list(linewidth = 1.2,
                                                     alpha = 1)))
}

roc_out <- species_panels(function(species_str) roc_plot(species_str, roc), 
                          theme_leg = theme(legend.box.margin = margin(120, 0, 0, 0), 
                                            legend.key.size = unit(1.8, "lines"), 
                                            legend.text = element_text(size = 10))) 

ggsave(roc_out, 
       filename = "./figures/figureS8.png", 
       width = 7.5, height = 7.5)
# fig 1: conceptual fig ----

# fig 1b: covariates ----
# from https://github.com/marcosci/layer/blob/main/R/tilt_maps.R since package will no longer install without rgeos
tilt_map <- function(data,
                     x_stretch = 2,
                     y_stretch = 1.2,
                     x_tilt = 0,
                     y_tilt = 1,
                     x_shift = 0,
                     y_shift = 0,
                     angle_rotate = pi/20,
                     boundary = NULL,
                     parallel = FALSE) {
  
  if (!any(class(data) %in% c("sf", "sfg"))) {
    data <- stars::st_as_stars(data)
    data <- sf::st_as_sf(data)
  }
  
  shear_matrix <- function(x) {
    matrix(c(x_stretch, y_stretch, x_tilt, y_tilt), 2, 2)
  }
  
  rotate_matrix <- function(x) {
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
  }
  
  if(!is.null(boundary)) data <- create_outline(boundary, data)
  
  if(parallel == TRUE){
    
    geom_func <- function(data, x_stretch, y_stretch, x_tilt, y_tilt, x_shift, y_shift){
      sf::st_geometry(data) <- sf::st_geometry(data) * shear_matrix() * rotate_matrix(angle_rotate) + c(x_shift, y_shift) 
      data <- data %>% sf::st_as_sf()
    }
    
    data <- data %>%
      dplyr::group_by(group = (dplyr::row_number()-1) %/% (dplyr::n()/10))%>%
      tidyr::nest() %>% 
      dplyr::pull(data) %>%
      furrr::future_map(~geom_func(data = .,
                                   x_stretch = x_stretch,
                                   y_stretch = y_stretch,
                                   x_tilt = x_tilt,
                                   y_tilt = y_tilt,
                                   x_shift = x_shift,
                                   y_shift = y_shift)) %>% 
      dplyr::bind_rows() %>% 
      sf::st_as_sf()
    
  } else {
    
    sf::st_geometry(data) <- sf::st_geometry(data) * shear_matrix() * rotate_matrix(angle_rotate) + c(x_shift, y_shift)
    
  }
  
  if(length(names(data)) > 1) names(data)[1] <- "value"
  
  return(data)
  
}
continents <- st_read("./World_Continents")
afr <- continents %>% filter(CONTINENT == "Africa") %>% 
  mutate(geometry = st_geometry(.) %>% st_cast("POLYGON") %>% last)

covar_rasts <- purrr::map(c(temp_mean = "./data/updated_covariates/TAM_Africa.tif", 
                            temp_season = "./data/updated_covariates/TASD_Africa.tif", 
                            water = "./data/covariate_rasters/PDQ.tif", 
                            veg = "./data/covariate_rasters/FC.tif", 
                            humans = "./data/covariate_rasters/HPD.tif"), 
                          function(x){
                            raster(x) %>% 
                              crop(afr) %>% 
                              mask(afr) %>% 
                              return }) 
tilt_rast <- stack(covar_rasts) %>% aggregate(fact = 10)
tilt_rast %<>% tilt_map 
# x_tilt = 0.1, y_tilt = 1) 
tilt_rast %<>% set_colnames(c("temp_mean", "temp_season", "water", "veg", "humans", "geometry"))

afr_tilt = tilt_map(afr)
# loop over the layers and add to the ggplot 
gg_mapLayers <- list(list(i = 0,
                          data = tilt_rast,
                          color = "Purples3",
                          val_col = "humans",
                          name = "human population",
                          mid = NULL,
                          rev_pal = TRUE,
                          scale_func = colorspace::scale_color_continuous_sequential),
                     list(i = 1,
                          data = tilt_rast,
                          color = "Greens", #Emrld",
                          val_col = "veg",
                          name = "forest cover",
                          mid = NULL,
                          rev_pal = TRUE,
                          scale_func = colorspace::scale_color_continuous_sequential), 
                     list(i = 2,
                          data = tilt_rast,
                          color = "Green-Brown",
                          val_col = "water",
                          name = "water availability",
                          mid = -7,
                          rev_pal = TRUE,
                          scale_func = colorspace::scale_color_continuous_diverging), 
                     list(i = 3,
                          data = tilt_rast,
                          color = "YlGnBu", #Emrld",
                          val_col = "temp_season",
                          name = "temperature variation",
                          mid = NULL,
                          rev_pal = TRUE,
                          scale_func = colorspace::scale_color_continuous_sequential), 
                     list(i = 4, 
                          data = tilt_rast, 
                          color = "Heat", #"Lajolla", #"YlOrRd", # "Blue-Red", 
                          val_col = "temp_mean",
                          name = "mean temperature", 
                          mid = NULL, #25, 
                          rev_pal = TRUE, #FALSE,
                          scale_func = colorspace::scale_color_continuous_sequential))  %>% 
  purrr::reduce(.f = function(prev_gg, layer_list){
    # construct the color scale 
    if(!is.null(layer_list$mid)) { 
      color_scale <- layer_list$scale_func(palette = layer_list$color, 
                                           aesthetics = c("color", "fill"),
                                           rev = layer_list$rev_pal,
                                           name = layer_list$name, 
                                           mid = layer_list$mid, 
                                           guide = guide_colorbar(ticks.colour = NA,
                                                                  label = FALSE,
                                                                  title.position = "right",
                                                                  barheight = 3, 
                                                                  title.theme = element_text(size = 12),
                                                                  order = 10 - layer_list$i)) 
    } else {
      color_scale <- layer_list$scale_func(palette = layer_list$color, 
                                           aesthetics = c("color", "fill"),
                                           rev = layer_list$rev_pal,
                                           name = layer_list$name, 
                                           guide = guide_colorbar(ticks.colour = NA,
                                                                  label = FALSE,
                                                                  title.position = "right",
                                                                  barheight = 3, 
                                                                  title.theme = element_text(size = 12),
                                                                  order = 10 - layer_list$i)) 
    }
    
    # if its not the first layer, set a new color scale 
    new_gg <- prev_gg + 
      {if(layer_list$i > 0) ggnewscale::new_scale_fill()} + 
      {if(layer_list$i > 0) ggnewscale::new_scale_color()} +
      # add the layer of interest, shifting the y value upwards to layer them
      geom_sf(data = tilt_rast %>% 
                mutate(humans = asinh(humans), 
                       water = log(water)) %>% 
                mutate(geometry = geometry + layer_list$i*c(0, 30)) %>% 
                rename(value = !!sym(layer_list$val_col)), 
              aes(fill = value, color = value), 
              size = 0.5, alpha = 1) + 
      geom_sf(data = afr_tilt %>% 
                mutate(geometry = geometry + layer_list$i*c(0, 30)), 
              color = "black", fill = NA,
              linewidth = 0.6, inherit.aes = FALSE) + 
      # add the color palette based on the specified values for this layer 
      color_scale 
    return(new_gg)
  },  .init = ggplot())  

ggsave(gg_mapLayers + theme_void(), 
       filename = "./figures/figure1b.png", 
       width = 5, height = 7)

# fig 1c: topt tmin identification ----
topt_tmin %>% 
  pivot_longer(cols = c(yhat, deriv, yhat_scale),
               values_to = "est") %>% 
  mutate(species = gsub("_", " ", species), 
         name = case_match(name, 
                           "deriv" ~ "d Pr(occ)/dT", 
                           "yhat_scale" ~ "scaled Pr(occ)", 
                           "yhat" ~ "Pr(occ)",
                           .default = name) %>% 
           factor(levels = c("Pr(occ)", "scaled Pr(occ)", "d Pr(occ)/dT"), 
                  ordered = T),
         .by = c(species, feature, iteration))%>% 
  filter(name != "Pr(occ)" & 
           species == "Aedes albopictus" & 
           iteration == 1) %>%
  {ggplot(data = ., 
          aes(x = value, y = est, 
              group = iteration)) + 
      geom_hline(data = data.frame(y = 0, 
                                   name = factor("d Pr(occ)/dT", 
                                                 levels = c("Pr(occ)", "scaled Pr(occ)", "d Pr(occ)/dT"), 
                                                 ordered = T)), 
                 aes(yintercept = y), 
                 color = "grey45") + 
      # geom_ribbon(data = m_t %>% filter(grepl("albopictus", species)) %>% 
      #               mutate(name = factor("scaled Pr(occ)", 
      #                                    levels = c("Pr(occ)", "scaled Pr(occ)", "d Pr(occ)/dT"), 
      #                                    ordered = T)), 
      #             aes(x = Temp, 
      #                 ymin = q2p5_scale, 
      #                 ymax = q97p5_scale), 
      #             color = NA,
      #             fill = "grey30", alpha = 0.4, 
      #             inherit.aes = FALSE) + 
      geom_line(data = m_t %>% filter(grepl("albopictus", species)) %>% 
                  mutate(q50_scale = q50/max(q50)) %>% 
                  mutate(name = factor("scaled Pr(occ)", 
                                       levels = c("Pr(occ)", "scaled Pr(occ)", "d Pr(occ)/dT"), 
                                       ordered = T)), 
                aes(x = Temp, 
                    y = q50_scale), 
                color = "grey10", linetype = "longdash", 
                inherit.aes = FALSE) + 
      geom_point(data = m_t %>% filter(grepl("albopictus", species)) %>% 
                   mutate(q50_scale = q50/max(q50)) %>% 
                   mutate(name = factor("scaled Pr(occ)", 
                                        levels = c("Pr(occ)", "scaled Pr(occ)", "d Pr(occ)/dT"), 
                                        ordered = T)) %>% 
                   arrange(Temp) %>% 
                   mutate(topt = q50_scale == max(q50_scale), 
                          tmin = (q50_scale == 0) & (lead(q50_scale) > 0)) %>% 
                   filter(topt | tmin), 
                 aes(x = Temp, 
                     y = q50_scale), 
                 size = 3,
                 color = c("#0b596d", "#8e5300"), 
                 inherit.aes = FALSE) + 
      geom_line(color = "grey30") + 
      geom_point(data = pivot_longer(., c(tmin, topt),
                                     values_to = "critical_point", 
                                     names_to = "type") %>% 
                   filter(critical_point),
                 aes(color = type),
                 show.legend = FALSE,
                 size = 3) + 
      facet_wrap(~name, scales = "free_y", nrow = 2,
                 switch = "y") + 
      ylab("") + xlab("Temperature (°C)") + 
      scale_color_manual(name = "", values = c("#1295b5", "#ed8b00")) +
      theme_classic() + 
      coord_cartesian(clip = 'off') +
      geom_text(data = data.frame(x = c(7, 34), 
                                  y = c(0.5, 0.7), 
                                  lab = c("SDM-\nbased", "lab-\nbased"),
                                  name = factor("scaled Pr(occ)", 
                                                levels = c("Pr(occ)", "scaled Pr(occ)", "d Pr(occ)/dT"), 
                                                ordered = T)), 
                aes(x = x, y = y, label = lab), inherit.aes = FALSE, 
                lineheight = .7, size = 3.5) + 
      xlim(0, 37) + 
      ggh4x::force_panelsizes(rows = c(1, 0.7)) + 
      theme(strip.background = element_blank(), 
            strip.placement = "outside", 
            strip.text.x = element_text(size = 9.7, face = "italic"),
            plot.margin = unit(c(0, 0, 0, -10), "pt"))} %>% 
  ggsave(filename = "./figures/figure1c.png", 
         width = 3, height = 2.5)

# fig SX: drop continents -----
# S9 and fig4 equivalent with continents dropped?
pdp_sensitivity <- list.files("./output_revision", recursive = T, 
                              pattern = "univariate_pdp", full.names = T) %>% 
  purrr::map(~readRDS(.x) %>% mutate(file = .x)) %>% 
  list_rbind %>% 
  separate(file, into = c(NA, NA, "drop", "file"), sep = "/")


topt_tmin_sens <- pdp_sensitivity %>%   
  filter(feature %in% c("TAM", "PhotoASTM", "PrecipASTM")) %>% 
  arrange(species, feature, iteration, value) %>% 
  mutate(deriv = (lead(yhat) - yhat)/(lead(value) - value),
         deriv_2pos = (deriv > 0) & lead(deriv, 1) >= 0,
         tmin = deriv_2pos & cumsum(deriv_2pos) == 1,
         # deriv3 = (lead(yhat, 5) - lag(yhat, 5))/(lead(value, 5) - lag(value, 5)),
         # tmin3 = cumsum(replace_na((deriv3 > 0) == 1, 0)), 
         # tmin_comb = pmin(tmin, tmin3),
         topt = (yhat == max(yhat)) & (lag(deriv) > 0 & deriv < 0), 
         yhat_scale = (yhat - min(yhat))/(max(yhat) - min(yhat)),
         .by = c(species, feature, iteration, drop)) 

pdp_sensitivity %>% 
  filter(feature %in% c("TAM", "PhotoASTM", "PrecipASTM")) %>% 
  filter(n_distinct(drop) > 1, .by = species) %>% 
  mutate(drop = ifelse(drop == "updated-mos-sdm-results-jun19", "all", drop)) %>% 
  ggplot(aes(x = value, y = yhat, group = interaction(species, drop, iteration), 
             color = drop)) + 
  geom_line() + 
  facet_wrap(~species) + 
  theme_classic()

plot_grid(topt_tmin_sens %>% 
            filter(n_distinct(drop) > 1, .by = species) %>% 
            pivot_longer(cols = c(yhat, deriv, yhat_scale),
                         values_to = "est") %>% 
            mutate(species = gsub("_", " ", species), 
                   drop = gsub("-", " ", drop) %>% stringr::str_to_title() %>% 
                     {ifelse(grepl("^No", .), ., "All occurrences")},
                   name = case_match(name, 
                                     "deriv" ~ "d Pr(occurrence)/dT", 
                                     "yhat_scale" ~ "scaled Pr(occurrence)", 
                                     "yhat" ~ "Pr(occurrence)",
                                     .default = name) %>% 
                     factor(levels = c("Pr(occurrence)", "scaled Pr(occurrence)", "d Pr(occurrence)/dT"), 
                            ordered = T),
                   .by = c(species, feature, iteration))%>% 
            filter(name != "Pr(occurrence)") %>%
            {ggplot(data = ., 
                    aes(x = value, y = est, 
                        shape = drop,
                        group = interaction(iteration, drop))) + 
                geom_line(aes(color = drop), alpha = 0.3) + 
                geom_point(data = pivot_longer(., c(tmin, topt),
                                               values_to = "critical_point", 
                                               names_to = "type") %>% 
                             filter(critical_point),
                           aes(color = drop, shape = drop),
                           alpha = 0.9, 
                           show.legend = TRUE,
                           size = 1) + 
                geom_label(data = pivot_longer(., c(tmin, topt),
                                               values_to = "critical_point", 
                                               names_to = "type") %>% 
                             filter(name == "d Pr(occurrence)/dT") %>% 
                             summarise(mid = round(median(value[critical_point], na.rm = T), 1),
                                       min = round(min(value[critical_point], na.rm = T), 1),
                                       max = round(max(value[critical_point], na.rm = T), 1),
                                       xmin = min(value),
                                       xmax = max(value),
                                       .by = c(species, type, drop, name)) %>% 
                             mutate(xmid = (xmax - xmin)*0.55 + xmin,
                                    lab = ifelse(drop == "All occurrences", 
                                                 paste0(gsub("^t", "T[", type), "]", ": ", mid, "~(", min, " - ", max, ")"),
                                                 paste0(mid, "~(", min, " - ", max, ")")),
                                    xloc = case_when(drop == "All occurrences" ~ xmin, 
                                                     drop == "No North America" ~ xmax, 
                                                     drop == "No Europe" ~ xmid)), 
                           aes(x = xloc, y = 0.32, label = lab, color = drop, 
                               vjust = ifelse(type == "tmin", 0, 1), 
                               hjust = case_when(drop == "All occurrences" ~ 0, 
                                                 drop == "No North America" ~ 1, 
                                                 drop == "No Europe" ~ 0.5)), 
                           parse = T, size = 3, 
                           label.size = 0, label.padding = unit(0.1, "lines"),
                           inherit.aes = FALSE, show.legend = FALSE) + 
                facet_grid(name ~ species, scales = "free", 
                           switch = "y") + 
                ylab("") + xlab("Temperature (°C)") + 
                scale_color_manual(name = "", values = c("grey30", "#dd5129", "#0f7ba2")) +
                scale_shape_manual(name = "", values = c(19, 0, 2)) +
                theme_classic() + 
                coord_cartesian(clip = 'off') +
                guides(colour = guide_legend(override.aes = list(alpha = 1)), 
                       shape = "legend") +
                theme(strip.background = element_blank(), 
                      strip.placement = "outside", 
                      strip.text.x = element_text(size = 9.7, face = "italic"),
                      legend.position = "inside",
                      legend.position.inside = c(0.15, 0.85),
                      legend.title = element_blank(),
                      plot.margin = unit(c(15, 0, 5, -10), "pt"))},
          # impact on the fig 4 correlations 
          topt_tmin_sens %>%
            summarise(Tmin = first(value[tmin]), 
                      Topt = first(value[topt]),
                      .by = c(species, feature, drop, iteration)) %>%  
            pivot_longer(starts_with("T")) %>% 
            summarise(sdm_mid = median(value, na.rm = T), 
                      sdm_min = min(value, na.rm = T), 
                      sdm_max = max(value, na.rm = T),
                      .by = c(species, feature, drop, name)) %>%  
            left_join(ct %>% 
                        rename(name = CT) %>% 
                        rename_with(~ paste0("lab_", .x), starts_with("q"))) %>% 
            mutate(species = gsub("_", " ", species)) %>% 
            mutate(name = gsub("T", "T[", name) %>% paste0(., "]"), 
                   drop = gsub("-", " ", drop) %>% stringr::str_to_title() %>% 
                     {ifelse(grepl("^No", .), ., "All occurrences")}) %>% 
            mutate(n_distinct_drop = n_distinct(drop), 
                   .by = species) %>% 
            mutate(comp_group = ifelse(n_distinct_drop > 1, drop, "comparison")) %>% 
            {ggplot(data = .,
                    aes(color = species, x = lab_q50, y = sdm_mid)) + 
                geom_linerange(aes(xmin = lab_q2p5, xmax = lab_q97p5), show.legend = FALSE) + 
                geom_linerange(aes(ymin = sdm_min, ymax = sdm_max), show.legend = FALSE) + 
                geom_point(aes(shape = drop)) + 
                geom_abline(slope = 1, intercept = 0, color = "grey40", linetype = "dashed") + 
                geom_point(data = {purrr::pmap(distinct(dplyr::select(., name_i = name, drop_i = drop)), 
                                               function(name_i, drop_i){
                                                 test <- filter(., name == name_i, comp_group %in% c(drop_i, "comparison")) 
                                                 data.frame(name = name_i, 
                                                            drop = drop_i, 
                                                            min = min(c(test$lab_q2p5, test$sdm_min)),
                                                            max = max(c(test$lab_q97p5, test$sdm_max)),
                                                            r = cor(test$sdm_mid, test$lab_q50)) %>% 
                                                   return}) %>% 
                    list_rbind %>% 
                    mutate(lab = paste0(case_when(drop == "All occurrences" ~ "△ ", 
                                                  drop == "No Europe" ~ "△ ", 
                                                  T ~ "△ "), 
                                        drop, ", r = ", round(r, 3)), 
                           yloc = max - (max - min)*0.05 + 
                             case_when(drop == "All occurrences" ~ 1, 
                                       drop == "No Europe" ~ -1, 
                                       T ~ -3))}, 
                    aes(x = min + (max - min)*0, y = yloc, shape = drop), 
                    inherit.aes = FALSE) +
                geom_text(data = {purrr::pmap(distinct(dplyr::select(., name_i = name, drop_i = drop)), 
                                              function(name_i, drop_i){
                                                test <- filter(., name == name_i, comp_group %in% c(drop_i, "comparison")) 
                                                data.frame(name = name_i, 
                                                           drop = drop_i, 
                                                           min = min(c(test$lab_q2p5, test$sdm_min)),
                                                           max = max(c(test$lab_q97p5, test$sdm_max)),
                                                           r = cor(test$sdm_mid, test$lab_q50)) %>% 
                                                  return}) %>% 
                    list_rbind %>% 
                    mutate(lab = paste0(drop, ", r = ", round(r, 3)), 
                           yloc = max - (max - min)*0.05 + 
                             case_when(drop == "All occurrences" ~ 1, 
                                       drop == "No Europe" ~ -1, 
                                       T ~ -3))}, 
                          aes(x = min + (max - min)*0.02,
                              y = yloc,
                              label = lab),
                          hjust = 0, size = 3,
                          inherit.aes =  FALSE) + 
                facet_wrap(~name, scales = "free", 
                           labeller = label_parsed) + 
                theme_classic() + 
                theme(strip.background = element_blank(), 
                      strip.text = element_text(size = 14, face = "bold"), 
                      legend.text = element_text(face = "italic"), 
                      plot.margin = unit(c(15.5, 0, 5.5, 5.5), "points")) + 
                scale_color_manual(values = c("#b695bc", "#574571", 
                                              "#7fa074", "#2c4b27", 
                                              "#f2c88f", "#d37750", "#92351e")) + 
                scale_shape_manual(name = "", values = c(19, 0, 2)) +
                guides(color = guide_legend(override.aes = aes(label = "")), 
                       shape = "none") + 
                xlab("mechanistic lab-based estimate") + 
                ylab("statistical SDM-based estimate") + 
                ggh4x::facetted_pos_scales(
                  x = list(scale_x_continuous(limits = filter(., name == "T[min]") %>%
                                                {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
                           scale_x_continuous(limits = filter(., name == "T[opt]") %>%
                                                {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))})),
                  y = list(scale_y_continuous(limits = filter(., name == "T[min]") %>%
                                                {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
                           scale_y_continuous(limits = filter(., name == "T[opt]") %>%
                                                {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}))
                )}, 
          labels = c("a) Critical thermal values with different geographic samples", 
                     "b) Comparison of different geographic samples to mechanistic thermal values"),
          hjust = 0, label_x = 0.01, label_size = 12,
          rel_heights = c(1.1, 0.9),
          nrow = 2) %>% 
  ggsave(filename = "figures/figureSX.png", 
         width = 7.5, height = 8)

# also plot species occurrences for without Aedes without europe and NA america 
alt_subset_ll = rbind(read.csv("./output_revision/no-europe/SDM_Data_June142024_noEurope.csv") %>% 
                        dplyr::select(Species, Occ1_or_Bg0, Data_Split, starts_with("Centroid")) %>% 
                        mutate(subset = "No Europe"),
                      read.csv("./output_revision/no-north-america/SDM_Data_June142024_noNA.csv") %>% 
                        dplyr::select(Species, Occ1_or_Bg0, Data_Split, starts_with("Centroid")) %>% 
                        mutate(subset = "No North America"))

bg_shapes <- readRDS("./simplified_bg_shapes.rds")  
coast <- giscoR::gisco_get_coastallines(resolution = 20)

# scale alpha as inverse of # of occ / area 
purrr::pmap(alt_subset_ll %>% dplyr::select(spec = Species, sub = subset) %>% distinct, 
           function(spec, sub){
             print(spec)
             print(sub)
             bg <- bg_shapes %>% 
               filter(tolower(species) == tolower(spec))
             bbox_lim = st_bbox(bg)
             bg_occ <- alt_subset_ll %>% 
               filter(Species == spec, subset == sub) %>% 
               st_as_sf(coords = c("Centroid_Longitude", "Centroid_Latitude")) %>% 
               st_set_crs(4326)
             #print(bbox_lim)
             #print(nrow(bg_occ))
             sf_use_s2(FALSE)
             spec_alpha = (st_area(bg)/1e6)/nrow(bg_occ)*3e-5
             sf_use_s2(TRUE)
             {ggplot() + 
                 geom_sf(data = coast, 
                         fill = "grey93") + 
                 geom_sf(data = bg, 
                         fill = "#A9B4C9", #"#606B84", #"steelblue4", "#607B8B"
                         color = NA) +  #"steelblue4", 
                 geom_sf(data = bg_occ, 
                         alpha = spec_alpha,
                         aes(color = as.factor(Occ1_or_Bg0), alpha = as.factor(Occ1_or_Bg0)), 
                         shape = 16, size = 0.5) + 
                 scale_color_manual(name = "", 
                                    values = c("grey20", "red"), 
                                    labels = c("background", "occurrence")) + 
                 scale_alpha_manual(name = "", 
                                    values = c(0.05, 0.1), 
                                    labels = c("background", "occurrence")) + 
                 theme_void() + 
                 xlim(bbox_lim[1], bbox_lim[3]) + 
                 ylim(bbox_lim[2], bbox_lim[4]) + 
                 ggtitle(str_to_sentence(spec)) +
                 guides(color = guide_legend(override.aes = list(alpha = 1, 
                                                                 size = 1.5))) + 
                 theme(legend.position = "inside", 
                       legend.position.inside = c(0.15, 0.25), 
                       title = element_text(face = "italic"),
                       legend.title = element_blank(),
                       legend.margin = margin(t = -2, r = 5, b = 0, l = 0, 
                                              unit = "pt"),
                       legend.background = element_rect(fill = "white"))} %>% 
               ggsave(filename = paste0("./figures/", 
                                        gsub(" ", "_", str_to_sentence(spec)), 
                                        "_",
                                        gsub(" ", "_", str_to_sentence(sub)), 
                                        "_bg_occ_map.png"), 
                      width = 5, height = 3)
           })

# testing/old/scratch -----
# pdp %>%   
#   filter(feature %in% c("TAM", "PhotoASTM", "PrecipASTM")) %>% 
#   arrange(species, feature, iteration, value) %>% 
#   mutate(deriv = (lead(yhat) - yhat)/(lead(value) - value),
#          deriv_2pos = (deriv > 0) & lead(deriv, 1) >= 0,
#          tmin = deriv == max(deriv, na.rm = T),
#          # deriv3 = (lead(yhat, 5) - lag(yhat, 5))/(lead(value, 5) - lag(value, 5)),
#          # tmin3 = cumsum(replace_na((deriv3 > 0) == 1, 0)), 
#          # tmin_comb = pmin(tmin, tmin3),
#          topt = (yhat == max(yhat)) & (lag(deriv) > 0 & deriv < 0), 
#          yhat_scale = (yhat - min(yhat))/(max(yhat) - min(yhat)),
#          .by = c(species, feature, iteration))  %>% 
#   summarise(Tmin = first(value[tmin]), 
#             Topt = first(value[topt]),
#             .by = c(species, feature, iteration)) %>%  
#   pivot_longer(starts_with("T")) %>% 
#   summarise(sdm_mid = median(value, na.rm = T), 
#             sdm_min = min(value, na.rm = T), 
#             sdm_max = max(value, na.rm = T),
#             .by = c(species, feature, name)) %>%  
#   left_join(ct %>% 
#               rename(name = CT) %>% 
#               rename_with(~ paste0("lab_", .x), starts_with("q"))) %>% 
#   mutate(species = gsub("_", " ", species)) %>% 
#   mutate(name = gsub("T", "T[", name) %>% paste0(., "]")) %>% 
#   {ggplot(data = .,
#           aes(color = species, x = lab_q50, y = sdm_mid)) + 
#       geom_point() + 
#       geom_linerange(aes(xmin = lab_q2p5, xmax = lab_q97p5), show.legend = FALSE) + 
#       geom_linerange(aes(ymin = sdm_min, ymax = sdm_max), show.legend = FALSE) + 
#       geom_abline(slope = 1, intercept = 0) + 
#       geom_text(data = summarise(.,
#                                  r2 = summary(lm(sdm_mid ~ lab_q50))$r.squared,
#                                  min = min(c(lab_q2p5, sdm_min)), 
#                                  max = max(c(lab_q97p5, sdm_max)), 
#                                  .by = name) %>%
#                   mutate(label = paste0("R^{2} == ", round(r2, 2))),
#                 aes(x = min + (max - min)*0.2, 
#                     y = max - (max - min)*0.05, 
#                     label = label), 
#                 hjust = "outwards", 
#                 inherit.aes =  FALSE, parse = T) + 
#       # geom_smooth(method = "lm", 
#       #             se = FALSE,
#       #             color = "grey10", 
#       #             linetype = "dashed") + 
#       # ggpubr::stat_regline_equation(aes(label =  ..rr.label..)) +
#       facet_wrap(~name, scales = "free", 
#                  labeller = label_parsed) + 
#       theme_classic() + 
#       theme(strip.background = element_blank(), 
#             strip.text = element_text(size = 14, face = "bold"), 
#             legend.text = element_text(face = "italic")) + 
#       scale_color_manual(values = c("#b695bc", "#574571", 
#                                              "#7fa074", "#2c4b27", 
#                                              "#f2c88f", "#d37750", "#92351e")) + 
#                                                guides(color = guide_legend(override.aes = aes(label = ""))) + 
#       xlab("mechanistic lab-based estimate") + 
#       ylab("statistical SDM-based estimate") + 
#       ggh4x::facetted_pos_scales(
#         x = list(scale_x_continuous(limits = filter(., name == "T[min]") %>%
#                                       {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
#                  scale_x_continuous(limits = filter(., name == "T[opt]") %>%
#                                       {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))})),
#         y = list(scale_y_continuous(limits = filter(., name == "T[min]") %>%
#                                       {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
#                  scale_y_continuous(limits = filter(., name == "T[opt]") %>%
#                                       {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}))
#       )} 
# 
# sdm_df %>% 
#   ggplot(aes(x = TAM, y = TASD, 
#              color = Activity_Season)) + 
#   geom_point(alpha = 0.2) + 
#   facet_wrap(~Activity_Season) + 
#   theme_classic()

# species_panels <- function(plot_fun, 
#                            theme_leg = theme(legend.box.margin = margin(120, 0, 0, 0), 
#                                              legend.key.size = unit(1.8, "lines"), 
#                                              legend.text = element_text(size = 10)), 
#                            plot_grid_label_x = 0, 
#                            plot_grid_label_y = 1, 
#                            plot_grid_rel_heights = c(1, -0.05, 1, -0.05, 1), 
#                            ...){
#   plot_grid(plot_fun("Aedes.aegypti") + 
#               theme(legend.position =  "none") + 
#               xlab(""),
#             plot_fun(vip_sum %>% filter(grepl("Aedes.albopictus", species)), 
#                      auc_sum %>% filter(grepl("Aedes.albopictus", species))) + 
#               theme(legend.position =  "none") + 
#               xlab("") + ylab(""),
#             get_legend(
#               plot_fun(vip_sum %>% filter(grepl("Aedes.aegypti", species)), 
#                        auc_sum %>% filter(grepl("Aedes.aegypti", species))) + 
#                 theme_leg
#             ), 
#             NULL, NULL, NULL,
#             plot_fun(vip_sum %>% filter(grepl("Anopheles.gambiae", species)), 
#                      auc_sum %>% filter(grepl("Anopheles.gambiae", species))) + 
#               theme(legend.position =  "none") + 
#               xlab(""),
#             plot_fun(vip_sum %>% filter(grepl("Anopheles.stephensi", species)), 
#                      auc_sum %>% filter(grepl("Anopheles.stephensi", species))) + 
#               theme(legend.position =  "none") + 
#               xlab("") + ylab(""),
#             NULL, 
#             NULL, NULL, NULL,
#             plot_fun(vip_sum %>% filter(grepl("Culex.pipiens", species)), 
#                      auc_sum %>% filter(grepl("Culex.pipiens", species))) + 
#               theme(legend.position =  "none"), 
#             plot_fun(vip_sum %>% filter(grepl("Culex.quinquefasciatus", species)), 
#                      auc_sum %>% filter(grepl("Culex.quinquefasciatus", species))) + 
#               theme(legend.position =  "none") + ylab(""), 
#             plot_fun(vip_sum %>% filter(grepl("Culex.tarsalis", species)), 
#                      auc_sum %>% filter(grepl("Culex.tarsalis", species)))+ 
#               theme(legend.position =  "none") + ylab(""), 
#             nrow = 5, ncol = 3, 
#             rel_heights = plot_grid_rel_heights,
#             align = "hv", 
#             axis = "rlbt",
#             label_x = plot_grid_label_x, 
#             label_y = plot_grid_label_y,
#             labels = c("A) ", "B)", "", 
#                        "", "", "", 
#                        "C)", "D)", "", 
#                        "", "", "", 
#                        "E)", "F)", "G)"))
# }
# library(giscoR)
# ae_aegypti_bg <- readRDS("./Shapefiles/Shapefile_AedesAegypti.RDS") 
# 
# sf_use_s2(FALSE)
# 
# ae_aegypti_bg %<>% st_make_valid %>% 
#   st_simplify(preserveTopology = T, dTolerance = 0.1)
# 
# sf_use_s2(TRUE)

# sdm_df %>% filter(species == "Aedes aegypti" & Occ1_or_Bg0 == 1) %>% nrow
# sdm_df %>% filter(species == "Aedes aegypti" & Occ1_or_Bg0 == 1) %>% 
#   select(starts_with("Centr")) %>% unique %>% nrow
# 
# fig1_map <- {ggplot() + 
#   geom_sf(data = coast, 
#           fill = "grey85") + 
#   geom_sf(data = ae_aegypti_bg, 
#           fill = "lightskyblue4", #"steelblue4", 
#           color = NA, #"steelblue4", 
#           alpha = 0.7) + 
#   geom_sf(data = sdm_df %>% 
#             filter(species == "Aedes aegypti") %>% 
#             st_as_sf(coords = c("Centroid_Longitude", "Centroid_Latitude")) %>% 
#             st_set_crs(4326), 
#           alpha = 0.1,
#           aes(color = as.factor(Occ1_or_Bg0), alpha = as.factor(Occ1_or_Bg0)), 
#           shape = 16, size = 0.5) + 
#     scale_color_manual(name = "", 
#                        values = c("grey20", "red"), 
#                        labels = c("background", "occurrence")) + 
#     scale_alpha_manual(name = "", 
#                        values = c(0.05, 0.1), 
#                        labels = c("background", "occurrence")) + 
#     theme_void() + 
#     guides(color = guide_legend(override.aes = list(alpha = 1))) + 
#     theme(legend.position = c(0.15, 0.5))}
# 
# ggsave(fig1_map, 
#        filename = "./figures/figure1.png", 
#        width = 5, height = 3)
# {ggplot(data = .,
#         aes(color = species, x = lab_q50, y = sdm_mid)) + 
#     geom_point() + 
#     geom_linerange(aes(xmin = lab_q2p5, xmax = lab_q97p5), show.legend = FALSE) + 
#     geom_linerange(aes(ymin = sdm_min, ymax = sdm_max), show.legend = FALSE) + 
#     geom_abline(slope = 1, intercept = 0) + 
#     geom_text(data = summarise(.,
#                                r2 = summary(lm(sdm_mid ~ lab_q50))$r.squared,
#                                min = min(c(lab_q2p5, sdm_min)), 
#                                max = max(c(lab_q97p5, sdm_max)), 
#                                .by = name) %>%
#                 mutate(label = paste0("R^{2} == ", round(r2, 2))),
#               aes(x = min + (max - min)*0.2, 
#                   y = max - (max - min)*0.05, 
#                   label = label), 
#               hjust = "outwards", 
#               inherit.aes =  FALSE, parse = T) + 
#     # geom_smooth(method = "lm", 
#     #             se = FALSE,
#     #             color = "grey10", 
#     #             linetype = "dashed") + 
#     # ggpubr::stat_regline_equation(aes(label =  ..rr.label..)) +
#     facet_wrap(~name, scales = "free", 
#                labeller = label_parsed) + 
#     theme_classic() + 
#     theme(strip.background = element_blank(), 
#           strip.text = element_text(size = 14, face = "bold"), 
#           legend.text = element_text(face = "italic")) + 
#     scale_color_manual(values = c("#b695bc", "#574571", 
#                                   "#7fa074", "#2c4b27", 
#                                   "#f2c88f", "#d37750", "#92351e")) + 
#     guides(color = guide_legend(override.aes = aes(label = ""))) + 
#     xlab("mechanistic lab-based estimate") + 
#     ylab("statistical SDM-based estimate") + 
#     ggh4x::facetted_pos_scales(
#       x = list(scale_x_continuous(limits = filter(., name == "T[min]") %>%
#                                     {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
#                scale_x_continuous(limits = filter(., name == "T[opt]") %>%
#                                     {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))})),
#       y = list(scale_y_continuous(limits = filter(., name == "T[min]") %>%
#                                     {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}),
#                scale_y_continuous(limits = filter(., name == "T[opt]") %>%
#                                     {range(c(.$lab_q2p5, .$sdm_min, .$lab_q97p5, .$sdm_max))}))
#     )}

# manuscript calculations ---- 
# table S3 
topt_tmin %>% 
  summarise(Tmin = first(value[tmin]), 
            Topt = first(value[topt]),
            .by = c(species, feature, iteration)) %>%  
  pivot_longer(starts_with("T")) %>% 
  summarise(sdm_mid = median(value, na.rm = T), 
            sdm_min = min(value, na.rm = T), 
            sdm_max = max(value, na.rm = T),
            .by = c(species, feature, name)) %>% 
  dplyr::select(-feature) %>% 
  full_join(ct %>% 
              rename(name = CT) %>% 
              rename_with(~ paste0("lab_", .x), starts_with("q"))) %>% 
  # mutate(sdm = paste0(round(sdm_mid, 1), "(", round(sdm_min, 1), "-", round(sdm_max, 1), ")"),
  #        lab = paste0(round(lab_q50, 1), "(", round(lab_q2p5, 1), "-", round(lab_q97p5, 1), ")")) %>% 
  mutate(sdm = round(sdm_mid, 1),
         lab = round(lab_q50, 1)) %>% 
  dplyr::select(species, name, sdm, lab) %>% 
  pivot_longer(cols = c("sdm", "lab"), names_to = "type") %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(across(c(Tmin, Topt, Tmax), list(rank =~ rank(-.x))), 
         .by = type) %>% 
  arrange(species, type) 

  
# correlation in tmin and topt (abstract, results)
topt_tmin %>% 
  summarise(Tmin = first(value[tmin]), 
            Topt = first(value[topt]),
            .by = c(species, feature, iteration)) %>%  
  pivot_longer(starts_with("T")) %>% 
  summarise(sdm_mid = median(value, na.rm = T), 
            sdm_min = min(value, na.rm = T), 
            sdm_max = max(value, na.rm = T),
            .by = c(species, feature, name)) %>% 
  left_join(ct %>% 
              rename(name = CT) %>% 
              rename_with(~ paste0("lab_", .x), starts_with("q"))) %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  mutate(name = gsub("T", "T[", name) %>% paste0(., "]")) %>% 
  summarise(r = cor(sdm_mid, lab_q50),
            .by = name) 

# difference between results
topt_tmin %>% 
  summarise(Tmin = first(value[tmin]), 
            Topt = first(value[topt]),
            .by = c(species, feature, iteration)) %>%  
  pivot_longer(starts_with("T")) %>% 
  summarise(sdm_mid = median(value, na.rm = T), 
            sdm_min = min(value, na.rm = T), 
            sdm_max = max(value, na.rm = T),
            .by = c(species, feature, name)) %>% 
  left_join(ct %>% 
              rename(name = CT) %>% 
              rename_with(~ paste0("lab_", .x), starts_with("q"))) %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  mutate(name = gsub("T", "T[", name) %>% paste0(., "]"), 
         diff = sdm_mid - lab_q50) %>% 
  dplyr::select(-starts_with(c("sdm", "lab"))) %>% 
  arrange(name)
