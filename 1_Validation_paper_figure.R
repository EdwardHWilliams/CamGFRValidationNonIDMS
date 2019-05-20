library(cowplot)

results <- Results_nonIDMS_data_centre%>% 
  bind_rows(mutate(Results_nonIDMS_data, Centre = "All")) %>%
  ungroup() %>% 
  filter(equation %in% c("WJ", "CKD_adj", "Wright", "MDRD_adj", "Cockcroft")) %>%
  mutate(equation = factor(equation, 
                           levels = c("WJ", "CKD_adj", "MDRD_adj", "Wright",  
                                      "Cockcroft"))) %>%
  mutate(Centre = factor(Centre, 
                         levels = c("All", "Manchester", "Edinburgh", "Cambridge", 
                                    "Southampton", "Melbourne", "Wales", "London-Barts"), 
                         labels = c("All Centres\n(n = 3827)", "Manchester\n(n = 1777)", 
                                    "Edinburgh\n(n = 599)", "Cambridge\n(n = 443)", 
                                    "Southampton\n(n = 436)", "Melbourne\n(n = 308)", 
                                    "Wales \n(n = 156)", "London-Barts\n(n = 108)"))) 


p1_1 <- results %>% 
  filter(Centre %in% c("All Centres\n(n = 3827)", "Manchester\n(n = 1777)")) %>%
  ggplot(aes_string(group = "equation", x = "Centre", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank())  + 
  scale_color_manual(values = c("WJ"  = "#332288", "CKD_adj" = "#88CCEE", 
                                "Wright" = "#117733", "MDRD_adj" = "#DDCC77", 
                                "Cockcroft" = "#CC6677"), 
                     labels = c("CAMGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                "Cockcroft-Gault")) +
  ylab("RMSE\n(Accuracy)") + 
  theme(text = element_text(size = 14)) + theme(axis.ticks.x = element_blank(), 
                                                axis.text.x = element_blank(), 
                                                axis.line.x = element_blank())

p1_2 <- results %>% 
  filter(!(Centre %in% c("All Centres\n(n = 3827)", "Manchester\n(n = 1777)"))) %>%
  ggplot(aes_string(group = "equation", x = "Centre", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())  + 
  scale_color_manual(values = c("WJ"  = "#332288", "CKD_adj" = "#88CCEE", 
                                "Wright" = "#117733", "MDRD_adj" = "#DDCC77", 
                                "Cockcroft" = "#CC6677"), 
                     labels = c("CAMGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                "Cockcroft-Gault")) +
  ylab("RMSE\n(Accuracy)") + 
  theme(text = element_text(size = 14)) + theme(axis.ticks.x = element_blank(), 
                                                axis.text.x = element_blank(), 
                                                axis.line.x = element_blank())


p2_1 <- results %>%
  filter(Centre %in% c("All Centres\n(n = 3827)", "Manchester\n(n = 1777)")) %>%
  ggplot(aes_string(group = "equation", x = "Centre", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  scale_color_manual(values = c("WJ"  = "#332288", "CKD_adj" = "#88CCEE", 
                                "Wright" = "#117733", "MDRD_adj" = "#DDCC77", 
                                "Cockcroft" = "#CC6677"), 
                     labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                "Cockcroft-Gault")) + 
  ylab("Residual median\n(Bias)") + 
  theme(text = element_text(size = 14)) + theme(axis.ticks.x = element_blank(), 
                                                axis.text.x = element_blank(), 
                                                axis.line.x = element_blank()) + 
  ylim(c(-4.4, 4.4))

p2_2 <- results %>% 
  filter(!(Centre %in% c("All Centres\n(n = 3827)", "Manchester\n(n = 1777)"))) %>%
  ggplot(aes_string(group = "equation", x = "Centre", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())  + 
  scale_color_manual(values = c("WJ"  = "#332288", "CKD_adj" = "#88CCEE", 
                                "Wright" = "#117733", "MDRD_adj" = "#DDCC77", 
                                "Cockcroft" = "#CC6677"), 
                     labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                "Cockcroft-Gault")) + 
  ylab("Residual median\n(Bias)") + 
  theme(text = element_text(size = 14)) + theme(axis.ticks.x = element_blank(), 
                                                axis.text.x = element_blank(), 
                                                axis.line.x = element_blank()) + 
  ylim(c(-26.3, 26.3))
p2_2



p3_1 <- results %>% 
  filter(Centre %in% c("All Centres\n(n = 3827)", "Manchester\n(n = 1777)")) %>%
  ggplot(aes_string(group = "equation", x = "Centre", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("WJ"  = "#332288", "CKD_adj" = "#88CCEE", 
                                "Wright" = "#117733", "MDRD_adj" = "#DDCC77", 
                                "Cockcroft" = "#CC6677"), 
                     labels = c(" CamGFR ", " CKD-EPI  ", " Wright  ", " MDRD-186  ", 
                                " Cockcroft-Gault ")) +
  ylab("Residual IQR\n(Precision)") +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank()) + 
  theme(text = element_text(size = 14)) + theme(axis.ticks.x = element_blank(), 
                                                axis.text.x = element_blank(), 
                                                axis.line.x = element_blank())


p3_2 <- results %>% 
  filter(!(Centre %in% c("All Centres\n(n = 3827)", "Manchester\n(n = 1777)"))) %>%
  ggplot(aes_string(group = "equation", x = "Centre", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("WJ"  = "#332288", "CKD_adj" = "#88CCEE", 
                                "Wright" = "#117733", "MDRD_adj" = "#DDCC77", 
                                "Cockcroft" = "#CC6677"), 
                     labels = c(" CamGFR ", " CKD-EPI  ", " Wright  ", " MDRD-186  ", 
                                " Cockcroft-Gault ")) +
  ylab("Residual IQR\n(Precision)") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())  +
  theme(text = element_text(size = 14)) + 
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p4_1 <- results %>% 
  filter(Centre %in% c("All Centres\n(n = 3827)", "Manchester\n(n = 1777)")) %>%
  ggplot(aes_string(group = "equation", x = "Centre", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("WJ"  = "#332288", "CKD_adj" = "#88CCEE", 
                                "Wright" = "#117733", "MDRD_adj" = "#DDCC77", 
                                "Cockcroft" = "#CC6677"), 
                     labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                "Cockcroft-Gault")) + 
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4_2 <- results %>% 
  filter(!(Centre %in% c("All Centres\n(n = 3827)", "Manchester\n(n = 1777)"))) %>%
  ggplot(aes_string(group = "equation", x = "Centre", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("WJ"  = "#332288", "CKD_adj" = "#88CCEE", 
                                "Wright" = "#117733", "MDRD_adj" = "#DDCC77", 
                                "Cockcroft" = "#CC6677"), 
                     labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                "Cockcroft-Gault")) + 
  ylab("1 - P20\n(Clinical robustness)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.spacing.x = unit(0.5, "cm"))


legend <- g_legend(p4_2)
p4_2 <- p4_2 + 
  theme(legend.position = "none")



g_left <- gtable_rbind(ggplotGrob(p2_1 ), 
                       ggplotGrob(p3_1 ), 
                       ggplotGrob(p1_1), 
                       ggplotGrob(p4_1 ))


g_right <- gtable_rbind(ggplotGrob(p2_2), 
                        ggplotGrob(p3_2 ), 
                        ggplotGrob(p1_2 ), 
                        ggplotGrob(p4_2))


p <- plot_grid(g_left, g_right, align = "h", ncol = 2, rel_widths = c(6, 12))
plot_centre <- plot_grid(p, legend, nrow = 2, rel_heights = c(20, 1))


ggsave(paste0(fig_export_dir, "Non_IDMS_centre_plot.pdf"), plot_centre,
       width = 10, height = 10)







################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


plot_data <- Results_nonIDMS_data_diagnosis %>% 
  ungroup() %>%
  filter(equation %in% c("WJ", "CKD_adj", "Wright", "MDRD_adj", "Cockcroft")) %>%
  mutate(equation = factor(equation, 
                           levels = c("WJ", "CKD_adj", "Wright", "MDRD_adj", 
                                      "Cockcroft"), 
                           labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                      "Cockcroft-Gault"))) %>%
  filter(n > 50) %>% 
  filter(Diagnosis != "Unknown", Diagnosis != "Other") %>% 
  arrange(-n) %>%
  mutate(Diagnosis = paste0(Diagnosis, "\n(n = ", n, ")")) %>%
  mutate(Diagnosis = factor(Diagnosis, levels = unique(.$Diagnosis[order(.$n, decreasing = T)])))


p1 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "Diagnosis", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("RMSE\n(Accuracy)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p2 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "Diagnosis", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("Residual median\n(Bias)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p3 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "Diagnosis", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("Residual IQR\n(Precision)") + 
  theme(legend.position = "none",
      axis.title.x = element_blank(),
      text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p4 <- plot_data  %>%
  ggplot(aes_string(group = "equation", x = "Diagnosis", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(), 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.spacing.x = unit(0.5, "cm")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_diagnosis <- rbind(ggplotGrob(p2), 
           ggplotGrob(p3), 
           ggplotGrob(p1), 
           ggplotGrob(p4), 
           size = "last")

ggsave(paste0(fig_export_dir, "Non_IDMS_diagnosis_plot.pdf"), plot_diagnosis,
       width = 10, height = 10)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

plot_data <- Table_df %>%
  mutate(Age_cut = cut_number(Age, 5)) %>% 
  group_by(equation, Age_cut) %>%
  Statistic_summary_withP20_both() %>% 
  ungroup() %>%
  filter(equation %in% c("WJ", "CKD_adj", "Wright", "MDRD_adj", "Cockcroft")) %>%
  mutate(equation = factor(equation, 
                           levels = c("WJ", "CKD_adj", "Wright", "MDRD_adj", 
                                      "Cockcroft"), 
                           labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                      "Cockcroft-Gault"))) 

p1 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "Age_cut", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("RMSE\n(Accuracy)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p2 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "Age_cut", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("Residual median\n(Bias)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p3 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "Age_cut", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("Residual IQR\n(Precision)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p4 <- plot_data  %>%
  ggplot(aes_string(group = "equation", x = "Age_cut", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.spacing.x = unit(0.5, "cm")) + 
  xlab("Age [years]") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_age <- rbind(ggplotGrob(p2), 
                  ggplotGrob(p3), 
                  ggplotGrob(p1), 
                  ggplotGrob(p4), 
                  size = "last")

ggsave(paste0(fig_export_dir, "Non_IDMS_age_plot.pdf"), plot_age,
       width = 13, height = 10)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################




plot_data <- Table_df %>%
  mutate(BSA_cut = cut_number(BSA, 5)) %>% 
  group_by(equation, BSA_cut) %>%
  Statistic_summary_withP20_both() %>% 
  ungroup() %>%
  filter(equation %in% c("WJ", "CKD_adj", "Wright", "MDRD_adj", "Cockcroft")) %>%
  mutate(equation = factor(equation, 
                           levels = c("WJ", "CKD_adj", "Wright", "MDRD_adj", 
                                      "Cockcroft"), 
                           labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                      "Cockcroft-Gault"))) 

p1 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "BSA_cut", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("RMSE\n(Accuracy)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p2 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "BSA_cut", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("Residual median\n(Bias)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p3 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "BSA_cut", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("Residual IQR\n(Precision)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p4 <- plot_data  %>%
  ggplot(aes_string(group = "equation", x = "BSA_cut", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.spacing.x = unit(0.5, "cm")) + 
  xlab("Body surface area [m\u00B2]") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_BSA <- rbind(ggplotGrob(p2), 
                  ggplotGrob(p3), 
                  ggplotGrob(p1), 
                  ggplotGrob(p4), 
                  size = "last")

ggsave(paste0(fig_export_dir, "Non_IDMS_BSA_plot.pdf"), plot_BSA,
       width = 13, height = 10)



################################################################################
################################################################################
################################################################################
################################################################################
# Plot creat split
################################################################################
################################################################################
################################################################################
################################################################################


plot_data <- Table_df %>%
  mutate(Creat_cut = cut_number(Creat, 5)) %>% 
  group_by(equation, Creat_cut) %>%
  Statistic_summary_withP20_both() %>% 
  ungroup() %>%
  filter(equation %in% c("WJ", "CKD_adj", "Wright", "MDRD_adj", "Cockcroft")) %>%
  mutate(equation = factor(equation, 
                           levels = c("WJ", "CKD_adj", "Wright", "MDRD_adj", 
                                      "Cockcroft"), 
                           labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                      "Cockcroft-Gault"))) 

p1 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "Creat_cut", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("RMSE\n(Accuracy)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p2 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "Creat_cut", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("Residual median\n(Bias)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p3 <- plot_data %>%
  ggplot(aes_string(group = "equation", x = "Creat_cut", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("Residual IQR\n(Precision)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p4 <- plot_data  %>%
  ggplot(aes_string(group = "equation", x = "Creat_cut", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", colour = "equation"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = c("CamGFR"  = "#332288", "CKD-EPI" = "#88CCEE",
                                "Wright" = "#117733", "MDRD-186" = "#DDCC77",
                                "Cockcroft-Gault" = "#CC6677")) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        text = element_text(size = 14), 
        legend.title = element_blank(), 
        legend.text = element_text(size = rel(1.2)),
        legend.spacing.x = unit(0.5, "cm")) + 
  xlab("Creatinine [mg/dL]") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_Creat <- rbind(ggplotGrob(p2), 
                  ggplotGrob(p3), 
                  ggplotGrob(p1), 
                  ggplotGrob(p4), 
                  size = "last")

ggsave(paste0(fig_export_dir, "Non_IDMS_BSA_plot.pdf"), plot_BSA,
       width = 13, height = 10)


################################################################################
################################################################################
################################################################################
################################################################################
# Plot date diff
################################################################################
################################################################################
################################################################################
################################################################################


plot_data <- Results_nonIDMS_data_date_diff %>%
  ungroup() %>%
  filter(equation %in% c("WJ", "CKD_adj", "Wright", "MDRD_adj", "Cockcroft")) %>%
  mutate(equation = factor(equation, 
                           levels = c("WJ", "CKD_adj", "Wright", "MDRD_adj", 
                                      "Cockcroft"), 
                           labels = c("CamGFR", "CKD-EPI", "Wright", "MDRD-186", 
                                      "Cockcroft-Gault"))) %>%
  mutate(Date_diff_group = factor(Date_diff_group, 
                                  levels = c("(-31,-8]", "(-8,-3]", "(-3,-1]", "(-1,0]", "(0,2]", 
                                             "(2,7]", "(7,30]", "NA"), 
                                  labels = c("-30:-8", "-7:-3", "-2:-1", "0", "1:2", 
                                             "3:7", "8:30", "NA"))) %>%
  filter(Date_diff_group != "NA")


p1 <- plot_data %>%
  ggplot(aes_string(group = "Date_diff_group", x = "equation", y = "RMSE")) + 
  geom_errorbar(aes_string(ymin = "RMSE_lwr", ymax = "RMSE_upr", colour = "Date_diff_group"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = inlmisc::GetColors( 7, scheme = "sunset")) +
  ylab("RMSE\n(Accuracy)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p2 <- plot_data %>%
  ggplot(aes_string(group = "Date_diff_group", x = "equation", y = "median")) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes_string(ymin = "median_lwr", ymax = "median_upr", colour = "Date_diff_group"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = inlmisc::GetColors( 7, scheme = "sunset")) +
  ylab("Residual median\n(Bias)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())

p3 <- plot_data %>%
  ggplot(aes_string(group = "Date_diff_group", x = "equation", y = "IQR")) + 
  geom_errorbar(aes_string(ymin = "IQR_lwr", ymax = "IQR_upr", colour = "Date_diff_group"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = inlmisc::GetColors( 7, scheme = "sunset")) +
  ylab("Residual IQR\n(Precision)") + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 14))  +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.line.x = element_blank())


p4 <- plot_data  %>%
  ggplot(aes_string(group = "Date_diff_group", x = "equation", y = "P20")) + 
  geom_errorbar(aes_string(ymin = "P20_lwr", ymax = "P20_upr", colour = "Date_diff_group"), 
                position= position_dodge(width = .5), width = 0.3, size = 1) + 
  geom_point(position= position_dodge(width = .5)) + 
  ggplot_theme() + 
  scale_color_manual(values = inlmisc::GetColors( 7, scheme = "sunset")) +
  labs(colour="GFR Date - Creatinien Date [days]") + 
  guides(colour = guide_legend(title.position = "top", nrow = 1)) +
  ylab("1 - P20\n(Clinical robustness)") +
  theme(legend.position = "bottom", 
        text = element_text(size = 14), 
        legend.text = element_text(size = rel(1.2)),
        legend.spacing.x = unit(0.5, "cm")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

plot_date_diff <- rbind(ggplotGrob(p2), 
                        ggplotGrob(p3), 
                        ggplotGrob(p1), 
                        ggplotGrob(p4), 
                        size = "last")
ggsave(paste0(fig_export_dir, "Non_IDMS_date_diff_plot.pdf"), plot_BSA,
       width = 13, height = 10)





