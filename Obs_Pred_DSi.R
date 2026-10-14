mcm<-read.csv("MCM_pred_obs_combined.csv")
mcm<-mcm %>%
  filter(!Stream_Name=="Harnish Creek at F7")

stats <- mcm %>%
  group_by(Stream_Name) %>%
  summarise(
    model = list(lm(Conc_mgL ~ observed_value)),
    .groups = "drop"
  ) %>%
  mutate(
    R2 = sapply(model, \(x) summary(x)$r.squared),
    RMSE = sapply(model, \(x) sqrt(mean(residuals(x)^2)))
  )

pdf("Obs_Pred_DSi.pdf", width = 12, height = 8)

ggplot(mcm, aes(observed_value, Conc_mgL)) +
  geom_point() +
  facet_wrap(~Stream_Name, scales = "free") +
  geom_abline(slope = 1) +
  geom_text(data = stats, aes(x = -Inf, y = Inf, label = paste0("R² = ", round(R2, 2), "\nRMSE = ", round(RMSE, 2))), 
            hjust = -0.1, vjust = 1.2, inherit.aes = FALSE) +
  theme_classic() +
  theme(text = element_text(size = 15))+labs(x="Observed DSi (mg Si/L)", y="Predicted DSi (mg Si/L)")

dev.off()
