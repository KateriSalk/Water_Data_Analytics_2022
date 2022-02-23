LAGOSMN <- LAGOSjoined %>%
  left_join(., LAGOSdata$hu4) %>%
  select(lagoslakeid, sampledate, state_name, hu4, chla, tn, tp) %>%
  mutate(Month = month(sampledate),
         Year = year(sampledate)) %>%
  filter(state_name == "Minnesota") %>%
  filter(Month %in% c(6:9)) %>%
  filter(Year >= 1990) %>%
  filter(hu4 %in% c("0401", "0701", "0702", "0703", "0704", "0902"))

ggplot(LAGOSMN, aes(x = tp, y = chla, color = hu4)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(vars(hu4)) +
  scale_x_log10() +
  scale_y_log10()

MNmodel <- lm(data = LAGOSMN, log10(chla) ~ log10(tp) * hu4)
summary(MNmodel)
par(mfrow = c(2,2))
plot(MNmodel)