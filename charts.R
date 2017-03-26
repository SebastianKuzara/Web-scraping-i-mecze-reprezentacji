library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)

## Obiekt result jest wynikiem wykonania kodu z pliku 'web scraping.R'
 source(file = "web scraping.R",encoding = "utf-8")

## Wszystkie mecze o punkty
results %>%
  filter(rozgrywki!="tow.") %>%
ggplot(aes(x = data, y = gole_zdobyte-gole_stracone, size = gole_zdobyte)) +
  geom_hline(yintercept = 0, color = "#333333", size = 0.75) +
  geom_point(color = "#ee6a50") +
  scale_size_continuous(breaks = c(0, 1, 2, 3, 5, 10)) +
  geom_text_repel(aes(label = rywal), size = 3) +
  geom_smooth(method = "loess", se = FALSE, size = 0.5, aes(color = "#2c57a6"), show.legend = TRUE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, aes(color = "#008000"), show.legend = TRUE) +
  labs(x = "Data", y = "Różnica bramek") +
  guides(size = guide_legend(title = "Gole zdobyte przez\nrep. Polski", override.aes = list(linetype = 0)), 
         color = guide_legend(title="Trend")) +
  scale_color_manual(values = c("#2c57a6", "#008000"), breaks = c("#2c57a6", "#008000"), labels = c("loess", "liniowy")) +
  theme_light() +
  theme(panel.border = element_blank(),
        legend.key = element_blank())

## Tylko teoretycznie mocni rywale
results %>%
  filter(rozgrywki!="tow.", !(rywal%in%c("San Marino", "Gibraltar", "Gruzja", "Mołdawia", "Kazachstan", "Armenia"))) %>%
ggplot(aes(x = data, y = gole_zdobyte-gole_stracone, size = gole_zdobyte)) +
  geom_hline(yintercept = 0, color = "#333333", size = 0.75) +
  geom_point(color = "#ee6a50") +
  scale_size_continuous(breaks = c(0, 1, 2, 3)) +
  geom_text_repel(aes(label = rywal), size = 3) +
  geom_smooth(method = "loess", se = FALSE, size = 0.5, aes(color = "#2c57a6"), show.legend = TRUE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, aes(color = "#008000"), show.legend = TRUE) +
  labs(x = "Data", y = "Różnica bramek") +
  guides(size = guide_legend(title = "Gole zdobyte przez\nrep. Polski", override.aes = list(linetype = 0)), 
         color = guide_legend(title="Trend")) +
  scale_color_manual(values = c("#2c57a6", "#008000"), breaks = c("#2c57a6", "#008000"), labels = c("loess", "liniowy")) +
  theme_light() +
  theme(panel.border = element_blank(),
        legend.key = element_blank())

## Dodatkowo tylko mecze wyjazdowe
results %>%
  filter(!(rozgrywki%in%c("tow.", "f.ME")), 
         !(rywal%in%c("San Marino", "Gibraltar", "Gruzja", "Mołdawia", "Kazachstan", "Armenia")),
         results$gospodarze!="Polska") %>%
ggplot(aes(x = data, y = gole_zdobyte-gole_stracone, size = gole_zdobyte)) +
  geom_hline(yintercept = 0, color = "#333333", size = 0.75) +
  geom_point(color = "#ee6a50") +
  scale_size_continuous(breaks = c(0, 1, 2, 3)) +
  geom_text_repel(aes(label = rywal), size = 3) +
  geom_smooth(method = "loess", se = FALSE, size = 0.5, aes(color = "#2c57a6"), show.legend = TRUE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, aes(color = "#008000"), show.legend = TRUE) +
  labs(x = "Data - początek roku", y = "Różnica bramek") +
  guides(size = guide_legend(title = "Gole zdobyte przez\nrep. Polski", override.aes = list(linetype = 0)), 
         color = guide_legend(title="Trend")) +
  scale_color_manual(values = c("#2c57a6", "#008000"), breaks = c("#2c57a6", "#008000"), labels = c("loess", "liniowy")) +
  theme_light() +
  theme(panel.border = element_blank(),
        legend.key = element_blank())

## Bilans bramek w meczach z poszczególnymi rywalami
results %>%
  filter(rozgrywki!="tow.") %>%
  filter(!(rywal%in%c("San Marino", "Gibraltar", "Gruzja", "Mołdawia", "Kazachstan", "Armenia"))) %>%
  group_by(rywal) %>%
  summarise(
    zdobyte = sum(gole_zdobyte),
    stracone = sum(gole_stracone),
    liczba_meczy = n()
  ) %>%
  mutate(liczba_meczy = as.factor(liczba_meczy)) %>%
  as.data.frame() %>%
  ggplot(aes(x = rywal, y = zdobyte-stracone, color = liczba_meczy)) +
  geom_hline(yintercept = 0, size = 0.2, color = "#555555") +
  geom_point(size = 2) +
  scale_color_manual(values = c( "#bfcce4" ,"#809ac9"  ,"#5678b7", "#1e3c74")) +
  coord_flip() +
  scale_y_continuous(breaks = -3:3) +
  labs(y = "Bilans bramek", x = "Przeciwnik") +
  guides(color = guide_legend(title = "Liczba meczy")) +
  theme_light() +
  theme(
    legend.key = element_blank(),
    panel.border = element_blank()
  )


## Odsetek zwycięstw w poszczególnych latach
results %>%
  group_by(rok) %>%
  summarise(
    zwyciestwa = sum(gole_zdobyte>gole_stracone),
    mecze = n(),
    proc_zw = round(zwyciestwa/mecze, 3),
    proc_niezw = 1-proc_zw
  ) %>%
  as.data.frame() %>%
  gather(key=wynik, value = proc, proc_zw:proc_niezw) %>%
  ggplot(aes(x = rok, y = proc, fill = wynik)) +
  geom_bar(stat = "identity",  position = "stack") +
  geom_hline(yintercept = 0.5, color = "#666666", linetype = 1, size = 0.5) +
  scale_fill_manual(values = c("#f9f9f9", "#a8a8a8")) +
  ylim(c(0,1)) +
  labs(x = "Rok", y = "Odsetek zwyciętstw") +
  theme_light() +
  theme(legend.position = "none",
        panel.border = element_blank())


