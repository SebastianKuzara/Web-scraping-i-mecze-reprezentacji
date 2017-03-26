library(dplyr)

## Obiekt result jest wynikiem wykonania kodu z pliku 'web scraping.R'
source(file = "web scraping.R",encoding = "utf-8")

## Liczba meczów w poszczególnych rozgrywkach
table(results$rozgrywki)

## Liczba meczów rozegranych w poszczególnych miastach
miasto <- sort(table(results$miejsce), decreasing = TRUE)

## Przeciwnicy i liczba meczów
sort(table(results$rywal), decreasing = TRUE)

## Wygrane-remisy-porażki
c(wygrane = sum(results$gole_zdobyte>results$gole_stracone),
  remisy = sum(results$gole_zdobyte==results$gole_stracone),
  przegrane = sum(results$gole_zdobyte<results$gole_stracone))

## wygrane-remisy-porażki w podziale na rodzaj rozgrywek
rr <- results %>%
  group_by(rozgrywki) %>%
  summarise(
    wygrane = sum(gole_zdobyte > gole_stracone),
    remisy = sum(gole_zdobyte == gole_stracone),
    przegrane = sum(gole_zdobyte < gole_stracone)
  ) %>%
  as.data.frame()

rr <- rbind(rr, c("Razem", colSums(rr[2:4])))
rr[,2:4] <- apply(rr[,2:4], 2, as.numeric)
rr <- data.frame(cbind(rr, c(rowSums(rr[,2:4]))))
names(rr)[5] <- "Razem"
rr

## Mecze w eliminacjach do EURO
results %>%
  filter(rozgrywki=="el.ME") %>%
  select(data, gospodarze, goscie)

## Mecze rozgrywane w marcu
marzec <- results %>%
  filter(miesiac=="03") %>%
  select(data, rozgrywki, gospodarze, goscie, wynik)
marzec

## Mecze o punkty rozgrywane w marcu
marzec_punkty <- results %>%
  filter(miesiac=="03", rozgrywki!="tow.") %>%
  select(data, rozgrywki, gospodarze, goscie, wynik)
marzec_punkty

## Wygrane-remisy-porażki w meczach o punkty rozgrywanych w marcu
results %>%
  filter(miesiac=="03", rozgrywki!="tow.") %>%
  summarise(
    wygrane = sum(gole_zdobyte > gole_stracone),
    remisy = sum(gole_zdobyte == gole_stracone),
    przegrane = sum(gole_zdobyte < gole_stracone)
  )

