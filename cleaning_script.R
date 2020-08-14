##regressions with word counts
##combine them
class(merged_B$track_number)
class(merged_C$track_number)
merged_lyrics <- rbind(merged_B, merged_C, merged_S)
model <- lm(tempo ~ sums, data = merged_B)
summary(model)
model <- lm(tempo ~ sums, data = merged_S)
summary(model)

##bring them together

merged_2 <- merge(merged_lyrics, rhcp_albums)
ggplot(merged_2, aes(x = sums, y = tempo)) +
  geom_point(aes(color = album_name))
class(merged_2$sums)

model <- glm(mode_name ~ sums + tempo, data = merged_2, family = binomial)
summary(model)

ggplot(merged_2, aes(tempo, sums, label = merged_2$track_name)) + 
  geom_text(color = "red", alpha = .6) +
  facet_wrap(~album_name)

ggplot(rhcp_albums, aes(album_name, tempo)) +
  geom_boxplot()
ggplot(merged_2, aes(album_name, sums)) +
  geom_boxplot()
###overlay the word count with the tempo here

mean(merged_C$sums)
mean(merged_B$sums)
mean(merged_S$sums)


table <- merged_2 %>%
  select(album_name, track_name, sums) %>%
  arrange(album_name, sums)
bythewaytable <- table %>%
  filter(album_name == "By the Way")
bythewaytable$WordCount <- bythewaytable$sums
bythewaytable$AlbumName <- bythewaytable$album_name
bythewaytable$TrackName <- bythewaytable$track_name
bythewaytable <- bythewaytable %>%
  select(AlbumName, WordCount,TrackName)
bythewaytable %>%
  mutate(
    TrackName = cell_spec(TrackName, color = "white"
                          align = "c", angle = 45,
                          background = factor(TrackName, c(4,6,8),
                                              c("#666666", "#999999", "#BBBBBB")))
  ) %>%
  select(TrackName, WordCount) %>%
  kable()
library(dplyr)
mtcars
mtcars[1:10, 1:2] %>%
  mutate(
    car = row.names(.),
    mpg = cell_spec(mpg, "html", color = "purple"),
    cyl = cell_spec(cyl, "html", color = "white", align = "c", angle = 45, 
                    background = factor(cyl, c(4, 6, 8), 
                                        c("#666666", "#999999", "#BBBBBB")))
  ) %>%
  select(car, mpg, cyl) %>%
  kable(format = "html", escape = F) %>%
  kable_styling("striped", full_width = F)

bythewaytable %>%
  mutate(
    WordCount = cell_spec(WordCount, "html", color = "firebrick3"),
    TrackName = cell_spec(TrackName, "html", color = "red", align = "c",
                          angle = 0, background = c("#C0C0C0"))
  ) %>%
  select(WordCount, TrackName) %>%
  kable(format = "html", escape = F) %>%
  kable_styling("striped", full_width = F)

dt <- bythewaytable %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped",
                                      "condensed"),
                stripe_color = "red")
dt
mean(bythewaytable$WordCount)




table <- merged_2 %>%
  select(album_name, track_name, sums, tempo) %>%
  arrange(album_name, sums)
bythewaytable <- table %>%
  filter(album_name == "Stadium Arcadium")
bythewaytable$WordCount <- bythewaytable$sums
bythewaytable$AlbumName <- bythewaytable$album_name
bythewaytable$TrackName <- bythewaytable$track_name
bythewaytable$Tempo <- bythewaytable$tempo
bythewaytable <- bythewaytable %>%
  select(AlbumName, WordCount,TrackName, Tempo)
library(magrittr)
dt <- bythewaytable %>%
  kable() %>%
  kable_styling() %>%
  column_spec(c(2,4), bold = T) %>%
  row_spec(c(3,9), bold = T,color = "black", background = "red") %>%
  row_spec(c(13,14), bold = T, color = "red", background = "black")
dt

table <- merged_2 %>%
  select(album_name, track_name, sums, tempo) %>%
  arrange(album_name, sums)
bythewaytable <- table %>%
  filter(album_name == "Californication")
bythewaytable$WordCount <- bythewaytable$sums
bythewaytable$AlbumName <- bythewaytable$album_name
bythewaytable$TrackName <- bythewaytable$track_name
bythewaytable$Tempo <- bythewaytable$tempo
bythewaytable <- bythewaytable %>%
  select(AlbumName, WordCount,TrackName, Tempo)
library(magrittr)
dt <- bythewaytable %>%
  kable() %>%
  kable_styling() %>%
  column_spec(c(2,4), bold = T) %>%
  row_spec(c(4), bold = T,color = "black", background = "red") %>%
  row_spec(c(14,15), bold = T, color = "red", background = "black")
dt


mean(bythewaytable$WordCount)

install.packages("kableExtra")
library(kableExtra)


#1

quantiles <- rhcp %>%
  select(tempo, album_name) %>%
  group_by(album_name) 
dt <- quantile(rhcp$tempo)
dt
dt <- as.data.frame(dt)
dt$Tempo <- dt$dt
dt$Quartile <- c("0%", "25%",
                 "50%", "75%",
                 "100%")
dt <- dt[,2:3]
dt$Tempo <- round(dt$Tempo, 2)
dt
rownames(dt) <- c()
dt %>%
  kable() %>%
  kable_styling()
dt <- dt %>%
  kable() %>%
  kable_styling(stripe_color = "red") 
dt

#2
bythewaytable %>%
  mutate(
    WordCount = cell_spec(WordCount, "html", color = "firebrick3"),
    TrackName = cell_spec(TrackName, "html", color = "firebrick", align = "c",
                          angle = 0, background = c("light grey"))
  ) %>%
  select(WordCount, TrackName) %>%
  kable(format = "html", escape = F) %>%
  kable_styling("striped", full_width = F)


ggplot(rhcp_albums, aes(album_name, tempo)) +
  geom_boxplot(fill = c("firebrick", "black",
                        "firebrick", "black",
                        "firebrick", "black")) +
  xlab("Album Name") +
  ylab("Tempo")  +
  theme(legend.title = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(face = "bold", angle = 15,
                                   size = 14)) +
  theme(axis.title.y  = element_text(face = "bold", size = 14))


ggplot(rhcp_albums, aes(album_name, tempo, fill = album_name)) +
  geom_violin() +
  ylab("Tempo")  +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(face = "bold", angle = 15,
                                   size = 14)) +
  theme(axis.title.y  = element_text(face = "bold", size = 14)) +
  scale_fill_manual(values = c("#a50f15", "#de2d26",
                               "#fb6a4a", "#fc9272",
                               "#fcbba1", "#fee5d9")) +
  geom_boxplot(width = 0.09, fill = "black")


#3

ggplot(data, aes(x=track_name, y = tempo, label = tempo)) +
  geom_point( stat = 'identity', color = "red", size = 17, alpha = .4) +
  geom_segment(aes(y=0,
                   x = `track_name`,
                   yend = tempo,
                   xend = `track_name`),
               color = "yellow") +
  geom_text(color = "black", size = 3) +
  ylim(50,200) +
  coord_flip() +
  ylab("Tempo") +
  xlab("Track Name") +
  theme(axis.text.y = element_text(face = "bold", size = 14)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_text(size = 14))


#4

ggplot(merged_2, aes(tempo, sums, label = merged_2$track_name)) + 
  geom_text(color = "red", alpha = .6, size = 5) +
  facet_wrap(~album_name) +
  xlab("Tempo") +
  ylab("Word Count")+
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x.top = element_text(size = 16))


res.aov <- aov(tempo ~ album_name, data = rhcp_albums)
s1 <- summary(res.aov)
s1
install.packages("summ")
library(summ)
summ(s1)
install.packages("xtable")
library(xtable)
s1 <- xtable(s1)
xtable(res.aov) %>%
  kable()


duration <- merged_2 %>%
  select(duration_ms, track_name, sums) %>%
  mutate(secons = duration_ms/1000) %>%
  mutate(minute = secons/60) %>%
  mutate(wps = sums/minute)
