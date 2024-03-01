library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(nflreadr)
library(dplyr)
reg_szn <- load_pbp(seasons=1999:2023)|>
  filter(pass==1,season_type=="REG",!is.na(epa))|>
  group_by(passer_player_id)|>
  summarize(name=first(name),team=first(posteam),epa_pass_reg=mean(epa))
playoffs <- load_pbp(seasons=1999:2023)|>
  filter(pass==1,season_type=="POST",!is.na(epa))|>
  group_by(passer_player_id)|>
  summarize(epa_pass_post=mean(epa),playoff_passes=n())|>
  filter(playoff_passes>=150)
total_data <- left_join(reg_szn,playoffs,by=c("passer_player_id"))|>
  filter(!is.na(epa_pass_post),!is.na(passer_player_id))|>
  mutate(epa_difference = epa_pass_post-epa_pass_reg)
total_data <- left_join(total_data,teams_colors_logos,by=c("team"="team_abbr"))
total_data|>
  ggplot(aes(x=epa_pass_reg,y=epa_pass_post))+
  geom_point(aes(fill=team_color,color=team_color2),shape=21,alpha=0.9)+
  scale_color_identity(aesthetics = c("fill","color"))+
  geom_text_repel(aes(label=paste(name)))+
  theme_bw()+
  geom_abline(slope=1,intercept=0,linetype="dashed")+
  labs(x="Regular Season EPA Per Dropback",
       y = "Postseason EPA Per Dropback",
       title="QB EPA Per Dropback in the Postseason and the Regular Season",
       subtitle="Since 1999 | Minimum of 150 Playoff Dropbacks",
       caption = "By Aariv Iyengar | @AarivAnalytics")+
  theme(panel.grid.major.y = element_blank(),
        plot.title=element_text(size=22,hjust=0.5,face="bold"),
        plot.subtitle=element_text(size=16,hjust=0.5))+
  annotate("text",x=-0.07,y=0.1,label="Playoff Riser",fontface="bold",size=7)+
  annotate("text",x=0.2,y=-0.3,label="Playoff Dropper",fontface="bold",size=7)
ggsave("epa_playoff.png",width=14,height=10,dpi="retina")
total_data <- total_data|>
  arrange(-epa_difference)|>
  mutate(rank=row_number())
playoff_risers <- total_data|>
  slice_max(epa_difference,n=8)
playoff_droppers <- total_data|>
  slice_min(epa_difference,n=8)
playoff_gt <- rbind(playoff_risers,playoff_droppers)|>
  arrange(-epa_difference)|>
  select(rank,name,team_wordmark,epa_pass_reg,epa_pass_post,epa_difference)|>
  mutate(epa_pass_reg=round(epa_pass_reg,2),
         epa_pass_post=round(epa_pass_post,2),
         epa_difference=round(epa_difference,2))|>
  gt()|>
  cols_align(align="center")|>
  gt_img_rows(team_wordmark)|>
  cols_label(rank="Rank",
             name="Quarterback",
             epa_pass_reg="Regular Season EPA per Dropback",
             epa_pass_post="Postseason EPA Per Dropback",
             epa_difference="Postseason EPA Difference",
             team_wordmark="")|>
  tab_header(title = "Playoff Risers and Droppers at Quarterback",subtitle="Since 1999")|>
  gt_theme_538()|>
  gt_hulk_col_numeric(columns=epa_difference)
gtsave(playoff_gt,"playoff_gt.png")

