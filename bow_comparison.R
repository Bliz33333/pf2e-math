
#old definitions--------------
# cantrip_sps <- c(0,0,2*4.5+3+4*4.5+3,(2*4.5+3+4*4.5+3)*2 + 5.5)
# focus_sps <- c(0,0,2*4.5+3+7*4.5+3,(2*4.5+3+7*4.5+3)*2 + 5.5)
#ranger_strike <- c(0,0,12,29.5)
#ac_strike <- c(0,0,15,30)
#ac_nimble <- c(0,0,13,26)



# cantrip_sps ----------
cantrip_sps <- matrix(data = 0,nrow = 20,ncol = 4)
cantrip_sps[,3] <-
  weapon_dice * 4.5 +
  d6_runes * 3.5 +
  fighter_wepspec +
  c(rep(2,4), rep(3,5), rep(4,10), 5) +
  ceiling(c(1:20)/2) * 4.5
cantrip_sps[,4] <-
  cantrip_sps[,3] * 2 +
  sapply(weapon_dice, FUN = function(n){max(n-1,1)*5.5})

# focus_sps ------------
focus_sps <- matrix(data = 0,nrow = 20,ncol = 4)
focus_sps[,3] <-
  cantrip_sps[,3] +
  (ceiling(c(1:20)/2) -1) * 4.5
focus_sps[,4] <-
  cantrip_sps[,4] +
  (ceiling(c(1:20)/2) -1) * 9

# ranger_strike-----------
ranger_strike <- matrix(data = 0,nrow = 20,ncol = 4)
ranger_strike[,3] <-
  weapon_dice * 4.5 +
  d6_runes * 3.5 +
  martial_wepspec +
  c(rep(1,9), rep(2,11))
ranger_strike[,4] <-
  ranger_strike[,3] * 2 +
  sapply(weapon_dice, FUN = function(n){max(n-1,1)*5.5})

# ac_strike/nimble-------------
ac_strike <- matrix(data = 0,nrow = 20,ncol = 4)
ac_nimble <- matrix(data = 0,nrow = 20,ncol = 4)
ac_strike[,3] <-
  companion_dice * 4.5 +
  companion_bonus +
  companion_dex_fa_strmod
ac_strike[,4] <-
  ac_strike[,3] *2
ac_nimble[,3] <-
  ac_strike[,3] - companion_dice
ac_nimble[,4] <-
  ac_nimble[,3] *2








# math--------------
fighter_spread <- full_spread("Fighter")
ranger_spread <- full_spread()
companion_spread <- full_spread("Companion_Nimble_FA")

cantrip_dmg <- dmg_table(fighter_spread, cantrip_sps)
focus_dmg <- dmg_table(fighter_spread, focus_sps)

ranger_dmg <- dmg_table(ranger_spread, ranger_strike) + dmg_table(ranger_spread + 5, ranger_strike) + precision_table(ranger_spread,c(0,-5))
companion_dmg <- dmg_table(companion_spread, ac_strike) + dmg_table(companion_spread + 4, ac_nimble) + precision_table(companion_spread,c(0,-4))
total_dmg <- ranger_dmg + companion_dmg

thalia_slice <- moderate_slice(total_dmg)
ranger_slice <- moderate_slice(ranger_dmg)
companion_slice <- moderate_slice(companion_dmg)
cantrip_slice <- moderate_slice(cantrip_dmg)
focus_slice <-moderate_slice(focus_dmg)

cantrip_diff <- (cantrip_slice-thalia_slice)[,-(1:7)]
focus_diff <- (focus_slice-thalia_slice)[,-(1:7)]
thalia_diff <- (ranger_slice-companion_slice)[,-(1:7)]

heatmap.2(cantrip_diff, dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none', col = rev(hcl.colors(71, palette = "Vik")), symbreaks = TRUE)
heatmap.2(focus_diff, dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none', col = rev(hcl.colors(71, palette = "Vik")), symbreaks = TRUE)
heatmap.2(thalia_diff, dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none', col = rev(hcl.colors(71, palette = "Vik")), symbreaks = TRUE)

View(thalia_diff/ranger_slice[,-(1:7)] * 100)

fighter_chances_sequence <-
moderate(8) %>% 
  {filter(fighter_spread, AC == .)} %>% 
  select(!AC)

(fighter_chances_sequence %>% 
    t() *
    cantrip_sps) %>% 
  t() %>% 
  sum()

(fighter_chances_sequence %>% 
    t() *
    focus_sps) %>% 
  t() %>% 
  sum()

ranger_chances_sequence <-
  ac_table %>% 
  filter(Level == 8) %>% 
  select(Moderate) %>% 
  unlist() %>% 
  {filter(ranger_spread, AC == . | AC == (.+5))} %>% 
  select(!AC)

(ranger_chances_sequence %>% 
  t() *
  ranger_strike) %>% 
  t() %>% 
  sum() +
  precision(ranger_chances_sequence, level = 8)

companion_chances_sequence <-
  ac_table %>% 
  filter(Level == 8) %>% 
  select(Moderate) %>% 
  unlist() %>% 
  {filter(companion_spread, AC == . | AC == (.+4))} %>% 
  select(!AC)

(companion_chances_sequence %>% 
    t() *
    c(ac_strike, ac_nimble)) %>% 
  t() %>% 
  sum() +
  precision(companion_chances_sequence, level = 8)
