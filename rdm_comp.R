adept <- FALSE

# Fighter/Thalia DMG ----
bow_dmg <- matrix(data = 0,nrow = 20,ncol = 4)
bow_dmg[,3] <-
  weapon_dice * 4.5 +
  d6_runes * 3.5 +
  fighter_wepspec +
  c(rep(1,9), rep(2,11))
bow_dmg[,4] <-
  bow_dmg[,3] * 2 +
  sapply(weapon_dice, FUN = function(n){max(n-1,1)*5.5})

fighter_spread <- full_spread("Fighter")

bow_av_dmg_vanilla <- dmg_table(fighter_spread, bow_dmg) + dmg_table(fighter_spread + 5, bow_dmg) + dmg_table(fighter_spread + 10, bow_dmg)
bow_av_dmg_double  <- dmg_table(fighter_spread + 2, bow_dmg) + dmg_table(fighter_spread + 2, bow_dmg) + dmg_table(fighter_spread + 10, bow_dmg)
bow_av_dmg_triple <- dmg_table(fighter_spread + 4, bow_dmg) + dmg_table(fighter_spread + 4, bow_dmg) + dmg_table(fighter_spread + 4, bow_dmg)
bow_av_dmg_multi <- dmg_table(fighter_spread + 2, bow_dmg) + dmg_table(fighter_spread + 2, bow_dmg) + dmg_table(fighter_spread + 2, bow_dmg)

bow_dpr <- cbind(bow_av_dmg_vanilla[,1:3], bow_av_dmg_double[,4:5], bow_av_dmg_triple[,6:15], bow_av_dmg_multi[,16:20])

sliced_bow_dpr <- moderate_slice(bow_dpr)
load(file = "thalia_slice_file")

# RDM Gross DMG ----

d6_runes_rdm <- d6_runes
d6_runes_rdm[2:8] <- 1

verfire_gross <- matrix(data = 0,nrow = 20,ncol = 4)
verfire_gross[,2] <-
  weapon_dice * 3.5 +
  d6_runes_rdm * 3.5 +
  main_stat
verfire_gross[,1] <-
  verfire_gross[,2] * 2
verfire_gross[,3] <-
  verfire_gross[,2]/2
verfire_gross[,2] <-
  verfire_gross[,2] + floor((1:20)/4)*3.5
verfire_gross[,1] <-
  verfire_gross[,1] + floor((1:20)/4)*7
verfire_gross[1:11,] <- 0

scatter_gross <- matrix(data = 0,nrow = 20,ncol = 4)
scatter_gross[1:6,2] <-
  (weapon_dice * 2 +
     d6_runes_rdm * 3.5)[1:6]
scatter_gross[7:20,2] <-
  (weapon_dice * 2.5 +
     d6_runes_rdm * 3.5)[7:20]
scatter_gross[,1] <-
  scatter_gross[,2] * 2
scatter_gross[,3] <-
  scatter_gross[,2]/2
scatter_gross[1:3,] <- 0

verthundara_gross <- matrix(data = 0,nrow = 20,ncol = 4)
verthundara_gross[,2] <-
  weapon_dice * 4 +
  d6_runes_rdm * 3.5 +
  main_stat
verthundara_gross[,1] <-
  verthundara_gross[,2] * 2
verthundara_gross[,3] <-
  verthundara_gross[,2]/2
verthundara_gross[1:4,] <- 0

#adept
if(adept)
{
  veraerora_gross <- matrix(data = 0,nrow = 20,ncol = 4)
  veraerora_gross[1:8,2] <-
    (weapon_dice * 2 +
       d6_runes_rdm * 3.5)[1:8] +
    main_stat[1:8]
  veraerora_gross[9:20,2] <-
    (weapon_dice * 2.5 +
       d6_runes_rdm * 3.5)[9:20] +
    main_stat[9:12]
  veraerora_gross[,1] <-
    veraerora_gross[,2] * 2
  veraerora_gross[,3] <-
    veraerora_gross[,2]/2
  veraerora_gross[,2] <-
    veraerora_gross[,2] + floor((1:20)/4)*3.5
  veraerora_gross[,1] <-
    veraerora_gross[,1] + floor((1:20)/4)*7
  veraerora_gross[1:4,] <- 0
}


#non-adept
if(!adept)
{
  veraerora_gross <- matrix(data = 0,nrow = 20,ncol = 4)
  veraerora_gross[1:8,2] <-
    (weapon_dice * 2.5 +
       d6_runes_rdm * 3.5 +
       main_stat)[1:8]
  veraerora_gross[9:20,2] <-
    (weapon_dice * 3.5 +
       d6_runes_rdm * 3.5 +
       main_stat)[9:20]
  veraerora_gross[,1] <-
    veraerora_gross[,2] * 2
  veraerora_gross[,3] <-
    veraerora_gross[,2]/2
  veraerora_gross[1:4,] <- 0
}

riposte_gross <- matrix(data = 0,nrow = 20,ncol = 4)
riposte_gross[,3] <-
  weapon_dice * 4.5 +
  main_stat
riposte_gross[,2] <-
  main_stat
riposte_gross[,4] <-
  riposte_gross[,3] +
  sapply(weapon_dice, FUN = function(n){max(n-1,1)*4.5})

riposte_map_gross <- matrix(data = 0,nrow = 20,ncol = 4)
riposte_map_gross[,3] <-
  weapon_dice * 4.5 +
  main_stat
riposte_map_gross[,4] <-
  riposte_map_gross[,3] +
  sapply(weapon_dice, FUN = function(n){max(n-1,1)*4.5})

jolt_gross <- matrix(data = 0,nrow = 20,ncol = 4)
jolt_gross[1:8,3] <-
  (weapon_dice * 2 +
     d6_runes_rdm * 3.5)[1:8] 
jolt_gross[9:20,3] <-
  (weapon_dice * 2.5 +
     d6_runes_rdm * 3.5)[9:20] 
jolt_gross[,4] <-
  jolt_gross[,3] * 2

adept_9 <- matrix(data = 0,nrow = 20,ncol = 4)
adept_9[,] <- (c(rep(4,9), rep(5,10), 6) +
  c(rep(0,16), rep(1,4)))*2
adept_9[1:8,] <- 0

adept_13_save <- matrix(data = 0,nrow = 20,ncol = 4)
adept_13_save[,1] <- 2
adept_13_save[,2] <- 1
adept_13_save[,3] <- .5
adept_13_save[1:12,] <- 0

adept_13_attack <- matrix(data = 0,nrow = 20,ncol = 4)
adept_13_attack[,3] <- 1
adept_13_attack[,4] <- 2
adept_13_attack[1:12,] <- 0

# RDM Attack Sequences -----

rdm_spread <- full_spread("Full_Martial")

fst <- save_dmg_table(rdm_spread, verfire_gross) + save_dmg_table(rdm_spread, adept_13_save)*9 + 
  save_dmg_table(rdm_spread, scatter_gross) + 
  save_dmg_table(rdm_spread, verthundara_gross) +
  save_dmg_table(rdm_spread, adept_9)
sliced_fst <- moderate_slice_save(fst)

#--

jftj_attacks <-
  (dmg_table(rdm_spread,jolt_gross) * 2) + 
  (dmg_table(rdm_spread,adept_13_attack) * 5) +
  dmg_table(rdm_spread, adept_9)
jftj_saves <- save_dmg_table(rdm_spread, verfire_gross) +
  save_dmg_table(rdm_spread, verthundara_gross)
sliced_jftj <- moderate_slice(jftj_attacks) + moderate_slice_save(jftj_saves)

#--

jaj_attacks <- 
  (dmg_table(rdm_spread,jolt_gross) * 2) + 
  (dmg_table(rdm_spread,adept_13_attack) * 5) +
  dmg_table(rdm_spread, adept_9)
jaj_saves <- save_dmg_table(rdm_spread, veraerora_gross)
sliced_jaj <- moderate_slice(jaj_attacks) + moderate_slice_save(jaj_saves)

#--

jsr_attacks <- 
  dmg_table(rdm_spread,jolt_gross) +
  dmg_table(rdm_spread, riposte_gross)
jsr_saves <- 
  save_dmg_table(rdm_spread,scatter_gross)
sliced_jsr <- moderate_slice(jsr_attacks) + moderate_slice_save(jsr_saves)  

#--

ja_attacks <- dmg_table(rdm_spread,jolt_gross)
ja_saves <- save_dmg_table(rdm_spread, veraerora_gross)
sliced_ja <- moderate_slice(ja_attacks) + moderate_slice_save(ja_saves)

# Comparisons -----


jftj_fighter_diff <- round(sliced_jftj - sliced_bow_dpr, 1)[,12:20]
jaj_fighter_diff <- round(sliced_jaj - sliced_bow_dpr, 1)[,12:20]
jsr_figher_diff <- round(sliced_jsr - sliced_bow_dpr, 1)[,1:7]
ja_figher_diff <- round(sliced_ja - sliced_bow_dpr, 1)[,1:7]

jftj_fighter_percent <- round((sliced_jftj - sliced_bow_dpr)/sliced_bow_dpr, 2)[,12:20] * 100
jaj_fighter_percent <- round((sliced_jaj - sliced_bow_dpr)/sliced_bow_dpr, 2)[,12:20] * 100
jsr_fighter_percent <- round((sliced_jsr - sliced_bow_dpr)/sliced_bow_dpr, 2)[,1:7] * 100
ja_fighter_percent <- round((sliced_ja - sliced_bow_dpr)/sliced_bow_dpr, 2)[,1:7] * 100

# heatmap.2(
#   jftj_fighter_percent, 
#   cellnote = jftj_fighter_percent, 
#   dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none', col = rev(hcl.colors(71, palette = "Vik")), symbreaks = TRUE, xlab = "Level", ylab = "Defense compared to Moderate for level", main = "(JFTJ - Fighter)/Fighter %")
# 
# heatmap.2(
#   jaj_fighter_percent, 
#   cellnote = jaj_fighter_percent,
#   dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none', col = rev(hcl.colors(71, palette = "Vik")), symbreaks = TRUE, xlab = "Level", ylab = "Defense compared to Moderate for level", main = "(JAJ - Fighter)/Fighter %")

heatmap.2(
  jsr_fighter_percent, 
  cellnote = jsr_fighter_percent,
  dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none', col = rev(hcl.colors(71, palette = "Vik")), symbreaks = TRUE, xlab = "Level", ylab = "Defense compared to Moderate for level", main = "(JSR - Fighter)/Fighter %")

heatmap.2(
  ja_fighter_percent, 
  cellnote = ja_fighter_percent,
  dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none', col = rev(hcl.colors(71, palette = "Vik")), symbreaks = TRUE, xlab = "Level", ylab = "Defense compared to Moderate for level", main = "(JA - Fighter)/Fighter %")
