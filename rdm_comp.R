
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
