bow_hit <- 4*4.5 + 3*3.5 + 2 + 8
bow_crit <- bow_hit * 2 + 3*5.5

gun_hit <- 4*4.5 + 3*3.5 + 1 + 8
gun_crit <- 10*6.5 + 6*3.5 + 2 + 16

to_hit_gun <- player_attack$Fighter[20] + 6
to_hit_iterative <- player_attack$Fighter[20] - 5
to_hit_bow <- player_attack$Fighter[20] - 2

ac <- ac_table$Moderate[20:24]

diffs_gun <- ac-to_hit_gun
diffs_bow <- ac-to_hit_bow
diffs_iter <- ac- to_hit_iterative

chances_gun <- 
  hit_matrix %>% 
  filter(difference %in% diffs_gun) %>% 
  select(!difference)

chances_bow <- 
  hit_matrix %>% 
  filter(difference %in% diffs_bow) %>% 
  select(!difference)


dpr_bow <- 
  t(t(chances_bow[,3:4]) * c(bow_hit, bow_crit)) %>% 
  rowSums() * 3

dpr_gun <- 
  t(t(chances_gun[,3:4]) * c(gun_hit, gun_crit)) %>% 
  rowSums()


chances_iter <- 
  hit_matrix %>% 
  filter(difference %in% diffs_iter) %>% 
  select(!difference)

dpr_bow_focus <- 
  t(t(chances_gun[,3:4]) * c(bow_hit, bow_crit)) %>% 
  rowSums()

dpr_iter <- 
  t(t(chances_iter[,3:4]) * c(bow_hit, bow_crit)) %>% 
  rowSums() 

dpr_bow_snipe <- dpr_bow_focus + dpr_iter