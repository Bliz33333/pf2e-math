library(pacman)
p_load(tidyverse, rio, tidytext, reshape2, stats, gplots)
options(digits = 4)
# Import ------

ac_table <- tibble(read.csv("ac.csv"))
saves_table <- tibble(read.csv("saves.csv"))
level_dc_table <- tibble(read.csv("level_dc.csv"))

main_stat <-   c(rep(4,9), rep(5,10), 6) + c(rep(0,16), rep(1,4))

skill_types <- c("Level","Full_Focus","Assurance")
player_skill <- (matrix(nrow = 20, ncol = length(skill_types)))
colnames(player_skill) <- skill_types
player_skill[,"Level"] <- 1:20
player_skill[,"Full_Focus"] <-
  (1:20) +
  c(rep(2,2), rep(4,4), rep(6,8), rep(8,6)) +
  main_stat +
  c(rep(0,2), rep(1,6), rep(2,8), rep(3,4))
player_skill[,"Assurance"] <-
  (1:20) +
  c(rep(2,2), rep(4,4), rep(6,8), rep(8,6))
player_skill <- as_tibble(player_skill)
  
  
attack_types <- c("Level", "Fighter", "Full_Martial", "Off_Martial", "Caster", "Caster_Buffed", "Wildshape_Own", "Wildshape_Spell","Companion_Nimble_FA")
player_attack <- (matrix(nrow = 20, ncol = length(attack_types)))
colnames(player_attack) <- attack_types
player_attack[,"Level"] <- 1:20
player_attack[,"Fighter"] <- 
  c(rep(4,4), rep(6,8), rep(8,8)) +
  c(rep(4,9), rep(5,10), 6) +
  c(rep(0,16), rep(1,4)) +
  c(0,rep(1,8), rep(2,6), rep(3,5)) +
  (1:20)
player_attack[,"Full_Martial"] <- (player_attack[,"Fighter"] -2)
player_attack[,"Off_Martial"] <- 
  player_attack[,"Full_Martial"] - 
  c(rep(1,4), rep(0, 5), rep(1, 5), rep(0,5), 1)
player_attack[,"Caster"] <- 
  c(rep(2,6), rep(4,8), rep(6,4), rep(8,2)) +
  c(rep(4,9), rep(5,10), 6) +
  c(rep(0,16), rep(1,4)) +
  (1:20)
player_attack[,"Caster_Buffed"] <- 
  c(rep(2,6), rep(4,8), rep(6,4), rep(8,2)) +
  c(rep(4,9), rep(5,10), 6) +
  c(rep(0,16), rep(1,4)) +
  c(0,rep(1,8), rep(2,6), rep(3,5)) +
  (1:20)
player_attack[,"Wildshape_Own"] <-
  c(rep(2,10),rep(4,10)) +
  c(rep(3,4), rep(4,10), rep(5,6)) +
  c(0,rep(1,8), rep(2,6), rep(3,5)) +
  (1:20) +
  rep(2,20)
player_attack[,"Wildshape_Spell"] <-
 c(0,0,9,9,14,14,16,16,18,18,23,23,25,25,28,28,31,31,34,34)
player_attack[,"Companion_Nimble_FA"] <-
 c(1:20) +
 c(rep(2,13),rep(4,7)) +
 c(rep(3,3),rep(4,4),rep(6,6), rep(9,7))
player_attack <- as_tibble(player_attack)

d6_runes <- c(rep(0,8),rep(1,1), rep(2,6), rep(3,5))
weapon_dice <- c(rep(1,3),rep(2,8), rep(3,7), rep(4,2))
companion_dice <- c(rep(1,3), rep(2,10), rep(3,7))
companion_bonus <- c(rep(0,7), rep(2,6), rep(4,7))
companion_dex_fa_strmod <- c(rep(2,3), rep(3,4), rep(4,8), rep(5,5))
martial_wepspec <- c(rep(0,6),rep(2,6), rep(3,2), rep(6,6))
fighter_wepspec <- c(rep(0,6),rep(3,6), rep(4,2), rep(8,6))

# Basic -------------------------------------


moderate_hit <- (-1*sweep(as.matrix(player_attack[,-1]), 1, as.matrix(ac_table[ac_table$Level %in% 1:20,"Moderate"]))) %>%
  as_tibble() %>%
  cbind(player_attack$Level, .)

colnames(moderate_hit)[1] <- "Level"

# moderate_hit %>%
#   group_by(Level)

moderate_hit %>%
  select(Level, Full_Martial, Wildshape_Own, Wildshape_Spell) %>%
  melt(id.vars = "Level") %>%
  ggplot(aes(Level, value, colour = variable)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = c(5,6,7,8,9,10,11,12,15,20),limits = c(1,20))

  # ylim(1,20)

  

# Hit Algorithm -----------------------------
hit_chances <- function(min_hit){
  
  critfail_die <- max(min(min_hit-9,21),1)
  fail_die <-  max(min(min_hit,21),1)
  hit_die <-  max(min(min_hit-1,20),0)
  crithit_die <-  max(min(min_hit+9,20),0)
  
  critfail <- (critfail_die-1)/20
  fail <- (fail_die - critfail_die)/20
  hit <- (crithit_die - hit_die)/20
  crithit <- (20-crithit_die)/20
  
  if(critfail>0)
  {
    
  } else if(fail > 0){
    critfail <- 0.05
    fail <- fail - .05
  } else if(hit > 0)
  {
    fail <- 0.05
    hit <- hit - .05
  } else {
    hit <- 0.05
    crithit <- crithit - .05
  }
  
  if(crithit>0)
  {
    
  } else if(hit > 0){
    crithit <- 0.05
    hit <- hit - .05
  } else if(fail > 0)
  {
    hit <- 0.05
    fail <- fail - .05
  } else {
    fail <- 0.05
    critfail <- critfail - .05
  }
  

  return(c(critfail,fail,hit,crithit))
}

dice_range <- -99:99
hit_matrix <- matrix(nrow=length(dice_range), ncol = 5)
rownames(hit_matrix) <- dice_range


for (i in dice_range) {
  hit_matrix[rownames(hit_matrix)==i,c(2:5)] <- hit_chances(i)
}

hit_matrix[,1] <- dice_range
colnames(hit_matrix) <- c("difference","crit_fail", "fail", "success", "crit_success")

hit_matrix <- as.data.frame(hit_matrix)


# Functions ------------------------------------------------

level_spread_slice <- function(level, attacker = "Full_Martial", spread = 5)
{
  
  temp_ac <- ac_table %>% 
    filter(Level == level) %>% 
    select(Moderate) %>% 
    unlist
  
  AC <- c((temp_ac-spread):(temp_ac+spread))
  
  temp_hit <- player_attack %>% 
    filter(Level == level) %>% 
    select(.data[[attacker]]) %>% 
    unlist
  
  temp <-
    AC - temp_hit
  
  temp_3 <-
    hit_matrix %>% 
    filter(difference %in% temp) %>% 
    cbind(AC,.) %>% 
    select(!difference)
  
  return(temp_3)
}

level_spread <- function(level, attacker = "Full_Martial")
{
  

  AC <- c(1:70)
  
  temp_hit <- player_attack %>% 
    filter(Level == level) %>% 
    select(.data[[attacker]]) %>% 
    unlist
  
  temp <-
    AC - temp_hit
  
  # temp_3 <-
  #   hit_matrix %>% 
  #   filter(difference %in% temp) %>% 
  #   cbind(AC,.) %>% 
  #   select(!difference)
  
  return(temp)
}

full_spread <- function(att="Full_Martial")
{
  spread <- matrix(data = 0, nrow=70,ncol = 20)
  
  for (i in 1:20) {
    spread[,i] <- level_spread(level = i, attacker = att)
  }
  
  return(spread)
  
}

load(file = "sub_11_file")
load(file = "super_11_file")
load(file = "mas2_file")
load(file = "mas3_file")

precision <- function(chances_table, level = 1, order = -1)
{
  
  #chances_table <- c(8,13,18)
  
  if(is.null(dim(chances_table)))
  {
    ntr <- chances_table
    
    chances_table <- matrix(data = 0, nrow = length(ntr), ncol = 4)
    chances_table <- data.frame(chances_table)
    
    colnames(chances_table) <- c("crit_fail", "fail", "success", "crit_success")
    
    for (i in 1:length(ntr)) {
      chances_table[i,] <- hit_chances(ntr[i])
    }
  }
  
  num_attacks <- nrow(chances_table)
  
  if(order[1] == -1)
  {
    order <- (1:num_attacks)
  }
  
  if(!(num_attacks %in% (1:4)))
  {
    return(-1)
  }
  
  dmg_list <- (0)
  
  if(level < 11)
  {
    dmg_list <- sub_11
  } else if(level < 17)
  {
    dmg_list <- super_11
  } else if(level < 19)
  {
    dmg_list <- mas2
  } else {
    dmg_list <- mas3
  }
  
  chances_table %>% 
    arrange(order)
  
  hits <- t(matrix(data = c(1,0,0), nrow = 3, ncol = 4))
  
  
  
  for (i in 1:num_attacks) {
    hits[i,] <- 
      chances_table[i,] %>%
      mutate(miss = fail + crit_fail) %>% 
      select(miss, success, crit_success) %>% 
      unlist()
  }
  
  
  # hits[1,]
  # hits[2,]
  d2 <- t(t(array((rep(hits[1,],3)),c(3,3)))  * (hits[2,]))
  # d2
  
  
  # hits[3,]
  # d2
  d3 <- ((array((rep(d2,3)),c(3,3,3)))  * c(rep(hits[3,1],9),rep(hits[3,2],9),rep(hits[3,3],9)))
  # d3
  
  # hits[4,]
  # d3
  d4 <- ((array((rep(d3,3)),c(3,3,3,3)))  * c(rep(hits[4,1],27),rep(hits[4,2],27),rep(hits[4,3],27)))
  # d4
  
  # sum(array(d4,81)*dmg_list)
  # sum((d4)*dmg_list)
  
  
  return(sum(d4*dmg_list))
  
}

ac <- function(level, diff = "Moderate", adjustment = 0)
{
  (ac_table %>% 
    filter(Level == level) %>% 
    select(.data[[diff]]) %>% 
    unlist() -
    adjustment) %>% 
    return()
}

low <- function(l, adj = 0)
{
  return(ac(level = l,diff = "Low", adjustment = adj))
}

moderate <- function(l, adj = 0)
{
  return(ac(level = l,diff = "Moderate", adjustment = adj))
}

moderate_save <- function(l, adj = 0)
{
  return(saves_table$Moderate[saves_table$Level == l] + adj)
}

high <- function(l, adj = 0)
{
  return(ac(level = l,diff = "High", adjustment = adj))
}

extreme <- function(l, adj = 0)
{
  return(ac(level = l,diff = "Extreme", adjustment = adj))
}

dmg_table <- function(spread, dmg)
{
  dtab <- matrix(data = 0, ncol = ncol(spread), nrow= nrow(spread))
  
  for (i in 1:ncol(spread)) {
    dtab[,i]<-sapply(spread[,i], function(n){sum(hit_chances(n) * dmg[i,])})
  }
 
  return(dtab) 
}

#spread -> table of ntr of attack vs row index ac
#want ntr of save of row index vs 10 + attack
save_dmg_table <- function(spread, dmg)
{
  dtab <- matrix(data = 0, ncol = ncol(spread), nrow= nrow(spread))
  
  spread <- (spread*-1)+10
  
  for (i in 1:ncol(spread)) {
    dtab[,i]<-sapply(spread[,i], function(n){sum(hit_chances(n) * dmg[i,])})
  }

  return(dtab)
}

precision_table <- function(spread, attack_mods)
{
  tab <- matrix(data = 0, nrow = nrow(spread), ncol = ncol(spread))
  
  # for (i in 1:nrow(tab))
  # {
  #   for(j in 1:ncol(tab))
  #   {
  #     tab[i,j] <- precision((-attack_mods + spread[i,j]), level = j)
  #   }
  # }
  
  

  for(j in 1:ncol(tab))
  {
    tab[,j] <- sapply(spread[,j], function(n,att,lev){precision((-att + n), level = lev)}, att = attack_mods, lev = j)
  }
  
  return(tab)
}

moderate_slice <- function(spread, width = 5)
{
  tab <- matrix(data = 0, ncol = ncol(spread), nrow = 2*width+1)
  
  for (i in 1:ncol(spread)) {
    tab[,i] <- spread[(moderate(i)-width):(moderate(i)+5),i]
  }
  
  rownames(tab) <- (-width:width)
  colnames(tab) <- (1:20)
  
  return(tab)
  
}

moderate_slice_save <- function(spread, width = 5)
{
  tab <- matrix(data = 0, ncol = ncol(spread), nrow = 2*width+1)
  
  for (i in 1:ncol(spread)) {
    tab[,i] <- spread[(moderate_save(i)-width):(moderate_save(i)+5),i]
  }
  
  rownames(tab) <- (-width:width)
  colnames(tab) <- (1:20)
  
  return(tab)
  
}

#precompute -------

# base3 <- function(num)
# {
#   if(num == 0)
#   {
#     return(c(0))
#   }
#   
#   vec <- (integer(ceiling(logb(num,3))))
#   
#   count <- 1
#   while (num>0) 
#   {
#     vec[count] <- num %% 3
#     num <- num %/% 3
#     count <- count +1
#   }
#   
#   return(vec)
# }
# 
# hits <- lapply(c(0:80), base3)
# hits <- lapply(hits,function(list){list[list!=0]})
# onedice <- lapply(hits, first) %>% unlist()
# onedice[is.na(onedice)] <- 0
# 
# sub_11 <- onedice*4.5
# super_11 <- onedice*9
# 
# mas2_hits <- lapply(hits, function(n){head(c(n,0,0),2)})
# mas2 <- lapply(mas2_hits, function(n){sum(n*c(9,4.5))}) %>% unlist()
# 
# mas3_hits <- lapply(hits, function(n){head(c(n,0,0,0),3)})
# mas3 <- lapply(mas3_hits, function(n){sum(n*c(13.5,9,4.5))}) %>% unlist()
# 
# save(sub_11, file = "sub_11_file")
# save(super_11, file = "super_11_file")
# save(mas2, file = "mas2_file")
# save(mas3, file = "mas3_file")