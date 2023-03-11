#_____________________________________________________________________
#current build

weapon_dice <- 10
num_wep_dice <- 2
rune_dice <- 6
num_rune_dice <- 1
perm_dmg <- 8
rage_buff <- 6
spell_buff <- 0

avg_wep_dmg <- (weapon_dice+1)/2*num_wep_dice + (rune_dice+1)/2*num_rune_dice
avg_total_dmg <- avg_wep_dmg + perm_dmg + rage_buff + spell_buff

strike <- c(0,0,avg_total_dmg, avg_total_dmg*2)
adv_ass <- c(0,2+num_wep_dice,avg_total_dmg+2+num_wep_dice,2*(avg_total_dmg+2+num_wep_dice))
certain_strike <- c(0,avg_total_dmg - avg_wep_dmg, avg_total_dmg, 2*avg_total_dmg)

sum(strike*hit_matrix["8",]) + sum(certain_strike*hit_matrix["13",]) + sum(certain_strike*hit_matrix["18",])
sum(strike*hit_matrix["6",]) + sum(certain_strike*hit_matrix["11",]) + sum(certain_strike*hit_matrix["16",])

#(sum(strike*hit_matrix["8",]) + sum(certain_strike*hit_matrix["13",]) + sum(strike*hit_matrix["8",]) + sum(certain_strike*hit_matrix["13",]) + sum(certain_strike*hit_matrix["18",]))/2
#(sum(strike*hit_matrix["6",]) + sum(certain_strike*hit_matrix["11",]) + sum(strike*hit_matrix["6",]) + sum(certain_strike*hit_matrix["11",]) + sum(certain_strike*hit_matrix["16",]))/2

#todo trip chance
#knockdown vs trip -> certain without flanking
(sum(strike*hit_matrix["8",])+ sum(hit_matrix["8",3:4])*(1/2)*sum(strike*hit_matrix["8",])) - ((1/2)*(sum(certain_strike*hit_matrix["11",]) + sum(strike*hit_matrix["8",])) + (1/2)*(sum(certain_strike*hit_matrix["13",])))
#knockdown vs trip -> certain with flanking
(sum(strike*hit_matrix["6",])+ sum(hit_matrix["6",3:4])*(1/2)*sum(strike*hit_matrix["6",])) - ((1/2)*(sum(certain_strike*hit_matrix["11",]) + sum(strike*hit_matrix["6",])) + (1/2)*(sum(certain_strike*hit_matrix["11",])))
#improved knockdown -> adv assault vs trip -> certain strike -> certain strike vs strike -> cs -> cs, no flanking
sum(strike*hit_matrix["8",]) + sum(hit_matrix["8",3:4])*((sum(strike*hit_matrix["8",])+(weapon_dice+1)/2) +sum(adv_ass*hit_matrix["11",])) + sum(hit_matrix["8",1:2])*sum(strike*hit_matrix["13",])
((1/2)*( sum(certain_strike*hit_matrix["11",])+ sum(certain_strike*hit_matrix["16",]) + sum(strike*hit_matrix["8",])) + (1/2)*(sum(certain_strike*hit_matrix["13",]) + sum(certain_strike*hit_matrix["18",])))
sum(strike*hit_matrix["8",]) + sum(certain_strike*hit_matrix["13",]) + sum(certain_strike*hit_matrix["18",])
#improved knockdown -> adv assault vs trip -> certain strike -> certain strike vs strike -> cs -> cs, flanking
sum(strike*hit_matrix["6",]) + sum(hit_matrix["6",3:4])*((sum(strike*hit_matrix["6",])+(weapon_dice+1)/2) +sum(adv_ass*hit_matrix["11",])) + sum(hit_matrix["6",1:2])*sum(strike*hit_matrix["11",])
((1/2)*( sum(certain_strike*hit_matrix["11",])+ sum(certain_strike*hit_matrix["16",]) + sum(strike*hit_matrix["6",])) + (1/2)*(sum(certain_strike*hit_matrix["11",]) + sum(certain_strike*hit_matrix["16",])))
sum(strike*hit_matrix["6",]) + sum(certain_strike*hit_matrix["11",]) + sum(certain_strike*hit_matrix["16",])


sum(certain_strike*hit_matrix["11",]) - sum(strike*hit_matrix["8",])
sum(certain_strike*hit_matrix["11",]) - sum(strike*hit_matrix["6",])

sum(certain_strike*hit_matrix["11",]) - sum(strike*hit_matrix["8",])
sum(certain_strike*hit_matrix["9",]) - sum(strike*hit_matrix["6",])










#_____________________________________________________________________
#weird build

weapon_dice <- 8
num_wep_dice <- 2
rune_dice <- 6
num_rune_dice <- 1
perm_dmg <- 8
rage_buff <- 6
spell_buff <- 10

avg_wep_dmg <- (weapon_dice+1)/2*num_wep_dice + (rune_dice+1)/2*num_rune_dice
avg_total_dmg <- avg_wep_dmg + perm_dmg + rage_buff + spell_buff

strike <- c(0,0,avg_total_dmg, avg_total_dmg*2)
adv_ass <- c(0,2+num_wep_dice,avg_total_dmg+2+num_wep_dice,2*(avg_total_dmg+2+num_wep_dice))
certain_strike <- c(0,avg_total_dmg - avg_wep_dmg, avg_total_dmg, 2*avg_total_dmg)


(sum(strike*hit_matrix["8",]) + sum(certain_strike*hit_matrix["13",]) + sum(strike*hit_matrix["8",]) + sum(certain_strike*hit_matrix["13",]) + sum(certain_strike*hit_matrix["18",]))/2
(sum(strike*hit_matrix["6",]) + sum(certain_strike*hit_matrix["11",]) + sum(strike*hit_matrix["6",]) + sum(certain_strike*hit_matrix["11",]) + sum(certain_strike*hit_matrix["16",]))/2



#########_______________________________________________________
hit_chances <- function(min_hit){
  
  critfail_die <- max(min(min_hit-10,21),1)
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

dice_range <- -35:35
hit_matrix <- matrix(nrow=length(dice_range), ncol = 4)
rownames(hit_matrix) <- dice_range


for (i in dice_range) {
  hit_matrix[rownames(hit_matrix)==i,] <- hit_chances(i)
}
