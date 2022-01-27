#########################################################
#########################################################

getwd()

mbta <- read_excel("Sexual_Abuse.xlsx",sheet = "T1")
str(mbta)
table(mbta$Study_Group)

Child = mbta %>% filter (T1_Age_Group == 0)
Child_victim = Child %>% filter (Study_Group == 1)
Child_comp = Child %>% filter (Study_Group == 2)
Child_comp = Child_comp %>% filter (T1relat_Code == 2)
str(Child_comp)

Adolescent = mbta %>% filter (T1_Age_Group == 1)
Adolescent_victim = Adolescent  %>% filter (Study_Group == 1)
Adolescent_comp = Adolescent  %>% filter (Study_Group == 2)
Adolescent_comp = Adolescent_comp  %>% filter (T1relat_Code == 2)
str(Adolescent_comp)

Adult = mbta %>% filter (T1_Age_Group == 2)
Adult_victim = Adult  %>% filter (Study_Group == 1)
Adult_comp = Adult  %>% filter (Study_Group == 2)
str(Adult_victim)


####Age Comparison
summary(Child_victim$T1_Age)
sd(Child_victim$T1_Age)
summary(Child_comp$T1_Age)
sd(Child_comp$T1_Age)
t.test(Child_victim$T1_Age, Child_comp$T1_Age, var.equal = FALSE)

summary(Adolescent_victim$T1_Age)
sd(Adolescent_victim$T1_Age)
summary(Adolescent_comp$T1_Age)
sd(Adolescent_comp$T1_Age)
t.test(Adolescent_victim$T1_Age, Adolescent_comp$T1_Age, var.equal = FALSE)

Adult_victim$T1_Age [Adult_victim$T1_Age == -99] <- NA
summary(Adult_victim$T1_Age)
sd(Adult_victim$T1_Age, na.rm = T)
summary(Adult_comp$T1_Age)
sd(Adult_comp$T1_Age)
t.test(Adult_victim$T1_Age, Adult_comp$T1_Age, var.equal = FALSE)

####Parental education level
#victim
Child_ma_vic = Child_victim$최종학력_모_1차
Child_ma_vic = as.numeric(Child_ma_vic)
Child_ma_vic[Child_ma_vic == -99] <- NA
Child_ma_vic [Child_ma_vic <= 7] <- 0
Child_ma_vic [Child_ma_vic > 7 ] <- 1
table(Child_ma_vic)

Adol_ma_vic = Adolescent_victim$최종학력_모_1차
Adol_ma_vic = as.numeric(Adol_ma_vic)
Adol_ma_vic [Adol_ma_vic == -99] <- NA
Adol_ma_vic [Adol_ma_vic <= 7 | Adol_ma_vic > 11] <- 0
Adol_ma_vic [Adol_ma_vic > 7 & Adol_ma_vic < 11] <- 1
table(Adol_ma_vic)

Adult_vim_edu = Adult_victim$`최종학력 (본인)_1차`
Adult_vim_edu = as.numeric(Adult_vim_edu)
Adult_vim_edu [Adult_vim_edu == -99] <- NA
Adult_vim_edu [Adult_vim_edu <= 7 | Adult_vim_edu > 11] <- 0
Adult_vim_edu [Adult_vim_edu > 7 & Adult_vim_edu < 11] <- 1
table(Adult_vim_edu)

Child_fam_vic = Child_victim$동거가족_1차
Child_fam_vic = as.numeric(Child_fam_vic)
Child_fam_vic [Child_fam_vic == -99] <- NA
Child_fam_vic [Child_fam_vic == "3,4"] <- 0
Child_fam_vic [Child_fam_vic > 7 ] <- 1
table(Child_fam_vic)

length(grep("3,4" , Child_fam_vic))
length(grep("3, 4," , Child_fam_vic))


#comp
Child_ma_comp = Child_comp$최종학력_모_1차
Child_ma_comp = as.numeric(Child_ma_comp)
Child_ma_comp[Child_ma_comp == -99] <- NA
Child_ma_comp [Child_ma_comp <= 7 | Child_ma_comp > 11] <- 0
Child_ma_comp [Child_ma_comp > 7 & Child_ma_comp < 11] <- 1
table(Child_ma_comp)

Adol_ma_comp = Adolescent_comp$최종학력_모_1차
Adol_ma_comp = as.numeric(Adol_ma_comp)
Adol_ma_comp[Adol_ma_comp == -99] <- NA
Adol_ma_comp [Adol_ma_comp <= 7 | Adol_ma_comp >= 11] <- 0
Adol_ma_comp [Adol_ma_comp > 7 & Adol_ma_comp < 11] <- 1
table(Adol_ma_comp)

Adult_comp_edu = Adult_comp$`최종학력 (본인)_1차`
Adult_comp_edu = as.numeric(Adult_comp_edu)
Adult_comp_edu [Adult_comp_edu == -99] <- NA
Adult_comp_edu [Adult_comp_edu <= 7 | Adult_comp_edu >= 11] <- 0
Adult_comp_edu [Adult_comp_edu > 7 & Adult_comp_edu < 11] <- 1
table(Adult_comp_edu)

#French Skiers Example - Child
Skiers <- matrix(c(23,22,20,133), nrow = 2, byrow = TRUE)
Skiers
chisq.test(Skiers, correct = FALSE)

#French Skiers Example - Adol
Skiers <- matrix(c(43,76,19,105), nrow = 2, byrow = TRUE)
Skiers
chisq.test(Skiers, correct = FALSE)

#French Skiers Example - Adult
Skiers <- matrix(c(100,64,92,50), nrow = 2, byrow = TRUE)
Skiers
chisq.test(Skiers, correct = FALSE)





age_dm$age_cat <- ifelse(age_dm$age <40, 1, 
                         ifelse(age_dm$age >=40 & age_dm$age <50, 2,
                                ifelse(age_dm$age>=50 & age_dm$age <60 , 3, 4)))

mbta_child_yes_v = mbta_child_yes  %>% filter (`Study_Group(1=victim)` == 1)
mbta_child_yes_c = mbta_child_yes  %>% filter (`Study_Group(1=victim)` == 2)
mbta_adolescent_yes_v = mbta_adolescet_yes  %>% filter (`Study_Group(1=victim)` == 1)
mbta_adolescent_yes_c = mbta_adolescet_yes %>% filter (`Study_Group(1=victim)` == 2)
mbta_adult_yes_v = mbta_adult_yes %>% filter (`Study_Group(1=victim)` == 1)
mbta_adult_yes_c = mbta_adult_yes %>% filter (`Study_Group(1=victim)` == 2)


str_count <- function(string, pattern = "3,4") {
  check_lengths(string, pattern)
  
  switch(type(pattern),
         empty = ,
         bound = stri_count_boundaries(string, opts_brkiter = opts(pattern)),
         fixed = stri_count_fixed(string, pattern, opts_fixed = opts(pattern)),
         coll  = stri_count_coll(string, pattern, opts_collator = opts(pattern)),
         regex = stri_count_regex(string, pattern, opts_regex = opts(pattern))
  )
}

