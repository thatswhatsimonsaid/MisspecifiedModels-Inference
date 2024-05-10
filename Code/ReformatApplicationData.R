ReformatApplicationDataFunction = function(dat){
  
  ### Summary: Reformats Sachs and Warner (1997) data
  ### Input: Sachs and Warner (1997) data
  ### Output: Sachs and Warner (1997) data reformatted in the form of Abadie, Imbens, and Zheng (2014).
  dat = dat %>% 
    select(c(COUNTRY,OPEN6590,TROPICS,LIFE,ICRGE80,CGB7090,LGDPEA65,GEAP_POP,SXP,LIFE2,ACCESS,GR6590)) %>%
    rename(name = COUNTRY,
           Y = GR6590,
           open = OPEN6590,
           tropics = TROPICS,
           life = LIFE,
           inst = ICRGE80,
           cgb = CGB7090,
           gdp65 = LGDPEA65,
           dpop = GEAP_POP,
           sxp = SXP,
           life2 = LIFE2,
           land = ACCESS) %>%
    mutate(life = exp(life),
           life2 = life^2) %>%
    relocate(Y, .after = name) %>%
    relocate(gdp65, .after = Y) %>%
    relocate(open, .after = gdp65) %>%
    relocate(dpop, .after = open) %>%
    relocate(cgb, .after = dpop) %>%
    relocate(inst, .after = cgb) %>%
    relocate(tropics, .after = inst) %>%
    relocate(land, .after = tropics) %>%
    relocate(sxp, .after = land) %>%
    relocate(life, .after = sxp)
  dat = na.omit(dat)
  rownames(dat) = 1:nrow(dat)
  
  return(dat)
}