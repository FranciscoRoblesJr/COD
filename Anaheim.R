# Pool A
# wElo: OG - 1617, Heretics - 1501, EG - 1494, ex-Denial - 1493
# HP: OG - 1624, SnD: OG - 1653, Control: OG - 1530
og_heretics_poolA = Series_Simulation(OpTic, Heretics)
og_heretics_poolA %>% extract2(3)
og_eg_poolA = Series_Simulation(OpTic, Evil_Geniuses)
og_eg_poolA %>% extract2(3)
og_denial_poolA = Series_Simulation(OpTic, Denial)
og_denial_poolA %>% extract2(3)
heretics_eg_poolA = Series_Simulation(Heretics, Evil_Geniuses)
heretics_eg_poolA %>% extract2(3)
heretics_denial_poolA = Series_Simulation(Heretics, Denial)
heretics_denial_poolA %>% extract2(3)
eg_denial_poolA = Series_Simulation(Evil_Geniuses, Denial)
eg_denial_poolA %>% extract2(3)

# Pool B
# wElo: eU - 1591, Rec - 1513, Elevate - 1478, UYU - 1436
# HP: Rec - 1557, SnD: eU - 1636, Control: eU - 1580
eu_rec_poolB = Series_Simulation(eUnited, Reciprocity)
eu_rec_poolB %>% extract2(3)
eu_elevate_poolB = Series_Simulation(eUnited, Elevate)
eu_elevate_poolB %>% extract2(3)
eu_uyu_poolB = Series_Simulation(eUnited, UYU)
eu_uyu_poolB %>% extract2(3)
rec_elevate_poolB = Series_Simulation(Reciprocity, Elevate)
rec_elevate_poolB %>% extract2(3)
rec_uyu_poolB = Series_Simulation(Reciprocity, UYU)
rec_uyu_poolB %>% extract2(3)
elevate_uyu_poolB = Series_Simulation(Elevate, UYU)
elevate_uyu_poolB %>% extract2(3)

# Pool C
# wElo: FaZe - 1545, Splyce - 1510, Enigma6 - 1485, Midnight - 1442
# HP: FaZe - 1613, SnD: Splyce - 1518, Control - FaZe - 1608
faze_splyce_poolC = Series_Simulation(FaZe, Splyce)
faze_splyce_poolC %>% extract2(3)
faze_e6_poolC = Series_Simulation(FaZe, Enigma6)
faze_e6_poolC %>% extract2(3)
faze_mid_poolC = Series_Simulation(FaZe, Midnight)
faze_mid_poolC %>% extract2(3)
splyce_e6_poolC = Series_Simulation(Splyce, Enigma6)
splyce_e6_poolC %>% extract2(3)
splyce_mid_poolC = Series_Simulation(Splyce, Midnight)
splyce_mid_poolC %>% extract2(3)
e6_mid_poolC = Series_Simulation(Enigma6, Midnight)
e6_mid_poolC %>% extract2(3)

# Pool D
# wElo: 100T - 1633, GenG - 1528, nV - 1465, LG - 1432
# HP: 100T - 1718, SnD: 100T - 1551, Control - 1629
t100_geng_poolD = Series_Simulation(Hundred_Thieves, Gen.G)
t100_geng_poolD %>% extract2(3)
t100_nv_poolD = Series_Simulation(Hundred_Thieves, EnVy)
t100_nv_poolD %>% extract2(3)
t100_lg_poolD = Series_Simulation(Hundred_Thieves, Luminosity)
t100_lg_poolD %>% extract2(3)
geng_nv_poolD = Series_Simulation(Gen.G, EnVy)
geng_nv_poolD %>% extract2(3)
geng_lg_poolD = Series_Simulation(Gen.G, Luminosity)
geng_lg_poolD %>% extract2(3)
nv_lg_poolD = Series_Simulation(EnVy, Luminosity)
nv_lg_poolD %>% extract2(3)
