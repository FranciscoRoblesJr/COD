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




# WB R1
# OpTic (1643, 1636, 1567, 1625) vs. FaZe (1622, 1427, 1578, 1536)
og_faze_wb = Series_Simulation(OpTic, FaZe)
og_faze_wb %>% extract(3) # OpTic 72%, OpTic 3-2: FaZe able to take respawns to push to G5, but cant close

# 100T (1748, 1584, 1628, 1659) vs. Elevate(1476, 1524, 1437, 1488)
t100_elevate_wb = Series_Simulation(Hundred_Thieves, Elevate)
t100_elevate_wb %>% extract2(3) # 100T 86%, 100T 3-0: Elevate drew short end of stick playing 100T R1

# E6 (1530, 1493, 1579, 1525) vs. Heretics (1534, 1538, 1452, 1520)
e6_heretics_wb = Series_Simulation(Enigma6, Heretics)
e6_heretics_wb %>% extract2(3) # Too close (50/50): If E6 does well in HP's they win. If not, they lose

# eUnited (1578, 1587, 1579, 1582) vs. GenG (1567, 1508, 1567, 1543)
eu_geng_wb = Series_Simulation(eUnited, Gen.G)
eu_geng_wb %>% extract2(3) # eU 60%, eU 3-2: Tight series, eU has slight edge in SnD


# LB R1
# Midnight (1394, 1499, 1444, 1446) vs. Units (1423, 1536, 1415, 1467)
mid_units_lb = Series_Simulation(Midnight, Denial)
mid_units_lb %>% extract2(3) # Units 56%, Units 3-1: Midnight win Control

# Reciprocity (1546, 1531, 1454, 1521) vs. EnVy (1486, 1350, 1460, 1427)
rec_nv_lb = Series_Simulation(Reciprocity, EnVy)
rec_nv_lb %>% extract2(3) # Rec 73%, Rec 3-2: nV win control and steal a HP, maybe?

# EG (1490, 1512, 1464, 1493) vs. Splyce (1413, 1528, 1494, 1475)
eg_splyce_lb = Series_Simulation(Evil_Geniuses, Splyce)
eg_splyce_lb %>% extract2(3) # Too close (EG 54%): Splyce in 5 or EG take the series in 3 or 4

# LG (1401, 1429, 1493, 1430) vs. UYU (1403, 1426, 1476, 1427)
lg_uyu_lb = Series_Simulation(Luminosity, UYU)
lg_uyu_lb %>% extract2(3) # Too close (50/50): System is essentially saying every map is coinflip
