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



# LB R2
# eU (1576, 1568, 1563, 1570) vs. Units (1438, 1551, 1433, 1482)
eu_units_lb = Series_Simulation(eUnited, Denial)
eu_units_lb %>% extract2(3) # eU win w/ 72% Probability, eU 3-1: Units might take Map 2

# Heretics (1536, 1504, 1474, 1511) vs. Reciprocity (1559, 1539, 1470, 1533)
heretics_rec_lb = Series_Simulation(Heretics, Reciprocity)
heretics_rec_lb %>% extract2(3) # Rec win w/ 55%, Rec 3-1: Could be any of the maps

# Elevate (1465, 1543, 1429, 1489) vs. Splcye(1421, 1528, 1509, 1481)
elevate_splyce_lb = Series_Simulation(Elevate, Splyce)
elevate_splyce_lb %>% extract2(3) # Too Close (Splyce 53%): If Elevate do well in HP, they win

# FaZe (1623, 1443, 1562, 1539) vs. LG (1431, 1430, 1476, 1440)
faze_lg_lb = Series_Simulation(FaZe, Luminosity)
faze_lg_lb %>% extract2(3) # FaZe 74%, FaZe 3-1




# WB R2
# E6 (1529, 1528, 1558, 1534) vs. GenG (1569, 1528, 1583, 1555)
e6_geng_wb = Series_Simulation(Enigma6, Gen.G)
e6_geng_wb %>% extract2(3) # Gen.G 56%, GenG 3-1: SnD 50/50, GenG have slight edge in respawns

# OpTic (1643, 1621, 1584, 1622) vs. 100T (1759, 1565, 1636, 1657)
og_100t_wb = Series_Simulation(OpTic, Hundred_Thieves)
og_100t_wb %>% extract2(3) # 100T 59%, 100T 3-1: OpTic take SnD, if they force G5 they will win



# LB R3
# eUnited (1587, 1583, 1573, 1582) vs. Heretics (1539, 1508, 1489, 1517)
eu_heretics_lb = Series_Simulation(eUnited, Heretics)
eu_heretics_lb %>% extract2(3) # eU 67%, eU 3-1

# Splyce (1426, 1559, 1489, 1492) vs. FaZe (1638, 1426, 1574, 1541)
splyce_faze_lb = Series_Simulation(Splyce, FaZe)
splyce_faze_lb %>% extract2(3) # FaZe 62%, FaZe 3-1




# LB R4
# OpTic (1652, 1602, 1570, 1615) vs. eUnited (1612, 1596, 1553, 1594)
og_eu_lb = Series_Simulation(OpTic, eUnited)
og_eu_lb %>% extract2(3) # OpTic 56% 3-1: OpTic barely get the edge. Could go either way.

# E6 (1531, 1497, 1575, 1526) vs. FaZe (1645, 1448, 1586, 1555)
e6_faze_lb = Series_Simulation(Enigma6, FaZe)
e6_faze_lb %>% extract2(3) # FaZe 57% 3-1: FaZe SnD is questionable, but so is E6's



# LB R5
# OpTic (1650, 1618, 1585, 1624) vs. FaZe (166, 1466, 1602, 1569)
og_faze_lb = Series_Simulation(OpTic, FaZe)
og_faze_lb %>% extract2(3) # OpTic 64% 3-2


# WB R3
# 100T (1751, 1584, 1649, 1664) vs. GenG (1567, 1558, 1566, 1563)
t100_geng_wb = Series_Simulation(Hundred_Thieves, Gen.G)
t100_geng_wb %>% extract2(3) # 100T 74% 3-1: 100T is poised to go back-to-back



# LB R6
# GenG (1559, 1544, 1554, 1552) vs. OpTic (1620, 1636, 1602, 1623)
geng_og_lb = Series_Simulation(Gen.G, OpTic)
geng_og_lb %>% extract2(3) # OpTic 69%, 3-1




# GF
# 100T (1759, 1599, 1662, 1675) vs. GenG (1534, 1582, 1572, 1561)
t100_geng_gf = Series_Simulation(Hundred_Thieves, Gen.G)
t100_geng_gf %>% extract2(3)
