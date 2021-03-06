Player.List = readRDS('Objects/Player.List_PostAnaheim')
Team.List = readRDS('Objects/Team.List_PostAnaheim')
source('Functions/Elo_Functions.R')
Team.List$Gen.G$Players[5] = 'Nagafen'

#### Pro League Week 11 - Week 12 ####
Series('Data/Pro League/PL - 145_units_uyu.csv', '2019-06-24')
Series('Data/Pro League/PL - 146_e6_lg.csv', '2019-06-24')
Series('Data/Pro League/PL - 147_faze_splyce.csv', '2019-06-24')
Series('Data/Pro League/PL - 148_100t_eg.csv', '2019-06-24')
Series('Data/Pro League/PL - 149_e6_uyu.csv', '2019-06-25')
Series('Data/Pro League/PL - 150_lg_units.csv', '2019-06-25')
Series('Data/Pro League/PL - 151_100t_faze.csv', '2019-06-25')
Series('Data/Pro League/PL - 152_eg_splyce.csv', '2019-06-25')
Series('Data/Pro League/PL - 153_e6_faze.csv', '2019-06-26')
Series('Data/Pro League/PL - 154_eg_units.csv', '2019-06-26')
Series('Data/Pro League/PL - 155_splyce_uyu.csv', '2019-06-26')
Series('Data/Pro League/PL - 156_100t_lg.csv', '2019-06-26')
Series('Data/Pro League/PL - 157_faze_units.csv', '2019-06-27')
Series('Data/Pro League/PL - 158_e6_eg.csv', '2019-06-27')
Series('Data/Pro League/PL - 159_100t_uyu.csv', '2019-06-27')
Series('Data/Pro League/PL - 160_lg_splyce.csv', '2019-06-27')
Series('Data/Pro League/PL - 161_elevate_mid.csv', '2019-07-01')
Series('Data/Pro League/PL - 162_eu_rec.csv', '2019-07-01')
Series('Data/Pro League/PL - 163_heretics_og.csv', '2019-07-01')
Series('Data/Pro League/PL - 164_geng_nv.csv', '2019-07-01')
Series('Data/Pro League/PL - 165_heretics_mid.csv', '2019-07-02')
Series('Data/Pro League/PL - 166_elevate_og.csv', '2019-07-02')
Series('Data/Pro League/PL - 167_rec_nv.csv', '2019-07-02')
Series('Data/Pro League/PL - 168_eu_geng.csv', '2019-07-02')
Series('Data/Pro League/PL - 169_elevate_geng.csv', '2019-07-03')
Series('Data/Pro League/PL - 170_heretics_rec.csv', '2019-07-03')
Series('Data/Pro League/PL - 171_eu_mid.csv', '2019-07-03')
Series('Data/Pro League/PL - 172_og_nv.csv', '2019-07-03')
Series('Data/Pro League/PL - 173_elevate_rec.csv', '2019-07-05')
Series('Data/Pro League/PL - 174_mid_nv.csv', '2019-07-05')
Series('Data/Pro League/PL - 175_geng_heretics.csv', '2019-07-05')
Series('Data/Pro League/PL - 176_eu_og.csv', '2019-07-05')

Series('Data/Miami/Miami - 1_units_lg.csv', '2019-07-19')
Series('Data/Miami/Miami - 2_mid_elevate.csv', '2019-07-19')
Series('Data/Miami/Miami - 3_eg_e6.csv', '2019-07-19')
Series('Data/Miami/Miami - 4_nv_uyu.csv', '2019-07-19')
Series('Data/Miami/Miami - 5_uyu_eg.csv', '2019-07-19')
Series('Data/Miami/Miami - 6_mid_lg.csv', '2019-07-19')
Series('Data/Miami/Miami - 7_heretics_lg.csv', '2019-07-19')
Series('Data/Miami/Miami - 8_rec_eg.csv', '2019-07-19')
Series('Data/Miami/Miami - 9_geng_splyce.csv', '2019-07-20')
Series('Data/Miami/Miami - 10_100t_faze.csv', '2019-07-20')
Series('Data/Miami/Miami - 11_eu_rec.csv', '2019-07-20')
Series('Data/Miami/Miami - 12_lg_og.csv', '2019-07-20')
Series('Data/Miami/Miami - 13_splyce_heretics.csv', '2019-07-20')
Series('Data/Miami/Miami - 14_100t_eg.csv', '2019-07-20')
Series('Data/Miami/Miami - 15_rec_splyce.csv', '2019-07-20')
Series('Data/Miami/Miami - 16_og_100t.csv', '2019-07-20')
Series('Data/Miami/Miami - 17_lg_faze.csv', '2019-07-20')
Series('Data/Miami/Miami - 18_eu_geng.csv', '2019-07-20')
Series('Data/Miami/Miami - 19_100t_geng.csv', '2019-07-20')
Series('Data/Miami/Miami - 20_lg_rec.csv', '2019-07-20')
Series('Data/Miami/Miami - 21_geng_rec.csv', '2019-07-21')
Series('Data/Miami/Miami - 22_eu_faze.csv', '2019-07-21')
Series('Data/Miami/Miami - 23_faze_geng.csv', '2019-07-21')
Series('Data/Miami/Miami - 24_eu_geng.csv', '2019-07-21')
Series('Data/Miami/Miami - 25_eu_geng.csv', '2019-07-21')

Player.List[['QwiKeR']] = Create_Player(QwiKeR)
Player.List[['Nastie']] = Create_Player(Nastie)
Player.List[['Chain']] = Create_Player(Chain)
Create_Team(Celtic, c('QwiKeR', 'Seany', 'Bance', 'Nastie', 'Chain'))
Team.List$Celtic$Logo.ID = '[](#celtic)'
Player.List[['Joshh']] = Create_Player(Joshh)
Player.List[['Dqvee']] = Create_Player(Dqvee)
Player.List[['Defrag']] = Create_Player(Defrag)
Player.List[['Peatie']] = Create_Player(Peatie)
Player.List[['Vortex']] = Create_Player(Vortex)
Create_Team(WaR, c('Joshh', 'Dqvee', 'Defrag', 'Peatie', 'Vortex'))
Team.List$WaR$Logo.ID = '[](#war)'
Player.List[['mosh']] = Create_Player(mosh)
Player.List[['Apox']] = Create_Player(Apox)
Create_Team(Mazer, c('mosh', 'Apox', 'SpaceLy', 'Parzelion', 'Jetli'))
Team.List$Mazer$Logo.ID = '[](#mazer)'
Player.List[['Cells']] = Create_Player(Cells)
Player.List[['GRVTY']] = Create_Player(GRVTY)
Player.List[['Hollow']] = Create_Player(Hollow)
Player.List[['Vivid']] = Create_Player(Vivid)
Player.List[['Wrecks']] = Create_Player(Wrecks)
Create_Team(Sage, c('Cells', 'GRVTY', 'Hollow', 'Vivid', 'Wrecks'))
Team.List$Sage$Logo.ID = '[](#sage)'
Player.List[['detain']] = Create_Player(detain)
Player.List[['Insight']] = Create_Player(Insight)
Player.List[['Bidz']] = Create_Player(Bidz)
Player.List[['Keza']] = Create_Player(Keza)
Player.List[['CleanX']] = Create_Player(CleanX)
Create_Team(Singularity, c('detain', 'Insight', 'Bidz', 'Keza', 'CleanX'))
Team.List$Singularity$Logo.ID = '[](#singularity)'
Player.List[['Exceed']] = Create_Player(Exceed)
Player.List[['Linney']] = Create_Player(Linney)
Create_Team(Sicario, c('Tommey', 'Chino', 'Proto', 'Exceed', 'Linney'))
Team.List$Sicario$Logo.ID = '[](#sicario)'
Player.List[['Charullz']] = Create_Player(Charullz)
Player.List[['Destiny']] = Create_Player(Destiny)
Player.List[['Newbz']] = Create_Player(Newbz)
Player.List[['Vicious']] = Create_Player(Vicious)
Player.List[['Pentagrxm']] = Create_Player(Pentagrxm)
Create_Team(Carnage, c('Charullz', 'Destiny', 'Newbz', 'Vicious', 'Pentagrxm'))
Team.List$Carnage$Logo.ID = '[](#carnage)'
Player.List[['Turnup2ez']] = Create_Player(Turnup2ez)
Player.List[['TISCH47']] = Create_Player(TISCH47)
Player.List[['Super']] = Create_Player(Super)
Create_Team(Aspire, c('Fastballa', 'RobbieB3319', 'Turnup2ez', 'TISCH47', 'Super'))
Team.List$Aspire$Logo.ID = '[](#aspire)'
Player.List[['Reaper']] = Create_Player(Reaper)
Player.List[['Deelo']] = Create_Player(Deelo)
Player.List[['Owakening']] = Create_Player(Owakening)
Player.List[['Landxn']] = Create_Player(Landxn)
Player.List[['Willett']] = Create_Player(Willett)
Create_Team(Vanity, c('Reaper', 'Deelo', 'Owakening', 'Landxn', 'Willett'))
Team.List$Vanity$Logo.ID = '[](#vanity)'
Player.List[['Teddyrecks']] = Create_Player(Teddyrecks)
Player.List[['DraMa']] = Create_Player(DraMa)
Player.List[['Ramby']] = Create_Player(Ramby)
Player.List[['GloFrosty']] = Create_Player(GloFrosty)
Player.List[['Demise']] = Create_Player(Demise)
Create_Team(Fury, c('Teddyrecks', 'DraMa', 'Ramby', 'GloFrosty', 'Demise'))
Team.List$Fury$Logo.ID = '[](#fury)'
Player.List[['Stamino']] = Create_Player(Stamino)
Player.List[['Spoof']] = Create_Player(Spoof)
Player.List[['Pandur']] = Create_Player(Pandur)
Player.List[['Jintroid']] = Create_Player(Jintroid)
Create_Team(Hybrid, c('Stamino', 'Spoof', 'Pandur', 'Beehzy', 'Jintroid'))
Team.List$Hybrid$Logo.ID = '[](#hybrid)'
Player.List[['Creza']] = Create_Player(Creza)
Player.List[['Yako']] = Create_Player(Yako)
Create_Team(TrainHard, c('Zeeked', 'Natshay', 'Creza', 'Yako', 'Rizk'))
Team.List$TrainHard$Logo.ID = '[](#trainhard)'
Player.List[['Glory']] = Create_Player(Glory)
Player.List[['Gunsiii']] = Create_Player(Gunsiii)
Player.List[['TooReal']] = Create_Player(TooReal)
Player.List[['KlinK']] = Create_Player(KlinK)
Player.List[['HumanJesus']] = Create_Player(HumanJesus)
Create_Team(RBL, c('Glory', 'Gunsiii', 'TooReal', 'KlinK', 'HumanJesus'))
Team.List$RBL$Logo.ID = '[](#rbl)'
Player.List[['Cookie']] = Create_Player(Cookie)
Player.List[['Phantom']] = Create_Player(Phantom)
Player.List[['Piero']] = Create_Player(Piero)
Player.List[['Sharko']] = Create_Player(Sharko)
Player.List[['Pazy']] = Create_Player(Pazy)
Create_Team(Animosity, c('Cookie', 'Phantom', 'Piero', 'Sharko', 'Pazy'))
Team.List$Animosity$Logo.ID = '[](#animosity)'
Player.List[['Conor']] = Create_Player(Conor)
Player.List[['Chaaxter']] = Create_Player(Chaaxter)
Player.List[['Hawqeh']] = Create_Player(Hawqeh)
Player.List[['MadCat']] = Create_Player(MadCat)
Player.List[['Harry']] = Create_Player(Harry)
Create_Team(Fuego, c('Conor', 'Chaaxter', 'Hawqeh', 'MadCat', 'Harry'))
Team.List$Fuego$Logo.ID = '[](#fuego)'
Player.List[['Foncho']] = Create_Player(Foncho)
Player.List[['Ryza']] = Create_Player(Ryza)
Player.List[['Phenom']] = Create_Player(Phenom)
Player.List[['Pure']] = Create_Player(Pure)
Player.List[['EFatal']] = Create_Player(EFatal)
Create_Team(LGND, c('Foncho', 'Ryza', 'Phenom', 'Pure', 'EFatal'))
Team.List$LGND$Logo.ID = '[](#lgnd)'

Series('Data/Champs/Champs - 1_fuego_geng.csv', '2019-08-14')
Series('Data/Champs/Champs - 2_celtic_elevate.csv', '2019-08-14')
Series('Data/Champs/Champs - 3_e6_war.csv', '2019-08-14')
Series('Data/Champs/Champs - 4_eu_rbl.csv', '2019-08-14')
Series('Data/Champs/Champs - 5_lgnd_rec.csv', '2019-08-14')
Series('Data/Champs/Champs - 6_sage_nv.csv', '2019-08-14')
Series('Data/Champs/Champs - 7_mazer_units.csv', '2019-08-14') #
Series('Data/Champs/Champs - 8_anim_faze.csv', '2019-08-14')
Series('Data/Champs/Champs - 9_uyu_vanity.csv', '2019-08-14') #
Series('Data/Champs/Champs - 10_100t_sic.csv', '2019-08-14')
Series('Data/Champs/Champs - 11_fury_mid.csv', '2019-08-14') #
Series('Data/Champs/Champs - 12_lg_sng.csv', '2019-08-14') #
Series('Data/Champs/Champs - 13_heretics_hybrid.csv', '2019-08-14')
Series('Data/Champs/Champs - 14_og_train.csv', '2019-08-14') #
Series('Data/Champs/Champs - 15_aspire_splyce.csv', '2019-08-14') #
Series('Data/Champs/Champs - 16_carnage_eg.csv', '2019-08-14')
Series('Data/Champs/Champs - 17_e6_fuego.csv', '2019-08-14')
Series('Data/Champs/Champs - 18_geng_war.csv', '2019-08-14')
Series('Data/Champs/Champs - 19_celtic_eu.csv', '2019-08-14')
Series('Data/Champs/Champs - 20_elevate_rbl.csv', '2019-08-14')
Series('Data/Champs/Champs - 21_anim_units.csv', '2019-08-14')
Series('Data/Champs/Champs - 22_rec_nv.csv', '2019-08-14')
Series('Data/Champs/Champs - 23_faze_mazer.csv', '2019-08-14')
Series('Data/Champs/Champs - 24_lgnd_sage.csv', '2019-08-14')
Series('Data/Champs/Champs - 25_mid_sic.csv', '2019-08-15')
Series('Data/Champs/Champs - 26_sng_vanity.csv', '2019-08-15')
Series('Data/Champs/Champs - 27_100t_fury.csv', '2019-08-15')
Series('Data/Champs/Champs - 28_lg_uyu.csv', '2019-08-15')
Series('Data/Champs/Champs - 29_eg_th.csv', '2019-08-15')
Series('Data/Champs/Champs - 30_asp_hybrid.csv', '2019-08-15')
Series('Data/Champs/Champs - 31_carnage_og.csv', '2019-08-15')
Series('Data/Champs/Champs - 32_heretics_splyce.csv', '2019-08-15')
Series('Data/Champs/Champs - 33_celtic_rbl.csv', '2019-08-15')
Series('Data/Champs/Champs - 34_e6_geng.csv', '2019-08-15')
Series('Data/Champs/Champs - 35_elevate_eu.csv', '2019-08-15')
Series('Data/Champs/Champs - 36_fuego_war.csv', '2019-08-15')
Series('Data/Champs/Champs - 37_lgnd_nv.csv', '2019-08-15')
Series('Data/Champs/Champs - 38_anim_mazer.csv', '2019-08-15')
Series('Data/Champs/Champs - 39_rec_sage.csv', '2019-08-15')
Series('Data/Champs/Champs - 40_faze_units.csv', '2019-08-15')
Series('Data/Champs/Champs - 41_sng_uyu.csv', '2019-08-15')
Series('Data/Champs/Champs - 42_100t_mid.csv', '2019-08-15')
Series('Data/Champs/Champs - 43_fury_sic.csv', '2019-08-15')
Series('Data/Champs/Champs - 44_lg_vanity.csv', '2019-08-15')
Series('Data/Champs/Champs - 45_hybrid_splyce.csv', '2019-08-15')
Series('Data/Champs/Champs - 46_carnage_th.csv', '2019-08-15')
Series('Data/Champs/Champs - 47_eg_og.csv', '2019-08-15')
Series('Data/Champs/Champs - 48_aspire_heretics.csv', '2019-08-15')

# Winners R1
Series('Data/Champs/Champs - 49_nv_units.csv', '2019-08-16')
Series('Data/Champs/Champs - 50_sic_sing.csv', '2019-08-16')
Series('Data/Champs/Champs - 51_eu_war.csv', '2019-08-16')
Series('Data/Champs/Champs - 52_eg_splyce.csv', '2019-08-16')
Series('Data/Champs/Champs - 53_faze_rec.csv', '2019-08-16')
Series('Data/Champs/Champs - 54_100t_lg.csv', '2019-08-16')
Series('Data/Champs/Champs - 55_elevate_e6.csv', '2019-08-16')
Series('Data/Champs/Champs - 56_heretics_og.csv', '2019-08-16')

# Losers R1
Series('Data/Champs/Champs - 57_splyce_war.csv', '2019-08-16')
Series('Data/Champs/Champs - 58_sng_nv.csv', '2019-08-16')
Series('Data/Champs/Champs - 59_elevate_heretics.csv', '2019-08-16')
Series('Data/Champs/Champs - 60_100t_faze.csv', '2019-08-16')

# Winners R2
Series('Data/Champs/Champs - 61_eu_eg.csv', '2019-08-17')
Series('Data/Champs/Champs - 62_sic_units.csv', '2019-08-17')
Series('Data/Champs/Champs - 63_lg_rec.csv', '2019-08-17')
Series('Data/Champs/Champs - 64_e6_og.csv', '2019-08-17')

# Losers R2
Series('Data/Champs/Champs - 65_100t_sic.csv', '2019-08-17')
Series('Data/Champs/Champs - 66_elevate_eg.csv', '2019-08-17')
Series('Data/Champs/Champs - 67_e6_war.csv', '2019-08-17')
Series('Data/Champs/Champs - 68_lg_sng.csv', '2019-08-17')

# Winners SF
Series('Data/Champs/Champs - 69_eu_units.csv', '2019-08-17')
Series('Data/Champs/Champs - 70_og_rec.csv', '2019-08-17')

# Losers R3
Series('Data/Champs/Champs - 71_100t_eg.csv', '2019-08-17')
Series('Data/Champs/Champs - 72_e6_lg.csv', '2019-08-17')

# Losers R4
Series('Data/Champs/Champs - 73_100t_rec.csv', '2019-08-18')
Series('Data/Champs/Champs - 74_e6_units.csv', '2019-08-18')

# Winners Finals
Series('Data/Champs/Champs - 75_eu_og.csv', '2019-08-18')

# Losers SF
Series('Data/Champs/Champs - 76_100t_e6.csv', '2019-08-18')

# Losers Finals
Series('Data/Champs/Champs - 77_100t_og.csv', '2019-08-18')

# Grand Finals
Series('Data/Champs/Champs - 78_100t_eu.csv', '2019-08-18')



