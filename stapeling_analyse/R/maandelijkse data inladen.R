bijstand_inladen <- function(){
  bijstand = read_sav("G:/Socialezekerheid/BIJSTANDUITKERINGTAB/2019/BIJSTANDUITKERINGTAB2019V1.sav",
                      col_select = c('RINPERSOONS', "RINPERSOON",
                                     "BIJSTANDStatistiekcodeA",
                                     'BIJSTANDAanvangUitkeringPersoon1A',
                                     'BIJSTANDEindeUitkeringPersoon1A',
                                     'BIJSTANDIndicatieLopendA')) %>%
    filter(BIJSTANDIndicatieLopendA == "1" | BIJSTANDIndicatieLopendA == "2") %>%
    mutate(bijstand_regeling = recode(BIJSTANDStatistiekcodeA,
                                    "01" = "Bijstand",
                                    "02" = 'IOAW',
                                    "03" = "IOAZ",
                                    "13" = "Bijstand",
                                    "14" = "Bbz",
                                    "15" = "Bijstand",
                                    "16" = "Bijstand",
                                    "18" = "Individuele inkomenstoeslag",
                                    "19" = "Individuele studietoeslag",
                                    "20" = "Bbz")) %>%
    dummy_cols(select_columns = 'bijstand_regeling') %>%
    filter_year_to_month_data("BIJSTANDEindeUitkeringPersoon1A") %>%
    select(RINPERSOONS, RINPERSOON,
           Bbz = bijstand_regeling_Bbz,
           Bijstand = bijstand_regeling_Bijstand,
           IOAW = bijstand_regeling_IOAW,
           IOAZ = bijstand_regeling_IOAZ) %>%
    group_by(RINPERSOONS, RINPERSOON) %>%
    summarise_all(.funs = sum)%>%
    mutate_at(vars(Bbz:IOAZ),
              list(~ ifelse(. > 1, 1, .)))
}

aow_inladen <- function(){
  aow = read_sav("G:/Socialezekerheid/AOWUITKERING1ATAB/2019/AOWUITKERING1A201912TABV1.sav",
                 col_select = c(RINPERSOONS, RINPERSOON, AOWLandcodeA, AOWSoortpensioen2A)) %>%
    filter(AOWSoortpensioen2A != "05" | AOWSoortpensioen2A != "51" | AOWSoortpensioen2A != "52" | AOWSoortpensioen2A != "53" | AOWSoortpensioen2A != "99") %>%
    select(-AOWSoortpensioen2A) %>%
    mutate(AOW = 1) %>%
    filter(RINPERSOON != "1")
        #deze zit er willekeurig 38 keer in
}
anw_inladen <- function(){
  anw = read.csv("G:/Socialezekerheid/ANWUITKERING1ATAB/2019/ANWUITKERING1A201912TABV1.csv",
                 sep = ";") %>%
    filter(Anwsoortpensioen1a != 1 | Anwsoortpensioen1a != 3 | Anwsoortpensioen1a != 10) %>%
    mutate(RINpersoonS = as.character(RINpersoonS),
           RINpersoon = as.character(RINpersoon),
           ANW = 1) %>%
    select(starts_with('RIN'), ANW, Anwlandcodea) %>%
    filter(RINpersoon != "0")
      #RINpersoon 0 zit er 3 keer in voor onduidelijke reden, deze heb ik er nu uitgehaald.

}

ao_inladen <- function(){
  ao = read_sav('G:/Socialezekerheid/AOTOTUITKERINGATAB/2019/AOTOTUITKERINGA201912TABV1.sav') %>%
    mutate(
      TW_bedrag = as.numeric(AOTotBedragTwV2) + as.numeric(AOTotBedragVakantiegeldTwV2),
      TW_dummy = ifelse(TW_bedrag > 0, 1, 0),
      TW_sub0 = ifelse(TW_bedrag < 0, 1, 0),
      AO_uitkering = recode(AOTotWetcodeV2,
                            "1" = "WAO",
                            "2" = "WAZ",
                            "3" = "Wajong",
                            "4" = "IVA",
                            "5" = "WGA",
                            "9" = "Onbekend/fout")
  ) %>%
    filter_year_to_month_data("AOTotDatumEindeV2") %>%
    filter(AOTotStatusV2 == "00001" | AOTotStatusV2 == "10001") %>%
    select(starts_with('RIN'),starts_with('TW'), AO_uitkering) %>%
    dummy_cols(select_columns = 'AO_uitkering') %>%
    group_by(RINPersoonS, RINPersoon) %>%
    summarise(
    #optellen per persoon in welke regeling ze zitten
      IVA = sum(AO_uitkering_IVA),
      Wajong = sum(AO_uitkering_Wajong),
      WAO = sum(AO_uitkering_WAO),
      WAZ = sum(AO_uitkering_WAZ),
      WGA = sum(AO_uitkering_WGA),
      TW_dummy = sum(TW_dummy)
  ) %>%
    mutate(
      IVA = ifelse(IVA > 1, 1, IVA),
      Wajong = ifelse(Wajong > 1, 1, Wajong),
      WAO = ifelse(WAO > 1, 1, WAO),
      WAZ = ifelse(WAZ > 1, 1, WAZ),
      WGA = ifelse(WGA > 1, 1, WGA)
    )
}

ww_inladen <- function(){
  wus = read_sav("G:/Socialezekerheid/WUSUITKERING1ATAB/2019/WUSUITKERING1A201912TABV1.sav") %>%
    filter_year_to_month_data("WUSEindeuitkering1A") %>%
    filter(WUSIndic1A == 1 | WUSIndic1A == 10000 | WUSIndic1A == 10001 | WUSIndic1A == 11000 | WUSIndic1A == 11001) %>%
    select(starts_with('RIN'), WUSBedragTw1A) %>%
    mutate(TW_dummy = ifelse(WUSBedragTw1A > 0, 1, 0),
           WW = 1) %>%
    group_by(RINPersoonS, RINPersoon) %>%
    summarise(
      TW_dummy = sum(TW_dummy),
      WW = sum(WW)
    ) %>%
    mutate(WW = ifelse(WW > 1, 1, WW))
}

zw_inladen <- function(){
  zw <- read_sav("G:/Socialezekerheid/ZWPERSOONMNDBEDRAGBUS/ZWPERSOONMNDBEDRAG2019BUSV1.sav") %>%
    filter_year_to_month_data("EINDMNDBEDRAGZW") %>%
    mutate(ZW = 1) %>%
    select(starts_with('RIN'), ZW) %>%
    unique()
}


sm_inladen <- function(){
  sm <- read_sav('L:/211130 Stapelingsmonitor SZW 2019 V3_selectie9198.sav',
                 col_select = c(RINPERSOONS, RINPERSOON,
                                Kinderopvangtoeslag_2019,
                                leeftijd_2019,
                                huishoudnr_2019)) %>%
    mutate(
      Kinderopvangtoeslag = ifelse(Kinderopvangtoeslag_2019 == -1, 0, Kinderopvangtoeslag_2019),
     )
}

inpatab_inladen <- function(){
  inpatab <- read_sav("G:/InkomenBestedingen/INPATAB/INPA2019TABV2.sav",
                      col_select = c(RINPERSOONS, RINPERSOON,
                                     INPPH868ZTS, INPT7340HRS, INPT6325KGB)) %>%
    mutate(Zorgtoeslag = ifelse(INPPH868ZTS > 0, 1, 0),
           Huurtoeslag = ifelse(INPT7340HRS > 0, 1, 0),
           Kindgebondenbudget = ifelse(INPT6325KGB > 0, 1, 0)) %>%
    select(- starts_with('INP'))
}

akw_inladen <- function(){
  akw <- read_sav("G:/Socialezekerheid/AKWUITKERING1BTAB/2019/AKWUITKERING1B2019k4TABV1.sav")
  verz1 <- akw %>% select(RINPERSOONS = RINPERSOONSverz1b, RINPERSOON = RINPERSOONverz1b)
  verz2 <- akw %>% select(RINPERSOONS = RINPERSOONSverz2b, RINPERSOON = RINPERSOONverz2b)
  akw_ontv <- rbind.data.frame(verz1, verz2) %>% group_by(RINPERSOONS, RINPERSOON) %>%
    summarise(n = n()) %>% mutate(AKW = 1) %>% select(-n)
}
