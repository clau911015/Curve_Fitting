library(dataonderivatives)


library(lubridate)


view<-ddr(Sys.Date()-1, "FX")


view<-view %>%
  subset(select=-c(RESET_FREQUENCY_1,RESET_FREQUENCY_2,PAYMENT_FREQUENCY_1,PAYMENT_FREQUENCY_2,
                   INDICATION_OF_END_USER_EXCEPTION,DAY_COUNT_CONVENTION,SETTLEMENT_CURRENCY,
                   `SUB-ASSET_CLASS_FOR_OTHER_COMMODITY`,UNDERLYING_ASSET_1,UNDERLYING_ASSET_2,
                   PRICE_NOTATION3,PRICE_NOTATION2,PRICE_NOTATION2_TYPE,PRICE_NOTATION3_TYPE))

view<-view %>%
          filter(TAXONOMY == "ForeignExchange:VanillaOption" & (
                   (NOTIONAL_CURRENCY_1=="USD" & NOTIONAL_CURRENCY_2=="MXN") |
                   (NOTIONAL_CURRENCY_2=="USD" & NOTIONAL_CURRENCY_1=="MXN")))




dyn.load("C:/Users/MB50294/AppData/Roamin/BBVANPUpdater/DLLADDIN/DLL/BBVADll.dll")



