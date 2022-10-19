source('load/entsoe/_shared.r')
# loadPackages()
library(tidyverse)


# - DOIT -----------------------------------------------------------------------
generation = loadEntsoeComb(
    type = 'generation', month.start = month.start, month.end = month.end, check.updates = TRUE
    # type = 'generation', month.start = month.start, month.end = month.end
)

load = loadEntsoeComb(
    type = 'load', month.start = month.start, month.end = month.end
    # type = 'load', month.start = "2022-08", month.end = month.end, check.updates = FALSE
)

non_demand_countries <- c("UK CTY",
                          "BA CTY",
                          "UA CTY",
                          "XK CTY",
                          "SI CTY",
                          "LU CTY",
                          "MD CTY",
                          "IE CTY",
                          "AL CTY",
                          "GE CTY",
                          "MK CTY",
                          "CY CTY")

available_generation_countries <- c("FI CTY",
                                    "IT CTY",
                                    "PL CTY",
                                    "FR CTY",
                                    "AT CTY",
                                    "DE CTY",
                                    "ES CTY",
                                    "SE CTY",
                                    "BE CTY", "NL CTY", "CZ CTY",
                                    "PT CTY", "SK CTY", "HR CTY",
                                    "SI CTY", "BG CTY", "RO CTY",
                                    "LU CTY", "LT CTY", "EE CTY",
                                    "IE CTY",
                                    "DK CTY", "GR CTY")

yhour <- function(time) {
    (yday(time) - 1) * 24 + hour(time)
}


load_eu_cum <- load %>%
    mutate(Year=year(DateTime),Month=month(DateTime),Week=week(DateTime),mDay=day(DateTime),Day=yday(DateTime),Hour=yhour(DateTime)) %>%
    filter(AreaTypeCode=="CTY") %>%
    group_by(AreaName,ResolutionCode,Week,mDay,Day,Month,Year,Hour) %>%
    summarize(Load=mean(TotalLoadValue)) %>%
    filter(!(AreaName %in% non_demand_countries)) %>%
    ungroup()


#######waterfall
hour_max <- load_eu_cum %>%
    group_by(Year,AreaName) %>%
    summarize(n=n()) %>%
    ungroup() %>%
    arrange(n) %>%
    filter(Year==max(Year)) %>%
    head(n=1) %>%
    ungroup() %>%
    dplyr::select(n) %>%
    unlist()

generation_variables<-c("Fossil Brown coal/Lignite",
                        "Fossil Gas",
                        "Fossil Hard coal",
                        #                        "Hydro Pumped Storage",
                        "Hydro Run-of-river and poundage",
                        "Nuclear",
                        "Solar",
                        "Wind Offshore",
                        "Wind Onshore")


max_hours_generation <- generation %>%
    filter(AreaName %in% available_generation_countries) %>%
    mutate(Year=year(DateTime)) %>%
    filter(Year==max(Year)) %>%
    filter(ProductionType %in% generation_variables) %>%
    group_by(AreaName,ProductionType) %>%
    summarize(n=n()) %>%
    ungroup() %>%
    group_by(ProductionType) %>%
    summarize(n=min(n)) %>%
    mutate(ProductionType=ifelse(ProductionType=="Wind Offshore","Other",ProductionType)) %>%
    mutate(n=ifelse(ProductionType=="Other",max(n),n))


generation_variables<-c("Fossil Brown coal/Lignite",
                        "Fossil Gas",
                        "Fossil Hard coal",
                        #                        "Hydro Pumped Storage",
                        "Hydro Run-of-river and poundage",
                        "Nuclear",
                        "Solar",
                        #                        "Wind Offshore",
                        "Wind Onshore")


generation_aggregated <- generation %>%
    mutate(ProductionType=ifelse(ProductionType %in% generation_variables,ProductionType,"Other")) %>%
    filter(AreaTypeCode=="CTY") %>%
    filter(AreaName %in% available_generation_countries) %>%
    mutate(Year=year(DateTime),Day=yday(DateTime),yHour=yhour(DateTime)) %>%
    filter(Year %in% c(2021,2022)) %>%
    mutate(Year=year(DateTime),Day=yday(DateTime),yHour=yhour(DateTime)) %>%
    group_by(Year,yHour,ProductionType,AreaName) %>%
    summarize(Generation=mean(ActualGenerationOutput,na.rm=TRUE)/1000) %>%
    ungroup() %>%
    full_join(max_hours_generation)  %>%
    group_by(Year,ProductionType) %>%
    filter((yHour<=n)) %>%
    ungroup() %>%
    group_by(Year,ProductionType) %>%
    summarize(Generation=sum(Generation,na.rm=TRUE))

generation_aggregated_diff <- generation_aggregated %>%
    spread(Year, Generation) %>%
    mutate(Change=`2022`-`2021`)

load_agg <- load_eu_cum %>%
    filter(Hour < hour_max) %>%
    filter(Year>2020) %>%
    group_by(Year) %>%
    summarize(Load=sum(Load)/10^3) %>%
    mutate(Label=paste0("Load ",Year)) %>%
    dplyr::select(Label,Value=Load)

generation_aggregated_final <- generation_aggregated_diff %>%
    ungroup() %>%
    dplyr::select(Label=ProductionType,Value=Change) %>%
    arrange(Value)


final_data <- bind_rows(load_agg[1,],
                        generation_aggregated_final) %>%
    mutate(Value=round(Value/1000))

error <- load_agg[2,2]/1000 - final_data$Value %>% sum()

final_data <- final_data %>%
    bind_rows(tibble(Label=c("Statistical difference"),Value=round(c(unlist(error)))))


library(waterfalls)
library(irenabpdata)
waterfall(final_data,calc_total=TRUE,
          fill_by_sign = FALSE,
          fill_colours = c("black",rep(COLORS5[4],3),rep(COLORS5[5],5),COLORS5[4])) +
    theme_bw(base_size=16) +
#    ylim(c(1700,2000)) +
    xlab("") +
    ylab("TWh") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    coord_cartesian(ylim=c(2000, max(final_data$Value)+.025)) +
    scale_fill_manual(values = (c("Decrease" = COLORS3[1], "Increase" = COLORS3[2], "Budget" = "black")))
ggsave("waterfall-electricity.png")

load_agg



########merit order austria
cost_data <- tibble(ProductionType=c("Biomass", "Fossil Gas",  "Hydro Pumped Storage",
                                     "Hydro Run-of-river and poundage", "Hydro Water Reservoir",
                                     "Other","Solar", "Waste","Wind Onshore" ),
                    ProductionCost=c(60,400,100,5,70,20,10,15,13))

generation_cum <- generation %>%
    filter(AreaName=="AT CTY") %>%
    filter(year(DateTime)==2022) %>%
    filter(month(DateTime)==6) %>%
    filter(mday(DateTime)==4) %>%
    filter(hour(DateTime)==0|hour(DateTime)==12) %>%
    filter(minute(DateTime)==0) %>%
    left_join(cost_data) %>%
    arrange(DateTime,ProductionCost) %>%
    mutate(cum_cap=cumsum(ActualGenerationOutput)) %>%
    dplyr::select(cum_cap,ProductionCost,ProductionType,DateTime,ActualGenerationOutput) %>%
    na.omit() %>%
    mutate(xmin=lag(cum_cap))


generation_cum %>%
    ggplot()+
    geom_rect(aes(xmin=xmin,xmax=cum_cap,ymin=0,ymax=ProductionCost,fill=ProductionType),size=2) +
    facet_wrap(.~hour(DateTime)) +
    xlab("Cumulative Capacity (GW)") +
    ylab("Marginal production cost (€)")+
    scale_fill_manual(values=COLORS10) +
    theme_bw() +
    xlim(c(0,13000))
ggsave("merit-order-austria.png")

