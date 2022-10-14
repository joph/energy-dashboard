# - INIT -----------------------------------------------------------------------
rm(list = ls())
source('load/entsoe/_shared.r')
loadPackages(ggplot2)

transmission = loadEntsoeComb(
    type = 'transmission', month.start = month.start, month.end = month.end
)

names(transmission)

transmission.at = transmission[OutAreaName=="AT CTY" | InAreaName=="AT CTY"]

transmission.at.in = transmission[InAreaName=="AT CTY"] %>%
    dplyr::select(DateTime, Capacity) %>%
    group_by(DateTime) %>%
    summarize(Capacity=sum(Capacity)) %>%
    mutate(type="inflow")

transmission.at.out = transmission[OutAreaName=="AT CTY"] %>%
    dplyr::select(DateTime, Capacity) %>%
    group_by(DateTime) %>%
    summarize(Capacity=sum(Capacity)) %>%
    mutate(type="outflow")

transmission.at.net.import = bind_rows(transmission.at.in,
          transmission.at.out) %>%
    spread(type,Capacity) %>%
    mutate(net_import_capacity=inflow-outflow)

transmission.at.net.import %>%
    ggplot(aes(x=DateTime,y=net_import_capacity)) +
    geom_line()

transmission.at.net.import.hourly = transmission.at.net.import %>%
    group_by(hour=yhour(DateTime),year=year(DateTime)) %>%
    summarize(DateTime=min(DateTime), net_import_capacity=mean(net_import_capacity)) %>%
    arrange(DateTime) %>%
    mutate(DateTime=as_datetime(DateTime))



prices = fread("data/prices/price-electricity-hourly.csv")

prices[, date_full:=ymd_h(paste(date, hour)), ]
prices[, year:=year(date_full)]

yhour <- function(time) {
    (yday(time) - 1) * 24 + hour(time)
}

prices[, hour_of_year:=yhour(date_full), by=c("year")]

prices = prices[order(date_full)]

prices %>% ggplot(aes(x=hour_of_year, y=price)) +
    geom_line() +
    facet_wrap(.~as.character(year)) +
    theme_bw()

# - DOIT -----------------------------------------------------------------------
d.base = loadEntsoeComb(
    type = 'load', month.start = month.start, month.end = month.end
    # type = 'load', month.start = "2022-08", month.end = month.end, check.updates = FALSE
)

load = d.base[AreaName == "AT CTY", .(
    dateTime = DateTime, load = TotalLoadValue
)]


load.day <- load %>%
    mutate(day=yday(dateTime)) %>%
    mutate(year=year(dateTime))

max_day_2022 <- load.day %>%
    filter(year==2022) %>%
    summarize(max_day=max(day)) %>%
    unlist()

max_date_2022 <- load.day %>%
    filter(year==2022) %>%
    summarize(max_date=max(dateTime))

mean.load.2019.2022 = load.day %>%
    filter(day <= max_day_2022) %>%
    summarize(mean_load=sum(load)/10^6/4)


load.day %>%
    filter(day <= max_day_2022) %>%
    group_by(year) %>%
    summarize(Electricity_Consumption = 100*(sum(load)/10^6-mean.load.2019.2022$mean_load)/mean.load.2019.2022$mean_load) %>%
    ggplot(aes(x=year,y=Electricity_Consumption)) +
    geom_bar(stat="identity",position="dodge",col="black") +
    theme_bw(base_size = 16) +
    xlab("Year") +
    ylab(glue("Electricity consumption until {max_date_2022$max_date}\n (% Deviation from Mean (2019-2022))")) +
    theme_bw(base_size=16)
ggsave("data/output/electric-load.png")


###can we predict prices by load?
setkey(prices, date_full)
setkey(load, dateTime)

prices_load = na.omit(prices[load])




d.base = loadEntsoeComb(
    type = 'generation', month.start = month.start, month.end = month.end
    # type = 'generation', month.start = month.start, month.end = month.end
)


generation = d.base[AreaName == "AT CTY" & ResolutionCode == "PT15M", .(
    value = mean(ActualGenerationOutput)/4/10^3
), by = .(year = year(DateTime), hour = yhour(DateTime), source = ProductionType)][order(year, hour)]

nrow(generation)

generation[, hour_of_year:=hour, ]

prices_load_residual_load = left_join(prices_load, generation, by=c("year"="year", "hour_of_year"="hour_of_year")) %>%
    spread(source, value) %>%
    mutate(residual_load = load-`Hydro Run-of-river and poundage` - `Wind Onshore` - `Solar`) %>%
    dplyr::select(date, date_full, year, hour_of_year, price, load, residual_load)

mod = lm(price~residual_load, data=prices_load_residual_load)
summary(mod)

prices_load_residual_load$prediction = predict(mod, prices_load_residual_load)

prices_load_residual_load %>%
    gather(variable, value, -date, -date_full, -year, -hour_of_year, -load, -residual_load) %>%
    ggplot(aes(x=date_full, y=value)) +
    geom_line(aes(col=variable)) +
    theme_bw()

price.data.original = fread("data/prices/prices.csv")

full.data = left_join(prices_load_residual_load,
          price.data.original,by=c("date"="date")) %>%
    mutate(gas_2=gas^2)

mod = (lm(price~gas+eua+residual_load, data=full.data))

summary(mod)

full.data$prediction = predict(mod, full.data)

full.data %>%
    gather(variable,
           value,
           -date,
           -date_full,
           -year,
           -hour_of_year,
           -load,
           -residual_load,
           -gas,
           -gas_2,
           -eua,
           -electricity.base,
           -electricity.peak) %>%
    ggplot(aes(x=date_full, y=value)) +
    geom_line(aes(col=variable)) +
    theme_bw()

full.data %>%
    ggplot(aes(x=price, y=prediction)) +
    geom_point() +
    theme_bw()

full.data %>%
    ggplot(aes(x=date_full, y=price-prediction)) +
    geom_point() +
    theme_bw()

price.coal <- fread("data/prices/price-coal.csv") %>%
    dplyr::select(date, coal_price=price)

full.data.imports = left_join(full.data,
                      transmission.at.net.import.hourly,by=c("date_full"="DateTime")) %>%
    mutate(residual_load_imports=residual_load-net_import_capacity) %>%
    mutate(hour_of_day_dummy=as.character(hour(date_full))) %>%
    mutate(month_dummy=as.character(month(date_full))) %>%
    mutate(weekday_dummy=wday(date_full)) %>%
    mutate(week_dummy=as.character(week(date_full))) %>%
    left_join(price.coal, by=c("date"="date"))

nrow(full.data.imports)

full.data.imports.filter = full.data.imports %>%
    filter(weekday_dummy %in% c(2:6)) %>%
    mutate(log_gas=log(gas)) %>%
    mutate(log_price=log(price)) %>%
    mutate(log_eua=log(eua)) %>%
    mutate(log_residual_load=log(residual_load)) %>%
    mutate(log_net_import_capacity=log(net_import_capacity)) %>%
    mutate(log_coal_price=log(coal_price)) %>%
    dplyr::select(date,date_full,price,log_price,log_gas,log_eua,residual_load,net_import_capacity,log_coal_price) %>%
    na.omit() %>%
    filter(log_gas>(-10)) %>%
    filter(log_price>(-10))



mod = (lm(log_price~log_gas+residual_load+net_import_capacity+log_eua+log_coal_price, data=full.data.imports.filter))

summary(mod)

library("lmtest")
library("sandwich")

# Robust t test
coeftest(mod, vcov = vcovHC(mod, type = "HC0"))

full.data.imports.filter$prediction = predict(mod, full.data.imports.filter)

full.data.imports.filter %>%
    dplyr::select(date_full,log_price,prediction) %>%
    gather(variable,
           value,
          -date_full) %>%
    ggplot(aes(x=date_full, y=value)) +
    geom_line(aes(col=variable)) +
    theme_bw()

full.data.imports.filter %>%
    ggplot(aes(x=log_price, y=prediction)) +
    geom_point(size=0.05) +
    theme_bw() +
    geom_abline(intercept=0, slope=1, col="red") +
    xlab("Observed price (€/MWh)") +
    ylab("Predicted price (€/MWh)")

full.data.imports.filter %>%
    ggplot(aes(x=date_full, y=log_price-prediction)) +
    geom_point(size=0.05) +
    theme_bw()

full.data.imports %>%
    ggplot(aes(x=load, y=price)) +
    geom_point(size=0.05) +
    geom_smooth(method="lm", col="red") +
    xlab("Load (MW)") +
    ylab("Price (€/MWh)") +
    theme_bw() +
    facet_wrap(.~year,scale="free")

full.data.imports %>%
    ggplot(aes(x=residual_load, y=price)) +
    geom_point(size=0.05) +
    geom_smooth(method="lm", col="red") +
    xlab("Residual load (MW)") +
    ylab("Price (€/MWh)") +
    theme_bw() +
    facet_wrap(.~year,scale="free")

full.data.imports %>%
    ggplot(aes(x=net_import_capacity, y=price)) +
    geom_point(size=0.05) +
    geom_smooth(method="lm", col="red") +
    xlab("Net imports (MW)") +
    ylab("Price (€/MWh)") +
    theme_bw() +
    facet_wrap(.~year,scale="free")

full.data.imports %>%
    ggplot(aes(x=gas, y=price)) +
    geom_point(size=0.05) +
    geom_smooth(method="lm", col="red") +
    xlab("Gas price (€/MWh)") +
    ylab("Price (€/MWh)") +
    theme_bw() +
    facet_wrap(.~year,scale="free")

full.data.imports %>%
    ggplot(aes(x=eua, y=price)) +
    geom_point(size=0.05) +
    geom_smooth(method="lm", col="red") +
    xlab("Emission price (€/tCO2)") +
    ylab("Price (€/MWh)") +
    theme_bw() +
    facet_wrap(.~year,scale="free")



price.data.original = fread("data/prices/prices.csv")

EFFICIENCY = 0.55


price.data = price.data.original

EMISSION_FACTOR_GAS_POWER = 0.202/EFFICIENCY

price.data[, simulation := gas / EFFICIENCY + eua * EMISSION_FACTOR_GAS_POWER / EFFICIENCY, ]


price.data = price.data[, .(date, electricity.base, simulation), ]

melt(price.data, c("date")) %>%
    ggplot(aes(x = date, y = value)) +
    geom_line(aes(col = variable)) +
    theme_bw() +
    xlab("Datum") +
    ylab("Strompreis (€/MWh")

price.data %>%
    ggplot(aes(x = electricity.base, y = simulation)) +
    geom_point(aes(col = date)) +
    geom_abline(slope = 1, intercept = 0) +
    theme_bw(base_size=16) +
    xlab("Austrian Base Price Electricity (€/MWh)") +
    ylab("Simulated Base Price Electricity (€/MWh)") +
    ylim(c(0, 788))

ggsave("data/output/market-power.png",width=10,height=6)


rmse = function(a, b) {
    sqrt(mean((a - b) ^ 2))

}

print(EFFICIENCY)
mean(price.data$electricity.peak)
mean(price.data$simulation)


#for(EFFICIENCY in c(0.54, 0.55, 0.56, 0.57, 0.58)) {
#}
