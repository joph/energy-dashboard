# - INIT -----------------------------------------------------------------------
rm(list = ls())
source('_shared.r')
loadPackages(stringr)


# - DATA -----------------------------------------------------------------------
d.hdd = fread(file.path(g$d$o, 'temp-hdd.csv'))[, `:=`(
    date = as.Date(date)
)]

# LOAD/PREP GAS CONS
d.consumption = fread(file.path(g$d$o, 'consumption-gas-aggm.csv'))[, .(
    date = as.Date(date),
    value = value
)]

# MERGE
d.comb = merge(d.consumption, d.hdd, by = "date")

# AUGMENT
d.comb[, `:=`(
    t = as.integer(date - min(date)),
    year = year(date),
    day = yday(date),
    week = week(date),
    wday = factor(
        weekdays(date, abbreviate = TRUE),
        c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    temp.15 = ifelse(temp < 15, 15 - temp, 0),
    temp.squared  = temp^2,
    temp.lag = shift(temp, 1)
)]

d.comb[, `:=`(
    t.squared = t^2,
    workday = ifelse(wday %in% c("Sat", "Sun"), wday, "Working day"),
    week = str_pad(week, 2, pad = "0"),
    temp.15.squared = temp.15^2,
    temp.15.lag = shift(temp.15, 1),
    year_character = as.character(year)
)]

#add holidays (feiertage) and holidays (ferien)
holidays = list()
for(year in 2014:2022){
    holidays = append(holidays, list(jsonlite::fromJSON(glue("https://date.nager.at/api/v2/publicholidays/{year}/AT "))))
}

holidays = bind_rows(holidays) %>%
    mutate(date=ymd(date)) %>%
    dplyr::select(date) %>%
    mutate(is.holiday = 1)

d.comb = d.comb %>%
    left_join(holidays,by = c("date"="date")) %>%
    mutate(is.holiday = ifelse(is.na(is.holiday), 0, 1)) %>%
    mutate(is.holiday = as.character(is.holiday)) %>%
    mutate(is.christmas.holidays = ifelse(week == 1|week == 52,1, 0)) %>%
    mutate(is.holidays.july = ifelse(month(date) == 7, 1, 0)) %>%
    mutate(is.holidays.august = ifelse(month(date) == 8, 1, 0))

###does average load & temperature dependency change over time?
coefs <- list()
for(year_c in c(2014:2022)){
    coefs = append(coefs, list(summary(lm(value~temp, dat=d.comb %>% filter(year==year_c)))$coefficients %>% as_tibble() %>% mutate(year=year_c) %>% mutate(type=c("intercept", "slope"))))
}

bind_rows(coefs) %>%
    dplyr::select(year, Estimate, type) %>%
    ggplot(aes(x=year, y=Estimate)) +
    geom_bar(aes(fill=type), stat="identity", position="dodge") +
    facet_wrap(.~type, scale="free")

####I'd say, 2016:2021 look similar, 2019 being an exception for some reason
####2014:2015 have a lower intercept and a higher slope, I would perhaps exclude them therefore - and maybe also 2019?



train_and_test_model <- function(years){

    # - MODEL ----------------------------------------------------------------------
    #d.train = d.comb[year %in% (min.year:(max(year) - 1)), ]
    d.train = d.comb[year %in% years, ]
    d.pred = copy(d.comb)

   m.linear = lm(
        value ~ t + t.squared +
            temp + temp.squared + temp.15 + temp.15.squared + temp.15.lag +
            wday + is.holiday + is.christmas.holidays + is.holidays.july + is.holidays.august
        , data = d.train)

    summary(m.linear)

    d.pred[, prediction := predict(m.linear, d.pred)]

    d.pred %>% ggplot(aes(x=value, y=prediction)) +
        geom_point(alpha=0.5, size=0.5) +
        geom_smooth(method="lm", size=1, col="red") +
        facet_wrap(.~year)

    rmse <- function(observed, predicted){
        sqrt(mean((observed - predicted)^2))
    }

    d.pred %>%
        group_by(year) %>%
        summarize(r2 = cor(value, prediction)^2,
                  mean_bias = mean(value) - mean(prediction),
                  rmse = rmse(value, prediction)) %>%
        print()

    p <- d.pred %>%
        dplyr::select(year, day, value, prediction) %>%
        gather(variable, value, -year, -day) %>%
        mutate(value = rollmean(value, 14, na.pad = TRUE, "left")) %>%
        ggplot(aes(x=day, y=value)) +
        geom_line(aes(col=variable)) +
        facet_wrap(.~year)

    plot(p)
}

years = c(2016:2018, 2019:2021)

train_and_test_model(years)

years = (min(d.comb$year):(max(d.comb$year) - 1))

train_and_test_model(years)



# - MODEL ----------------------------------------------------------------------
#d.train = d.comb[year %in% (min.year:(max(year) - 1)), ]
d.train = d.comb[year %in% years, ]
d.pred = copy(d.comb)

m.linear = lm(
    value ~ t + t.squared +
        temp + temp.squared + temp.15 + temp.15.squared + temp.15.lag +
        wday + is.holiday + is.christmas.holidays + is.holidays.july + is.holidays.august
    , data = d.train)

summary(m.linear)

d.pred[, prediction := predict(m.linear, d.pred)]

d.pred %>% ggplot(aes(x=value, y=prediction)) +
    geom_point(alpha=0.5, size=0.5) +
    geom_smooth(method="lm", size=1, col="red") +
    facet_wrap(.~year)

rmse <- function(observed, predicted){
    sqrt(mean((observed - predicted)^2))
}

d.pred %>%
    group_by(year) %>%
    summarize(r2 = cor(value, prediction)^2,
              mean_bias = mean(value) - mean(prediction),
              rmse = rmse(value, prediction))

d.pred %>%
    dplyr::select(year, day, value, prediction) %>%
    gather(variable, value, -year, -day) %>%
    mutate(value = rollmean(value, 14, na.pad = TRUE, "left")) %>%
    ggplot(aes(x=day, y=value)) +
    geom_line(aes(col=variable)) +
    facet_wrap(.~year)



# - OUTPUT ---------------------------------------------------------------------
d.all = melt(d.pred, variable.name = 'type',
    id.vars = c('date'), measure.vars = c('value', 'prediction')
)

# PREP FOR PLOT
addRollMean(d.all, 7, 'type')
addCum(d.all, 'type')
d.plot <- melt(d.all, id.vars = c("date", "type"))[!is.na(value)]
dates2PlotDates(d.plot)

# SAVE
fwrite(d.plot[date >= "2019-01-01"], file.path(g$d$wd, 'pred-gas-cons.csv'))
