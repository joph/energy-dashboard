# - INIT -----------------------------------------------------------------------
rm(list = ls())
source('_shared.r')

d.base <- fread("data/output/heating-degree-days.csv")

d.hdd = d.base[, .(
    date = as.Date(time),
    value = `0`
)]

d.agg.aggm <- fread(file.path(g$d$o, 'consumption-gas-aggm.csv'))

#####temperature correction with linear model#####
d.agg.aggm.variables <- d.agg.aggm[,.(
    date=as.Date(date),
    year=year(date),
    day=yday(date),
    wday=weekdays(date),
    value
),]

d.agg.aggm.variables <- d.agg.aggm.variables[d.hdd, on = "date"][,.(
    date,
    year=year(date),
    day=yday(date),
    wday=ifelse(weekdays(date)%in%c("Saturday","Sunday"),weekdays(date),"Working day"),
    sinc=sin(2*pi*day/365),
    cosc=cos(2*pi*day/365),
    value,
    hdd=i.value,
    hdd2=i.value^2,
    hdd_s_10=ifelse(i.value<10, i.value, 0),
    hdd_l_10=ifelse(i.value>=10, i.value, 0),
    week=as.character(week(date)),
    month=as.character(month(date))
),]

#data_training <- d.agg.aggm.variables[year %in% (2019:(max(year)-1)), , ]
data_training <- d.agg.aggm.variables[year %in% c(2019:2021), , ]

data_prediction <- d.agg.aggm.variables %>%
    filter(year==2022)

linear_model <- lm(value~wday+hdd_s_10+hdd_l_10+week, data=data_training)

summary(linear_model)

prediction <- predict(linear_model, data_prediction, interval="prediction")
#prediction <- predict(linear_model, data_prediction, interval="confidence")

data_prediction <- bind_cols(data_prediction, prediction) %>%
    mutate(diff_fit = value - fit) %>%
    mutate(diff_lwr = value - lwr) %>%
    mutate(diff_upr = value - upr) %>%
    mutate(diff_fit_rel = 100 * diff_fit/prediction) %>%
    mutate(diff_lwr_rel = 100 * diff_lwr/prediction) %>%
    mutate(diff_upr_rel = 100 * diff_upr/prediction) %>%
    mutate(diff_fit_cum = cumsum(diff_fit)) %>%
    mutate(diff_lwr_cum = cumsum(diff_lwr)) %>%
    mutate(diff_upr_cum = cumsum(diff_upr)) %>%
    mutate(value_cum = cumsum(value)) %>%
    mutate(diff_fit_cum_rel = 100 * diff_fit_cum/value_cum) %>%
    mutate(diff_lwr_cum_rel = 100 * diff_lwr_cum/value_cum) %>%
    mutate(diff_upr_cum_rel = 100 * diff_upr_cum/value_cum)

data_prediction %>%
    dplyr::select(day, diff_fit_rel, diff_lwr_rel, diff_upr_rel) %>%
    gather(variable, value, -day) %>%
    group_by(variable) %>%
    mutate(value_rolling=rollmean(value, 14, fill=NA, align="right")) %>%
    ungroup() %>%
    dplyr::select(day, variable, value_rolling) %>%
    spread(variable, value_rolling)  %>%
    ggplot(aes(x=day, y=diff_fit_rel)) +
    geom_ribbon(aes(ymin=diff_lwr_rel, ymax=diff_upr_rel),fill="gray",alpha=0.3) +
    geom_line(size=1) +
    theme_bw()+
    xlab("Tag (2022)") +
    ylab("Relative Differenz zwischen \nModellverbrauch und tatsächlichem Verbrauch \n(%, 14 Tage rollierender Durchschnitt)")


data_prediction %>%
    dplyr::select(day, diff_fit_cum_rel, diff_lwr_cum_rel, diff_upr_cum_rel) %>%
    gather(variable, value, -day) %>%
    group_by(variable) %>%
    mutate(value_rolling=rollmean(value, 7, fill=NA, align="right")) %>%
    ungroup() %>%
    dplyr::select(day, variable, value_rolling) %>%
    spread(variable, value_rolling) %>%
    ggplot(aes(x=day, y=diff_fit_cum_rel)) +
    geom_ribbon(aes(ymin=diff_lwr_cum_rel, ymax=diff_upr_cum_rel),fill="gray",alpha=0.3) +
    geom_line(size=1) +
    theme_bw() +
    xlab("Tag (2022)") +
    ylab("Kumulative relative Differenz zwischen \nModellverbrauch und tatsächlichem Verbrauch \n(% 14 Tage rollierender Durchschnitt)")
