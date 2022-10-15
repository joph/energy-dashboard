# - INIT -----------------------------------------------------------------------
rm(list = ls())
source('_shared.r')

library(tidyverse)

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
    wday=weekdays(date),
    #ifelse(weekdays(date)%in%c("Saturday","Sunday"),weekdays(date),"Working day"),
    value,
    hdd=i.value,
    hdd_s_10=ifelse(i.value<10, i.value, 0),
    hdd_l_10=ifelse(i.value>=10, i.value, 0),
    week=as.character(week(date)),
    month=as.character(month(date))
),]

mods <- list()

for(current_year in list(c(2019),c(2020),c(2021),c(2019:2021),c(2019:2020),c(2020:2021),c(2019,2021))){
    data_training <- d.agg.aggm.variables[year%in%current_year, , ]
    #data_training <- d.agg.aggm.variables[year %in% c(2019, 2021), , ]



    data_test = d.agg.aggm.variables %>%
        filter(!(year %in%current_year)) %>%
        filter(year!=2022)


    data_prediction <- d.agg.aggm.variables %>%
        filter(year==2022)

    linear_model <- lm(value~wday+hdd_s_10+hdd_l_10, data=data_training)

    summary(linear_model)

    data_test$prediction = predict(linear_model, data_test)


    print(current_year)
    print("Correlation: ")
    print(cor(data_test$prediction, data_test$value,use="pairwise.complete.obs"))

    p <- data_test %>%
        ggplot(aes(x=value,y=prediction)) +
        geom_point() +
        geom_abline(intercept=0,slope=1,col="red")


    plot(p)

    prediction <- predict(linear_model, data_prediction, interval="confidence")
    #prediction <- predict(linear_model, data_prediction, interval="confidence")

    data_prediction <- bind_cols(data_prediction, prediction) %>%
        mutate(diff_fit = value - fit) %>%
        mutate(diff_lwr = value - lwr) %>%
        mutate(diff_upr = value - upr) %>%
        mutate(diff_fit_rel = 100 * diff_fit/fit) %>%
        mutate(diff_lwr_rel = 100 * diff_lwr/fit) %>%
        mutate(diff_upr_rel = 100 * diff_upr/fit) %>%
        mutate(diff_fit_cum = cumsum(diff_fit)) %>%
        mutate(diff_lwr_cum = cumsum(diff_lwr)) %>%
        mutate(diff_upr_cum = cumsum(diff_upr)) %>%
        mutate(value_cum = cumsum(value)) %>%
        mutate(diff_fit_cum_rel = 100 * diff_fit_cum/value_cum) %>%
        mutate(diff_lwr_cum_rel = 100 * diff_lwr_cum/value_cum) %>%
        mutate(diff_upr_cum_rel = 100 * diff_upr_cum/value_cum) %>%
        mutate(train_year=paste0(current_year,collapse=" ")) %>%
        as_tibble()

    mods <- append(mods, list(data_prediction))

}

library(irenabpdata)

all_mods = bind_rows(mods)

all_mods %>%
    dplyr::select(day, diff_fit_rel,train_year) %>%
    group_by(train_year) %>%
    mutate(diff_fit_rel=rollmean(diff_fit_rel, 14, fill=NA, align="right")) %>%
    ungroup() %>%
    ggplot(aes(x=day,y=diff_fit_rel)) +
    geom_line(aes(col=train_year,linetype=train_year),size=1) +
    scale_color_manual(values=COLORS10) +
    theme_bw()

all_mods %>%
    filter(train_year == "2019 2020 2021") %>%
    dplyr::select(day, diff_fit_rel, diff_lwr_rel, diff_upr_rel) %>%
    gather(variable, value, -day) %>%
    group_by(variable) %>%
    mutate(value_rolling=rollmean(value, 28, fill=NA, align="right")) %>%
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
