#PNI/SWE
pni_data<-read_csv(here("data","pni_data.csv"), skip = 1)

pni_data<-pni_data %>%
  select( "year" = Year, "value" = "Annual PNI") %>%
  mutate(index = "PNI")

pni_data<-pni_data %>%
  drop_na()

swe_data<-read_csv(here("data","swepeak.csv"))

swe_data<-swe_data %>%
  select(year, "value" = mean) %>%
  mutate(index = "SWE")

##DART
dart_bon<-read_csv(here("data","dart_BON_BOA_sar.csv")) %>%
  mutate(logit.s = qlogis(meanSAR/100)) %>%
  select(year, logit.s ) %>%
  mutate(
    reach = "bon",
         sar = "dart") %>%
  mutate(year = as.numeric(year))

dart_lgr<-read_csv(here("data","LGRLGA_allpassage_wild_spsu_chinook.csv")) %>%
  mutate(logit.s = qlogis(meanSAR/100)) %>%
  select(year, logit.s) %>%
  mutate(reach = "lgr",
         sar = "dart") %>%
  mutate(year = as.numeric(year))

##CJS
cjs_bon<-read_csv(here("data","bon.boa.surv.yr.1994.2021_250309.csv")) %>%
  mutate(logit.s = qlogis(BON_BOA_SAR)) %>%
  select(year, logit.s) %>%
  mutate(sar = "cjs",
         reach = "bon") %>%
  mutate(year = as.numeric(year))
cjs_lgr<-read_csv(here("data","lgr.lga.surv.yr.1994.2021_250309.csv")) %>%
  mutate(logit.s = qlogis(LGR_LGA_SAR)) %>%
  select(year, logit.s) %>%
  mutate(sar = "cjs",
         reach = "lgr") %>%
  mutate(year = as.numeric(year))


# DLM

sar<-dart_bon %>%
  left_join(pni_data, by = "year") %>%
  drop_na(index)

sar<-dart_bon %>%
  left_join(swe_data, by = "year") %>%
  drop_na(index)


years <- sar$year
TT <- length(years)
dat <- matrix(sar$logit.s, nrow = 1)

m <- 2
index <- sar$value
index_z <- matrix((index - mean(index)) / sqrt(var(index)), nrow = 1)
B <- "diagonal and unequal"  #diag(m) ##diagonal and unequal" #diag(m) #"identity" #"unconstrained"
U <- matrix(0, nrow = m, ncol = 1) ##"zero" #"u", or set to -1 to 1
Q <-  "unconstrained"  #"diagonal and equal" #"equalvarcov" #unconstrained" #"unconstrained"
A <- matrix(0)
R <- matrix("r") #.05 #"r"
Z <- array(NA, c(1, m, TT))
Z[1,1,] <- rep(1, TT)
Z[1,2,] <- index_z

inits_list <- list(x0 = matrix(c(0), nrow = m))
mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)

fit.cjs.lgr.pni<- MARSS::MARSS(dat, inits = inits_list, model = mod_list)

autoplot(fit.cjs.lgr.pni, silent = TRUE)
autoplot(fit.dart.bon.swe, silent = TRUE, plot.type = "fitted.ytt1")

saveRDS(fit.dart.lgr.swe, here("results","fit.dart.lgr.swe.rds") )

### annual trends : %spill, temp, flow


#BON
{
url<-"https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&year%5B%5D=2023&year%5B%5D=2022&year%5B%5D=2021&year%5B%5D=2020&loc%5B%5D=BON&loc%5B%5D=LWG&data%5B%5D=Spill+Percent&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=0&y1max=&y2min=&y2max=&size=large"

url <- paste0(
  "https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?",
  "sc=1&mgconfig=river&outputFormat=csvSingle",
  "&", paste0("year%5B%5D=", 1998:2023, collapse = "&"),
  "&loc%5B%5D=BON",
  # "&loc%5B%5D=LWG",
  "&data%5B%5D=Spill+Percent",
  "&data%5B%5D=Temp+%28WQM%29",
  "&data%5B%5D=Outflow",
  "&startdate=1%2F1&enddate=12%2F31",
  "&avgyear=0&consolidate=1&grid=1",
  "&y1min=0&y1max=&y2min=&y2max=&size=large"
)

riv_bon<-read_csv(url) %>%
  mutate(date = ymd(paste0(year,`mm-dd`, sep = "-")),
         doy = yday(date))

riv_bon_spill <- riv_bon %>%
  filter(between(doy, 90,160),
         parameter == "spillpct") %>%
  group_by(year) %>%
  mutate(mean = mean(value, na.rm = TRUE),
         median. = median(value),
         mode = mode(value))

riv_bon_mean<-riv_bon_spill %>%
  group_by(year, parameter, location) %>%
  distinct(mean) %>%
  rename(
     "value" = mean ,
    "index" = parameter ,
    "reach" = location
  )


riv_bon_flow<-riv_bon %>%
  filter(between(doy, 90,160),
         parameter == "outflow") %>%
  group_by(year) %>%
  mutate(mean = mean(value, na.rm = TRUE),
         median. = median(value),
         mode = mode(value))

riv_bon_mean<-riv_bon_mean %>%
  bind_rows(riv_bon_flow %>%
              group_by(year, parameter, location) %>%
              distinct(mean) %>%
              rename(
                "value" = mean ,
                "index" = parameter ,
                "reach" = location
              ))
riv_bon_temp<-riv_bon%>%
  filter(parameter == "tempc") %>%
  group_by(year) %>%
  mutate(temp7d = zoo::rollmean(value, 7, fill = NA, align = "right")) %>%
  filter(between(doy, 90,160)) %>%
  mutate(mean = mean(temp7d, na.rm = TRUE),
         median. = median(temp7d),
         mode = mode(temp7d))

riv_bon_mean<-riv_bon_mean %>%
  bind_rows(riv_bon_temp %>%
              group_by(year, parameter, location) %>%
              distinct(mean) %>%
              rename(
                "value" = mean ,
                "index" = parameter ,
                "reach" = location
              )) %>%
  mutate(year = as.numeric(year))

#plots
riv_bon_spill %>%
  group_by(year) %>%
  ggplot(aes(x = doy, y = value, color = year)) +
  geom_line() +
  geom_segment(
               aes(x = min(doy), xend = max(doy),
                   y = mean, yend = mean,
                   color = year),
               linetype = "dashed", size = 0.8) +
  labs(title = "Spill Percent at BON",
       x = "Date",
       y = "Spill Percent") +
  theme_minimal()

riv_bon_flow %>%
  group_by(year) %>%
  ggplot(aes(x = doy, y = value, color = year)) +
  geom_line() +
  geom_segment(
    aes(x = min(doy), xend = max(doy),
        y = mean, yend = mean,
        color = year),
    linetype = "dashed", size = 0.8) +
  labs(title = "Outflow at BON",
       x = "Date",
       y = "Outflow") +
  theme_minimal()

riv_bon_temp %>%
  group_by(year) %>%
  ggplot(aes(x = doy, y = value, color = year)) +
  geom_line() +
  geom_segment(
    aes(x = min(doy), xend = max(doy),
        y = mean, yend = mean,
        color = year),
    linetype = "dashed", size = 0.8) +
  labs(title = "Temp at BON",
       x = "Date",
       y = "Temp") +
  theme_minimal()

}
#LGR
{

  url <- paste0(
    "https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?",
    "sc=1&mgconfig=river&outputFormat=csvSingle",
    "&", paste0("year%5B%5D=", 1998:2023, collapse = "&"),
    # "&loc%5B%5D=BON",
    "&loc%5B%5D=LWG",
    "&data%5B%5D=Spill+Percent",
    "&data%5B%5D=Temp+%28WQM%29",
    "&data%5B%5D=Outflow",
    "&startdate=1%2F1&enddate=12%2F31",
    "&avgyear=0&consolidate=1&grid=1",
    "&y1min=0&y1max=&y2min=&y2max=&size=large"
  )

  riv_lgr<-read_csv(url) %>%
    mutate(date = ymd(paste0(year,`mm-dd`, sep = "-")),
           doy = yday(date))

  riv_lgr_spill <- riv_lgr %>%
    filter(between(doy, 90,160),
           parameter == "spillpct") %>%
    group_by(year) %>%
    mutate(mean = mean(value, na.rm = TRUE),
           median. = median(value),
           mode = mode(value))

  riv_lgr_mean<-riv_lgr_spill %>%
    group_by(year, parameter, location) %>%
    distinct(mean) %>%
    rename(
      "value" = mean ,
      "index" = parameter ,
      "reach" = location
    )


  riv_lgr_flow<-riv_lgr %>%
    filter(between(doy, 90,160),
           parameter == "outflow") %>%
    group_by(year) %>%
    mutate(mean = mean(value, na.rm = TRUE),
           median. = median(value),
           mode = mode(value))

  riv_lgr_mean<-riv_lgr_mean %>%
    bind_rows(riv_lgr_flow %>%
                group_by(year, parameter, location) %>%
                distinct(mean) %>%
                rename(
                  "value" = mean ,
                  "index" = parameter ,
                  "reach" = location
                ))
  riv_lgr_temp<-riv_lgr%>%
    filter(parameter == "tempc") %>%
    group_by(year) %>%
    mutate(temp7d = zoo::rollmean(value, 7, fill = NA, align = "right")) %>%
    filter(between(doy, 90,160)) %>%
    mutate(mean = mean(temp7d, na.rm = TRUE),
           median. = median(temp7d),
           mode = mode(temp7d))

  riv_lgr_mean<-riv_lgr_mean %>%
    bind_rows(riv_lgr_temp %>%
                group_by(year, parameter, location) %>%
                distinct(mean) %>%
                rename(
                  "value" = mean ,
                  "index" = parameter ,
                  "reach" = location
                )) %>%
    mutate(year = as.numeric(year))

  #plots
  riv_lgr_spill %>%
    group_by(year) %>%
    ggplot(aes(x = doy, y = value, color = year)) +
    geom_line() +
    geom_segment(
      aes(x = min(doy), xend = max(doy),
          y = mean, yend = mean,
          color = year),
      linetype = "dashed", size = 0.8) +
    labs(title = "Spill Percent at lgr",
         x = "Date",
         y = "Spill Percent") +
    theme_minimal()

  riv_lgr_flow %>%
    group_by(year) %>%
    ggplot(aes(x = doy, y = value, color = year)) +
    geom_line() +
    geom_segment(
      aes(x = min(doy), xend = max(doy),
          y = mean, yend = mean,
          color = year),
      linetype = "dashed", size = 0.8) +
    labs(title = "Outflow at lgr",
         x = "Date",
         y = "Outflow") +
    theme_minimal()

  riv_lgr_temp %>%
    group_by(year) %>%
    ggplot(aes(x = doy, y = value, color = year)) +
    geom_line() +
    geom_segment(
      aes(x = min(doy), xend = max(doy),
          y = mean, yend = mean,
          color = year),
      linetype = "dashed", size = 0.8) +
    labs(title = "Temp at lgr",
         x = "Date",
         y = "Temp") +
    theme_minimal()

}


### DLM

sar<-dart_lgr %>%
left_join(riv_lgr_mean %>% filter(index == "outflow"), by = "year") %>%
  drop_na(index)



# sar<-dart_bon %>%
#   left_join(swe_data, by = "year") %>%
#   drop_na(index)


years <- sar$year
TT <- length(years)
dat <- matrix(sar$logit.s, nrow = 1)

m <- 2
index <- sar$value
index_z <- matrix((index - mean(index)) / sqrt(var(index)), nrow = 1)
B <-diag(m) ##diagonal and unequal" #diag(m) #"identity" #"unconstrained"
U <- matrix(0, nrow = m, ncol = 1) ##"zero" #"u", or set to -1 to 1
Q <- "unconstrained" #"equalvarcov" #unconstrained" #"unconstrained"
A <- matrix(0)
R <- matrix(.05) #.05 #"r"
Z <- array(NA, c(1, m, TT))
Z[1,1,] <- rep(1, TT)
Z[1,2,] <- index_z

inits_list <- list(x0 = matrix(c(0), nrow = m))
mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)

fit.dart.lgr.flow<- MARSS::MARSS(dat, inits = inits_list, model = mod_list)

autoplot(fit.dart.lgr.flow, silent = TRUE)
autoplot(fit.dart.bon.swe, silent = TRUE, plot.type = "fitted.ytt1")

saveRDS(fit.dart.lgr.spill, here("results","fit.dart.lgr.spill.rds") )






# at a glance-- annual trend
ggplot(riv_bon_mean , aes( x= year, y = value)) +
  geom_line() +
  geom_smooth(method ="lm", se = FALSE, color = "black") +
  facet_grid(index~., scale = "free")


ggplot(pni_data , aes( x= year, y = value)) +
  geom_line() +
  geom_smooth(method ="lm", se = FALSE, color = "black") +
  facet_grid(index~., scale = "free")

ggplot(swe_data , aes( x= year, y = value)) +
  geom_line() +
  geom_smooth(method ="lm", se = FALSE, color = "black") +
  facet_grid(index~., scale = "free")
























