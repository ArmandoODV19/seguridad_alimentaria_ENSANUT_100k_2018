# abrir seguridad alimentaria

seguridad_alimentaria <- readRDS("clean_data/seguridad_alimentaria_100k_2018.rds")

# seguridad alimentaria en el pais

security_plot <- function(x = seguridad_alimentaria){
  x %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = n/8996) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    geom_col()+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme_classic() +
    theme(legend.position="none")
}

security_plot()

security_df <- function(x = seguridad_alimentaria){
  x %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/8996)
}

security_df()

# seguridad alimentaria por estado

security_state_plot <- function(x = seguridad_alimentaria, state){
  sta <- x %>%
    select(alimentaria, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, entidad) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(entidad == state) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme_classic()+
    theme(legend.position="none")
}

security_state_plot(state = "AGUASCALIENTES")

security_state_df <- function(x = seguridad_alimentaria, state){
  sta <- x %>%
    select(alimentaria, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, entidad) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(entidad == state) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total)
}

security_state_df(state = "AGUASCALIENTES")

# seguridad alimentaria por municipio

security_city_plot <- function(x = seguridad_alimentaria, city){
  sta <- x %>%
    select(alimentaria, municipio) %>%
    filter(municipio == city) %>%
    group_by(alimentaria, municipio) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(municipio == city) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme_classic()+
    theme(legend.position="none")
}

security_city_plot(city = "ZARAGOZA")

security_city_df <- function(x = seguridad_alimentaria, city){
  sta <- x %>%
    select(alimentaria, municipio) %>%
    filter(municipio == city) %>%
    group_by(alimentaria, municipio) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(municipio == city) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total)
}

security_city_df(city = "ZARAGOZA")

# seguridad alimentaria por localidad

security_town_plot <- function(x = seguridad_alimentaria, town){
  sta <- x %>%
    select(alimentaria, localidad) %>%
    filter(localidad == town) %>%
    group_by(alimentaria, localidad) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(localidad == town) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme_classic()+
    theme(legend.position="none")
}

security_town_plot(town = "TENANGO")

security_town_df <- function(x = seguridad_alimentaria, town){
  sta <- x %>%
    select(alimentaria, localidad) %>%
    filter(localidad == town) %>%
    group_by(alimentaria, localidad) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(localidad == town) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total)
}

security_town_df(town = "TENANGO")

# seguridad alimentaria por region (norte, sur, centro, cdmx)

security_region_plot <- function(x = seguridad_alimentaria, zone){
  sta <- x %>%
    select(alimentaria, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, region) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(region == zone) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme_classic()+
    theme(legend.position="none")
}

security_region_plot(zone = "sur")

security_region_df <- function(x = seguridad_alimentaria, zone){
  sta <- x %>%
    select(alimentaria, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, region) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(region == zone) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total)
}

security_region_df(zone = "sur")

# seguridad alimentaria por area (rural, urbano)

security_estrato_plot <- function(x = seguridad_alimentaria, estrato){
  sta <- x %>%
    select(alimentaria, area) %>%
    filter(area == estrato) %>%
    group_by(alimentaria, area) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(area == estrato) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme_classic()+
    theme(legend.position="none")
}

security_estrato_plot(estrato = "rural")

security_estrato_df <- function(x = seguridad_alimentaria, estrato){
  sta <- x %>%
    select(alimentaria, area) %>%
    filter(area == estrato) %>%
    group_by(alimentaria, area) %>%
    count() %>%
    plyr::summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(area == estrato) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total)
}

security_estrato_df(estrato = "rural")
