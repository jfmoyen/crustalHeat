library(devtools)

document()
load_all()

t_T_surf <- 0
t_rho <- 2700
t_Cp <- 1000
t_z_moho_km <- 30
t_k <- 2
t_q_moho_mw <- 30
t_HP_muW <- 0.65
t_geoth_param <- "k"
t_geoth_param_from <- 2
t_geoth_param_to <- 3
t_n_curves <- 7
t_N <- 100

## Should be defined in app
t_UItoSI <- c(z_moho=1000,
              q_moho=1e-3,
              HP=1e-6,
              k=1,
              rho=1,
              Cp=1,
              u=1e-3*1e6)



t_ee <-steady_geotherm_bundle(T_surf = t_T_surf,
                           rho = t_rho,
                           Cp = t_Cp,
                           z_moho = t_z_moho_km,
                           k = t_k,
                           q_moho = t_q_moho_mw,
                           HP = t_HP_muW,
                           varying = t_geoth_param,
                           varies_from = t_geoth_param_from,
                           varies_to = t_geoth_param_to,
                           n_curves = 1,
                           UItoSI = t_UItoSI,
                           N = t_N)



p1<-plotCanvas(faciesMeta)
p2<- addGeotherms(p1,t_ee)
  p2

# t_ee %>% ggplot()+
#   scale_x_continuous(limits=c(0,900),expand=c(0,0),
#                      name="Température (°C)",
#                      position="top")+
#   scale_y_continuous(limits=c(0,40000),expand=c(0,0),
#                      name="Profondeur (km)")+
#   geom_line(aes(x=Temp,y=Depth,
#                 colour= as.character(!!rlang::parse_expr(t_geoth_param))))+
#   theme_linedraw()
