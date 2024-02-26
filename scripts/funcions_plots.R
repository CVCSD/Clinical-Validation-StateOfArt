
library(ggplot2)

generar_plot_estimacions_profile<-function() {
  
  # llistat_estimacions=llistat_Probs_estim_Ramo
  # vector_inc=vector_incidencies
  
  N_total=sum(dt_temp2$n)
  
  datos <- dt_temp2 %>% 
    mutate(Journal, categoria=Journal, valor=n) %>% 
    mutate(valor=(n/N_total)*100) %>% arrange(valor) %>% 
    mutate(categoria = reorder(categoria, valor)) %>% arrange(valor) %>% 
    filter(valor>1.5) %>%  filter(Journal!="Others")
  
  # # Generar dades 
  # datos <- data.frame(
  #   categoria = names(llistat_estimacions),
  #   valor = estim*100, 
  #   Inc_valor=vector_inc*100) %>% 
  #   etiquetar_taula(camp="categoria", path_conductor) %>% 
  #   mutate(categoria = reorder(categoria, valor))  %>% arrange(categoria)
  
  leyenda_puntos <- guide_legend(title = "Leyenda de Puntos")
  # Crea el gráfico de barras horizontales
  fig<-
    ggplot(datos, aes(x = valor, y = reorder(categoria, +valor), fill = valor)) +
    geom_bar(stat = "identity", orientation = "y",width = 0.5) +
    scale_fill_gradient(low = "grey", high = "orange")

  fig<-
    fig +
    geom_text(
      data = datos,
      mapping = aes(x = valor, y = categoria, label = round(valor,1)),
      
      hjust = -0.2,
      vjust = 1.75,
      nudge_y = 0.35,
      color = 'grey30',
      # fontface = 'bold',
      size = 3.5)
  
  fig + 
    geom_text(
      data = datos,
      mapping = aes(
        x = 0, 
        y = categoria, 
        label = categoria
      ),
      hjust = 0,
      vjust = 0,
      nudge_y = 0.35,
      nudge_x = 0.05,
      color = 'grey20',
      # fontface = 'bold',
      size = 3
    ) +
    
    labs(
      y = element_blank(), 
      x = '',
      title = 'Frequency of papers by scope (%)',
      caption = "Removed: < 1.5% & Others",
      fill="Percentage (%)"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = rel(1.1)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )+
    # geom_vline(xintercept = 0) +
    scale_y_discrete(breaks = NULL) +  
    scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0, 0.10))) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  
  
}

