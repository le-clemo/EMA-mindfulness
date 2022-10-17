#create common theme for plots

# Reduce the opacity of the grid lines: Default is 255
col_grid <- rgb(220, 220, 220, 62, maxColorValue = 255)

single_plot_theme <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5, margin = margin(0,0,20,0)),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line.x = element_line(colour="black"),
            axis.line.y = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid = element_line(color = col_grid),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vetical",
            legend.key.size= unit(0.5, "cm"),
            #legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#D5E4EB",fill="#D5E4EB"),
            strip.text = element_text(face="bold")
    ))
  
}
#"#D5E4EB" #f0f0f0

multi_plot_theme <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    +   theme(legend.position="none",
              panel.grid = element_line(color = col_grid),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background = element_rect(colour = NA, fill = NA),
              plot.background = element_rect(colour = NA, fill = NA),
              panel.border = element_rect(colour = NA, fill = NA),
              axis.text = element_text(), 
              axis.line.x = element_line(colour="black"),
              axis.line.y = element_line(colour="black"),
              axis.ticks = element_line(),
              panel.grid.minor = element_blank(),
              plot.margin=unit(c(10,5,5,5),"mm"),
              strip.background=element_rect(colour="#D5E4EB",fill="#D5E4EB"),
              strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#f87f01","#7fc97f","#ef3b2c","#feca01","#a6cee3","#fb9a99","#984ea3","#8C591D")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#f87f01","#7fc97f","#ef3b2c","#feca01","#a6cee3","#fb9a99","#984ea3","#8C591D")), ...)
  
}

