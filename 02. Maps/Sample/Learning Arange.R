library(grid)
library(here)
library(glue)
rect <- rectGrob(
  x = unit(1, "in"),
  y = unit(1, "npc") - unit(1, "in"),
  width = unit(1, "in"),
  height = unit(1, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "skyblue2", alpha = 0.5)
)

ggdraw(p) +
  draw_grob(rect)

?linesGrob()
grid.lines()


grid.newpage()
grid.polyline(x=c((0:4)/10, rep(.5, 5), (10:6)/10, rep(.5, 5)),
              y=c(rep(.5, 5), (10:6/10), rep(.5, 5), (0:4)/10),
              id=rep(1:5, 4),
              gp=gpar(col=1:5, lwd=3))
# Using id.lengths
grid.newpage()
grid.polyline(x=outer(c(0, .5, 1, .5), 5:1/5),
              y=outer(c(.5, 1, .5, 0), 5:1/5),
              id.lengths=rep(4, 5),
              gp=gpar(col=1:5, lwd=3))


p

lg <- linesGrob(unit(c(0, 0.5), "npc"), y = unit(c(0, 0.5), "npc"))

ggdraw() + draw_grob(lg) +
draw_grob(lg, x = 0.5, y=0.1, width = 0.2, height = 0.2)

grid.lines


###########################################################
chic <- readr::read_csv("https://raw.githubusercontent.com/Z3tt/R-Tutorials/master/ggplot2/chicago-nmmaps.csv")
tibble::glimpse(chic)



###################### THEME 
cust_theme_base <-theme_bw() + theme(
  # plot.background = element_rect(fill = "transparent", colour = 'transparent'),
  plot.background = element_rect(fill = "#2B303D", color = "#2B303D"),
  panel.background = element_rect(fill = "#2B303D", color = "#2B303D"), 
  panel.grid.major = element_line(color = "#8A8A8A"),
  panel.grid.minor = element_line(color = "#8A8A8A"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  plot.title = element_text(size = 8, hjust = 0.5),
  axis.title = element_blank(), #element_text(size = 7, vjust = 1,face = 'bold'),
  text = element_text(color = "white",size = 8),
  axis.text= element_text(color = "white", size =8 ),# , size = 7
  
  strip.background = element_rect(fill = 'gray30', color=NA),
  strip.text = element_text(face = 'bold', colour ='white',vjust = 1, size =9),
  strip.placement = "outside",
  # axis.title = element_text(face='bold')
)





########################################
# xmin: 2.668432 xmax: 14.67808  
# ymin: 4.277144  ymax: 13.90103


plt_map2 <- suppressWarnings(plt_map +
  coord_sf(clip = 'off', xlim = c(1.5,15), ylim = c(1.5,20)) +
  theme(
    legend.position = 'none'
  )
)
ggdraw(ggplot() + theme(
  plot.background = element_rect(fill = "#2B303D", color = "#2B303D"),
  panel.background = element_rect(fill = "#2B303D", color = "#2B303D"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()#,
  # plot.margin = margin(l = 10, t=10, unit = "cm")
)) +
  draw_plot(plt_map2, x=-0.05,y=0, scale=0.88) +
  draw_plot(plt_area_nw, scale = 0.28, x = -0.22, y = 0.28,width = 0.94,height =  0.9) + #UL
  draw_plot(plt_area_ne, scale = 0.28, x = 0.22, y = 0.34,width = 0.9, height = 0.8) + #UR
  draw_plot(plt_area_nc, scale = 0.275, x = -0.3, y = -0.02,width = 0.94, height =  0.9) + #ML
  draw_plot(plt_area_se, scale = 0.28, x = 0.34, y = 0.035,width = 0.9, height = 0.8)  + # MR
  draw_plot(plt_area_sw, scale = 0.28, x = -0.25, y = -0.38) + #BL
  # draw_plot(plt_area_ss, scale = 0.28, x = 0.25, y = -0.38) # BR
  
  
  ggsave(here("02. Maps", "image", "03 - Summary",
              glue("plot_{format(Sys.time(),'%d%m%H%M%S')}.png")), 
         width = 17, height = 11)







## Main Plotting
suppressWarnings(
  
  ggdraw(plt_map2,clip = 'off') +
    draw_plot(plt_area_nw,scale = 0.285, x = -0.29, y = 0.265, 
              width = 1,height = 1)
  
)



suppressWarnings(

ggdraw(plt_map2,clip = 'off') +
  # draw_plot(, scale = 0.25, x = 0.35, y = 0.3) +
  draw_plot(plt_area_nw, scale = 0.285, x = -0.29, y = 0.265) + #UL
  draw_plot(plt_area_ne, scale = 0.27, x = 0.34, y = 0.275) + #UR
  draw_plot(plt_area_nc, scale = 0.29, x = -0.345, y = -0.05) + #ML
  draw_plot(plt_area_se, scale = 0.28, x = 0.34, y = -0.05)  + # MR
  draw_plot(plt_area_sw, scale = 0.28, x = -0.25, y = -0.38) + #BL
  draw_plot(plt_area_ss, scale = 0.28, x = 0.25, y = -0.38) # BR
  


)

ggsave(here("02. Maps", "image", "03 - Summary",
            glue("plot_{format(Sys.time(),'%d%m%H%M%S')}.png")),
       # plot = plot, 
       width = 12, height = 10)











