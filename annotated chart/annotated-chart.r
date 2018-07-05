
  #===============================================================
  #
  # Nice annotated chart using ggplot
  # 
  # Source: https://rud.is/b/2016/03/16/supreme-annotations/
  #
  #===============================================================
  
  rm(list  =  ls())
  library(here)
  library(tidyverse)
  
  dat <- read.csv(here("annotated chart", "supreme_court_vacancies.csv"), col.names = c("year", "wait"))
  
  # We only want every other tick labeled
  xlabs <- seq(1780, 2020, by = 10)
  xlabs[seq(2, 24, by = 2)]  <-  " "
  
  # Pretty long caption
  caption <- "Note: Vacancies are counted as the number of days between a justice's death, retirement or resignation and the successor justice's swearing in (or commissioning in the case of a recess appointment) as a member of the court.Sources: U.S. Senate, 'Supreme Court Nominations, present-1789'; Supreme Court, 'Members of the Supreme Court of the United States'; Pew Research Center calculations"
  caption <- label_wrap_gen(160)(caption)
  
  # Annotation table
  annot <- read.table(text = 
                      "year|wait|just|text
                      1848|860|0|Robert Cooper Grier was sworn in Aug 10, 1846,<br>841 days after the death of Henry Baldwin
                      1969|440|1|Henry Blackmun was sworn<br>in June 9, 1970, 391 days<br>after Abe Fortas resigned.
                      1990|290|0|Anthony Kennedy<br>was sworn in Feb.<br>18, 1988, 237<br>days after Lewis<br>Powell retired.",
                      sep = "|", header = TRUE, stringsAsFactors = FALSE)
  annot$text <- gsub("<br>", "\n", annot$text)
  
  gg <- ggplot() + 
          geom_point(data = dat, aes(x = year, y = wait)) + 
          geom_label(aes(x = 1780, y = 900, label = "days"), size = 3.5, hjust = 0.1, label.size = 0, color = "grey50") + 
          geom_label(data = annot, aes(x = year, y = wait, label = text, hjust = just), lineheight = 0.95, size = 3, label.size = 0, color = "grey50") + 
          scale_x_continuous(expand = c(0,0),
                             breaks = seq(1780, 2020, by = 10),
                             labels = xlabs, limits = c(1780,2020)) + 
          scale_y_continuous(expand = c(0,10),
                             breaks = seq(100, 900, by = 100),
                             limits = c(0, 1000)) + 
          labs(x = NULL, 
               y = NULL,
               title = "Lengthy Supreme Court vacancies are rare now, but weren't always",
               subtitle = "Supreme Court vacancies, by duration",
               caption = caption)
  
  # add theme
  gg <- gg + theme_minimal()
  # breathing room for the plot
  gg <- gg + theme(plot.margin = unit(rep(0.5, 4), "cm"))
  # light, dotted major y-grid lines only
  gg <- gg + theme(panel.grid = element_line())
  gg <- gg + theme(panel.grid.major.y = element_line(color = "grey50", linetype = "dotted", size = 0.15))
  gg <- gg + theme(panel.grid.major.x = element_blank())
  gg <- gg + theme(panel.grid.minor.x = element_blank())
  gg <- gg + theme(panel.grid.minor.y = element_blank())
  # make the plot title bold and modify the bottom margin a bit
  gg <- gg + theme(plot.title = element_text(face = "bold", margin = margin(b = 15)))
  # make the subtitle italic 
  gg <- gg + theme(plot.subtitle = element_text(face = "italic"))
  # make the caption smaller, left-justified and give it some room from the main part of the panel
  gg <- gg + theme(plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 15)))
  # light x-axis line only
  gg <- gg + theme(axis.line.x = element_line(color = "grey50", size = 0.15))
  gg
  