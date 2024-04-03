#' @name GMP
#' @title title
#' @param chr_length Chromosome length information
#' @param gene_info Gene information
#' @param radian Chromosome drawing radian
#' @export GMP
#' @import grid
#' @import tidyverse
#' @import dplyr
#' @examples
#' library(GMP)
#' chr_length_default <- data.frame(chromosome = c(paste0("Chr",1:10)),
#'                                  chr_length = c(301284077, 237032434, 232132502, 241403054, 217807808,
#'                                                 169151979, 176396028, 175788001, 156637819, 150177341),
#'                                  fill = rep("slategray2",10),
#'                                  color = rep("black",10))
#' gene_info_default <- data.frame(chromosome = c("Chr3","Chr4","Chr8","Chr3","Chr6","Chr6","Chr4"),
#'                                 start = c(1260877,233800183,158089073,210103369,3052152,88701559,82892492),
#'                                 end = c(1262711,233811237,158099500,210107016,3054637,88711918,82893957),
#'                                 label = c("wrky93","cts3","vln2","RINGLET 2","Rpl19","CRK23","myb28"),
#'                                 gene_color = rep("black",7),
#'                                 label_color = rep("red",7))
#'GMP(chr_length = chr_length_default,gene_info = gene_info_default)
GMP <- function(chr_length = NULL, gene_info = NULL,radian = 0) {
  grid.newpage()
  chr_pos_x <- data.frame(chromosome = character(0),chr_pos_x = numeric(0))
  for (i in 1:nrow(chr_length_default)) {
    y_position = 0.9 - (0.8 * chr_length$chr_length[i] / max(chr_length$chr_length) / 2)
    height = 0.8 * chr_length$chr_length[i] / max(chr_length$chr_length)
    grid.roundrect(x = unit(0.1 + 0.8 / (nrow(chr_length_default) * 2 - 1) * (2 * i - 1), "npc"),
                   y = unit(y_position, "npc"),
                   width = unit(0.8 / (nrow(chr_length_default) * 2 - 1), "npc"),
                   height = unit(height, "npc"),
                   r = unit(radian, "npc"),
                   gp = gpar(fill = chr_length$fill[i], col = chr_length$color[i], lwd = 2))
    chr_pos_x <- rbind(chr_pos_x, data.frame(chromosome =paste0("Chr", i),
                                             chr_pos_x = 0.1 + 0.8 / (nrow(chr_length_default) * 2 - 1) * (2 * i - 1),
                                             chr_pos_x_1 = 0.1 + 0.8 / (nrow(chr_length_default) * 2 - 1) * (2 * i - 1.5),
                                             chr_pos_x_2 = 0.1 + 0.8 / (nrow(chr_length_default) * 2 - 1) * (2 * i - 0.5)))
    grid.text(chr_length$chromosome[i],
              x = unit(0.1 + 0.8 / (nrow(chr_length_default) * 2 - 1) * (2 * i - 1), "npc"),
              y = unit(0.95, "npc"),
              gp = gpar(col = "black", fontsize = 14))
  }
  chr_pos_x$chr_length <- chr_length$chr_length
  chr_length_max <- max(chr_length$chr_length)
  gene_info_pos_lab <- chr_pos_x %>%
    left_join(gene_info,by = "chromosome") %>%
    filter(start != "NA") %>%
    mutate(chr_pos_y =0.9 - (0.8 * (start+(end-start)/ 2) /chr_length_max),
           height = (end-start)/chr_length*0.8) %>%
    relocate(chr_pos_y,.after = chr_pos_x_2)
  for (j in 1:nrow(gene_info_pos_lab)) {
    grid.roundrect(x = unit(gene_info_pos_lab$chr_pos_x[j], "npc"),
                   y = unit(gene_info_pos_lab$chr_pos_y[j], "npc"),
                   width = unit(0.8 / (nrow(chr_length_default) * 2 - 1), "npc"),
                   height = unit(gene_info_pos_lab$height[j], "npc"),
                   r = unit(0.5, "npc"),
                   just="centre",
                   gp = gpar(fill = gene_info_pos_lab$gene_color,
                             col = gene_info_pos_lab$gene_color, lwd = 2))
    grid.text(gene_info_pos_lab$label[j],
              x = unit(gene_info_pos_lab$chr_pos_x_2[j], "npc"),
              y = unit(gene_info_pos_lab$chr_pos_y[j], "npc"),
              just = "left",
              gp = gpar(col = gene_info_pos_lab$label_color[j], fontsize = 10))
  }
  cat("\033[1;31m", "Finish plot !!!", "\033[0m\n")
  return(gene_info_pos_lab %>% select(label,chromosome,start,end,gene_color,label_color))
}
