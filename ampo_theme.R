factor_val <- 1.5
ampo_theme <- function(use_showtext = TRUE,
                       size_header = 22 * factor_val,
                       size_axis_title = 14* factor_val,
                       size_legend_title = 14* factor_val,
                       size_axis_text = 13* factor_val,
                       size_legend_text = 12* factor_val,
                       size_caption = 8* factor_val,
                       size_margin = 10* factor_val) {
  if (use_showtext == TRUE) {
    requireNamespace("sysfonts", quietly = TRUE)
    requireNamespace("showtext", quietly = TRUE)
    
    showtext::showtext_auto()
    sysfonts::font_paths()
    files <- sysfonts::font_files()
    sysfonts::font_add("HelveticaNeueLT Std Cn", "HelveticaNeueLTStd-Cn.otf")
    sysfonts::font_add("HelveticaNeueLT Std Lt", "HelveticaNeueLTStd-Lt.otf")
    sysfonts::font_add("Arial Narrow", "ARIALN.TTF")
    sysfonts::font_add("HelveticaNeueLT Std Med Cn", "HelveticaNeueLTStd-MdCn.otf")
    sysfonts::font_add("Palatino Linotype", "pala.ttf")
    
    
    font_title <- "HelveticaNeueLT Std Lt"
    font_legend_title <- "HelveticaNeueLT Std Med Cn"
    font_caption <- "Palatino Linotype"
    font_axis <- "Arial"
    font_legend_text <- "Arial"
    
  } else {
    font_title <- "sans"
    font_caption <- "serif"
    font_axis <- "sans"
  }
  
  
  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(size_margin, 
                                    size_margin, 
                                    size_margin, size_margin, "pt"),
      
      axis.title.x = ggplot2::element_text(
        vjust = -1,
        family = font_axis,
        size = size_axis_title
      ),
      axis.title.y = ggplot2::element_text(
        vjust = 2,
        family = font_axis,
        size = size_axis_title
      ),
      axis.text.x = ggplot2::element_text(
        family = font_axis,
        size = size_axis_text,
        vjust = 0.5
      ),
      axis.text.y = ggplot2::element_text(
        family = font_axis,
        size = size_axis_text,
        vjust = 1
      ),
      plot.caption = ggplot2::element_text(
        family = font_caption,
        hjust = 1,
        size = size_caption
      ),
      plot.title = ggplot2::element_text(
        family = font_title,
        # hjust = 0.5,
        size = size_header
      ),
      legend.title = ggplot2::element_text(
        family = font_legend_title,
        size = size_legend_title
      ),
      legend.text = ggplot2::element_text(
        family = font_legend_text,
        size = size_legend_text,
      ),
      strip.text.x = ggplot2::element_text(
        family = font_axis,
        size = size_axis_title
      )
    )
}