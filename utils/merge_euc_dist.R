"
Usage: script.R [--expes=<e>] [--width=<w>] [--height=<h>] [--output_dir=<o>]

Options:
  -e --expes=<e>         list of experiments in the form name1:rds_path1,name2:rds_path2,...
  -w --width=<w>         Output width in inches [default: 7]
  -h --height=<h>        Output height in inches [default: 4]
  -o --output_dir=<o>    Output directory
" -> doc

# attach libraries
library(docopt)
library(ggplot2)
library(tikzDevice)


# parse args
args <- docopt(doc)
data.all <- NULL

for (e in strsplit(args$expes, ",")[[1]])
{
    # Get expe infos
    infos <- strsplit(e, ":")[[1]]

    # Load expes data
    tmp <- readRDS(infos[2])
    tmp$expe <- infos[1]

    # Add the expe data to the common pot
    data.all <- rbind(data.all, tmp)
}


for (cur_axis in c("x", "y", "z", "distance"))
{
    print(sprintf("dealing with axis %s", cur_axis))
    data <- subset(data.all,
                   coil %in% c("T1", "T2", "T3") &
                   axis == cur_axis
                   )

    ylim1 = boxplot.stats(data$value)$stats[c(1, 5)]
    p <- ggplot(data) +
        geom_boxplot(aes(x=expe, y=value, color=expe),,outlier.colour = NA) +
        facet_grid(coil ~ phone_class, scales = "free_y") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(legend.position = "bottom") +
        labs(x = "Experimental setup", y = "Euclidian distance (mm)", colour="") +
        coord_cartesian(ylim = ylim1*1.2)

    options(tikzDocumentDeclaration = "\\documentclass{IEEEtran}\n")
    tikz(sprintf("%s/%s_dist.tex", args$output_dir, cur_axis), sanitize = TRUE, standAlone = TRUE, width = args$width, height = args$height)
    print(p)
    dev.off()
}
