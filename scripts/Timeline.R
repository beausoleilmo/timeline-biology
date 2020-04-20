library(ggplot2)
library(scales)
library(lubridate)
library(openxlsx)
library(plotly)

df <- read.xlsx(sheet = "Timeliner",
                #sheet = "Timeline Birds",
                xlsxFile = '~/Dropbox/Timeline.xlsx')

# df$Event[grep(pattern = "\r\n",x = df$Event)]
df$Event2 = gsub(pattern = "\r\n",replacement = " ",x = df$Event)

head(df)
# df.small = df[18:22,]
# df = df.small
status_levels <- c(sort(unique(df$Type))) #c("birth or death", "literature", "discovery","history")
# df=df[df$Year > 1925 & df$Year < 1950,]
df=df[df$Year > -2000,]
if (length(which(is.na(df$Year)))!=0) {
df = df[-which(is.na(df$Year)),]
}

if(length(which(status_levels=="none"))==0){
status_levels = c(status_levels,"none")
}

df$status <- factor(df$Type, levels = status_levels, ordered=FALSE)
df$status[is.na(df$status)] <- as.factor("none")
status_colors <- c("#0070C0", "#00B050", "#FFC000", "#C00000","darkgreen","purple","darkgrey","blue","salmon","darkorange","black","navy","darkblue")
positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
  "date"=(df$Year),
  "position"=rep(positions, length.out=length((df$Year))),
  "position2"=runif(length(df$Year),0,1),
  # "position"=df$height,
  "direction"=rep(directions, length.out=length((df$Year)))
)
df$pos1 = rep(positions, length.out=length((df$Year)))
df$pos2 = runif(length(df$Year),0,1)
df$pos2 = df$pos2 * c(1,-1)
df[df$pos2 >= -0.05 & df$pos2 <= 0,"pos2"] <- df[df$pos2 >= -0.05 & df$pos2 <= 0,"pos2"]-0.05
df[df$pos2 <= 0.05 & df$pos2 >= 0,"pos2"] <- df[df$pos2 <= 0.05 & df$pos2 >= 0,"pos2"]+0.05
df$dir = rep(directions, length.out=length((df$Year)))

year.dup = unique(df$Year[which(duplicated(df$Year))])
year.dup = na.omit(year.dup)
df$new.year = df$Year

if (length(year.dup)!=0) {
for (i in 1:length(year.dup)) {
  y.vec = na.omit(df[df$Year==year.dup[i],"Year"])
  lengtmp = length(y.vec)
  for (j in 1:lengtmp) {
    y.vec[j] <- y.vec[j]+0.1*(j-1)
  }
  df[df$Year==year.dup[i],"new.year"] <- y.vec
}
  table.dup.year = table(df[df$Year %in% year.dup,"Year"])
  year.more.2 = names(table.dup.year[which(table.dup.year >2 )])
  
  for (k in 1:length(year.more.2)) {
    year.seq.large.gap = seq(-1,1,
                             length.out = length(df[df$Year %in% year.more.2[k],"pos2"]))
    if(length(year.seq.large.gap) %% 2 == 0){
      year.seq.large.gap = year.seq.large.gap * c(-1,1,1)
    } else {year.seq.large.gap = year.seq.large.gap * c(-1,1,1,-1)
    }
      
    length(which(year.seq.large.gap==10))
    year.seq.large.gap[year.seq.large.gap==0]<-0.05
    df[df$Year %in% year.more.2[k],"pos2"] <- year.seq.large.gap
  }
  
}

# df <- merge(x=df, y=line_pos, by.x ="Year", by.y = "date", all = TRUE)
df <- df[with(df, order(Year)), ]

head(df)
text_offset <- 0.05
df[df$Year == 2007,]
# df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
# df$text_position <- (df$month_count * text_offset * df$direction) + df$position
# head(df)
# month_buffer <- 2

# month_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='month')
# month_format <- format(month_date_range, '%b')
# month_df <- data.frame(month_date_range, month_format)
# year_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='year')
# year_date_range <- as.Date(
#   intersect(
#     ceiling_date(year_date_range, unit="year"),
#     floor_date(year_date_range, unit="year")
#   ),  origin = "1970-01-01"
# )

# year_format <- format(year_date_range, '%Y')
# year_df <- data.frame(year_date_range, year_format)

df = df[-which(is.na(df$Event2)),]
wrapper <- function(x, wd=12) paste(strwrap(x, width = wd), collapse = "\n")
df$evtwrap=sapply(df$Event2, wrapper)
wrapper(df$Event2,15)

ggempty <- ggplot(df,aes(x=new.year, y=0, 
                         col = status)) + 
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0, color = "black", size=0.3) +
  labs(col="Milestones") + 
  scale_color_manual(values=status_colors,
                     labels=status_levels, drop = FALSE) +
  theme_classic() + 
  # Don't show axes, appropriately position legend
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        # axis.text.x =element_blank(),
        # axis.ticks.x =element_blank(),
        # axis.line.x =element_blank(),
        legend.position = "bottom")

pos.lab = data.frame(year= unique(df$Year))
df[grep("2007",df$new.year),]

# Plot vertical segment lines for milestones
tl_line1 <- ggempty + 
  geom_segment(data = df, aes(y = pos2,
                              yend = 0,
                              x = new.year,
                              xend = new.year, col = status), 
               # color = 'black', 
               size = 0.2) + 
  # Plot scatter points at zero and date
  geom_point(data = df, aes(x = new.year, y = 0), size=3) +
  # Show year text
  # geom_text(data = pos.lab, aes(x = year, y = 0, #y = -0.1, 
  #                               label = year, 
  #                          fontface = "bold"),
  #           size = 2.5, vjust = 0.5, color = 'black', angle = 90)
  geom_label(data = pos.lab, aes(x = year, y = 0, #y = -0.1, 
                                label = year, 
                           fontface = "bold"),
            size = 2.5, color = 'black')

# Show text for each month
# timeline_plot <- timeline_plot + 
#   geom_text(data = month_df, aes(x = month_date_range, y = -0.1, label = month_format),
#             size = 2.5, vjust = 0.5, color = 'black', angle = 90)
# Show text for each milestone
library(ggrepel)

timeline_plot <- tl_line1 #+ geom_text(aes(x = new.year, y = pos2, label = evtwrap), size = 2.5) 
timeline_plot
# timeline_plot+geom_text_repel(aes(x=Year, y=pos2,label=Event2))

# pdf(file = "~/Desktop/timelineR.pdf",width = 10,height = 5)
# print(timeline_plot)
# dev.off()


fig = ggplotly(timeline_plot, tooltip = NULL)
fig %>%  
  add_text(
    x = df$new.year,
    y = ifelse(df$pos2>0,df$pos2+0.05,df$pos2-0.05),
    text = df$evtwrap,
    hovertext = df$description,
    hoverinfo = 'text',
    mode ="text",
    textfont = list(color=status_colors[df$status], size =10),
    marker = list(color=status_colors[df$status], size = 0.00001),
    showlegend = T,
    textposition = ifelse(df$pos2>0,"top center","bottom center")
  ) %>%
  add_text(
    x = pos.lab$year,
    y = -0.05,
    text = pos.lab$year,
    mode ="text",
    hovertext = "none",
    hoverinfo = "none",
    textfont = list(color="black", size =12),
    marker = list(color="black", size = 0.00001),
    showlegend = F,
    textposition = "bottomleft"
  ) %>%
  # add_annotations(
  #   x = df$new.year, 
  #   y = ifelse(df$pos2>0,df$pos2+0.05,df$pos2-0.05),
  #   text = df$evtwrap,
  #   hovertext = df$description,
  #   hoverinfo = 'text',
  #   mode ="text",
  #   textfont = list(color=status_colors[df$status], size =10),
  #   marker = list(color=status_colors[df$status], size = 0.00001),
  #   showlegend = F,
  #   textposition = ifelse(df$pos2>0,"top center","bottom center")
  # ) %>% 
  layout(#title = list(text = "Biology history timeline", y = 0.8),
    title = "Biology history timeline",
         showlegend = FALSE,
         xaxis = list(range = c(2000, 2010),
                      rangeselector = list(buttons = list(list(step = "all"))),
                      rangeslider = list(type = "date")
  ), 
         yaxis = list(range = c(-1.5,1.5)),
         dragmode = "pan")

