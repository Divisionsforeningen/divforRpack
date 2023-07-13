add_events <- function(df=NA, x=NA,y=NA, xend=NA, yend=NA, pCol="steelblue", heatmap=FALSE, shot=FALSE, shot_args=list(), hmtype=NA){
  # TODO Write basic x,y points
  # TODO Write arrows x,y, & endx,endy
  # TODO Add heatmap functionality - start, end or all as argument
  # TODO Make usable across providers


    if(is.na(xend) && is.na(yend) && shot==F){
      p=list(geom_point(data=df, aes(x=x,y=y), color=pCol, size=10),
             theme_pitch())

      if(heatmap==TRUE){
        print("Add heatmap!")
        #p=append(p,
        # heatmap function)
      }

      return(p)
    }

    else if (shot==FALSE){
      #a <- geom_line()
    }

    else if (shot==TRUE){
      s=list(geom_point(data=df, aes(x=x,y=y), color="Red", size=10),
             theme_pitch(),
             coord_flip())
      return(s)
    }

    else{stop("Something went wrong...")}
}

a=data.frame(x=c(25,50,75), y=c(50,25,75), xend=50, yend=50)

ggplot()+
  annotate_pitch(dimensions = pitch_opta)+
  add_events(df=a, x=x, y=y, shot = F, heatmap=T)
