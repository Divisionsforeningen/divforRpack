add_events <- function(df=NA, x=NA,y=NA, xend=NA, yend=NA,
                       pCol="steelblue", lCol="black",
                       heatmap=FALSE, shot=FALSE, lines=FALSE,
                       shot_args=list(),
                       hmtype=NA){
  # TODO Write basic x,y points
  # TODO Write arrows x,y, & endx,endy
  # TODO Add heatmap functionality - start, end or all as argument
  # TODO Make usable across providers


    if(shot==F){
      p=list(geom_point())

      if(heatmap==TRUE){
        print("Add heatmap!")
        #p=append(p,
        # heatmap function)
      }

      if(lines==TRUE){
        print("Add lines!")
        p=append(p,
        geom_segment(data=df, aes(x=x, y=y, xend=xend, yend=yend), color=lCol,
          arrow = arrow(length=unit(.25, 'cm')))
        )
      }

      p=append(p,
               c(geom_point(data=df, aes(x=x,y=y), color=pCol, size=10),
               theme_pitch())
               )

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


a=data.frame(x=c(25,50,75), y=c(50,25,75), xend=100, yend=50)

ggplot2::ggplot()+
  divforRpack::annotate_pitch(dimensions = pitch_opta)+
  add_events(df=a, x=x, y=y, shot = F, heatmap=F, lines=T)
