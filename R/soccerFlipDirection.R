soccerFlipDirection <- function(df, lengthPitch = 105, widthPitch = 68, 
                                teamToFlip = NULL, periodToFlip = 1:2, team = "team", 
                                period = "period", x = "x", y = "y") {
        
        if(is.null(teamToFlip)) teamToFlip <- unique(df[,team])[1]
        
                df <- df %>%
                        mutate(!!x := if_else(!!sym(team) == teamToFlip & !!sym(period) %in% periodToFlip, 
                                              lengthPitch - !!sym(x), !!sym(x)) ,
                               !!y := if_else(!!sym(team) != teamToFlip & !!sym(period) %in% periodToFlip, 
                                             widthPitch - !!sym(y), !!sym(y)))       
        return(df)
}
