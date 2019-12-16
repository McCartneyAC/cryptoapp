library(tidyverse)

lets5<-data.frame(
    "lets" = LETTERS,
    "x" = rep(1:26, each=26),
    "y" = rep(1:26, times = 26)
)

lets6 <- lets5  %>% 
    filter(y != (x - 1))  %>% 
    mutate(y2 = rep(1:26, length.out=651))

lets_shift <- lets6  %>% 
    mutate(shiftedx = ifelse(((x + y2) >= 29), (x-1), x))
    
shift_add <- data.frame(
    "lets" = LETTERS, 
    "shiftedx" = rep(26, length.out= 26), 
    "y2" = 2:27
)  %>% 
    filter(y2 != 27)
    
lets_final <-bind_rows(lets_shift, shift_add)

lets_final %>%  
    ggplot(aes(y = y2, x = shiftedx, label = .$lets)) +
    geom_tile(fill = "#FFFFFF", color = "black") + 
    geom_text(position = "identity") +
    scale_y_reverse(breaks = 1:26) + 
    theme_void() + 
    scale_x_continuous(breaks = 1:26) + 
    labs(title = "Tabula Recta") + 
    NULL
