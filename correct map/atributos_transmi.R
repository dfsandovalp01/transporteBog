{
  library(openxlsx)
  library(tidyr)
  library(dplyr)
  library(splitstackshape)
  library(readr)
  #library(tm)
  library(lubridate)
  library(rowr)
  library(igraph)
  library(networkD3)
  library(tidyverse)
  library(viridis)
}


add_atribut_transmi <-new.vertices %>%
  semi_join(portales, by = c("V1"="port")) %>%
  mutate(attri = "PORT") %>%
  select("new.id", "attri") %>%
  mutate(new.id = as.character(new.id))

dsfz <- caminos.conectores.corr %>%
  add_rownames() %>%
  filter(SUMA.x > 2 | SUMA.y > 2) %>%
  select(V2, V3, SUMA.x, SUMA.y, new.id.x, new.id.y, V4, V5) 

origen.para.marcar <- origenes.id %>%
  select(ori1) %>%
  distinct() %>%
  left_join(new.vertices, by = c("ori1"="V1")) %>%
  mutate(attri = "ORI",
         new.id = as.character(new.id)) %>%
  select(new.id, attri)
  

VERTEX <- new.vertices %>%
  mutate (new.id= as.character(new.id)) %>%
  left_join(add_atribut_transmi, by = c("new.id")) %>%
  left_join(origen.para.marcar, by = c("new.id")) %>%
  mutate(V4 = ifelse(!is.na(attri.x) == T,
                     attri.x,
                     ifelse(!is.na(attri.y) == T,
                            attri.y,
                            "STOP")))%>%
  select(new.id, V2, V3, V4) %>%
  rename("V1" = "new.id")

write.table(VERTEX, "/home/dfsandovalp/WORK/transporteBog/streets/verticesAtri1.csv", sep = "\t", col.names = FALSE, row.names = FALSE)
