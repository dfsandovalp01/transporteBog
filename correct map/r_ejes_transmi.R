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

#### IMPORTANDO DATOS Y DERIVANDO ESTACIONES, PORTALES Y CONECTORES(ORIGENES) ####
{
  edges1 <- read.csv("/home/dfsandovalp/WORK/transporteBog/streets/edges (copia).csv", header = F)  %>%
  # edges1 <- read.csv("/home/dfsandovalp/Descargas/edges.csv", header = F)  %>%
  mutate(#V6 = paste(V2,"-",V3, sep = ""),
         #V7 = paste(V3,"-",V2, sep = ""),
         V2 = as.character(V2),
         V3 = as.character(V3)) %>%
  filter(V5 == 1)



count.vert1 <- edges1[3]
names(count.vert1) = c("V2")

count.vert <- edges1[2] %>% ###conteo de vertices
  rbind(count.vert1) %>%
  group_by(V2) %>%
  summarise(SUMA = n())

# Tabla con origenes (ejes con conteo)
conect.wit <- edges1 %>%
  left_join(count.vert, by = c("V2")) %>%
  left_join(count.vert, by = c("V3" = "V2"))

# Origenes *(Puntos union entre troncales)
origenes <- conect.wit %>%
  filter(SUMA.x > 2) %>%
  rbind(filter(conect.wit, SUMA.y >2)) %>%
  mutate(ori1 = ifelse(SUMA.x > 2, V2, V3),
         ori2 = ifelse(ori1 == V2, V3, V2)) %>%
  select(ori1, ori2) %>%
  distinct() %>% 
  arrange(ori1)

origenes.comp <-  conect.wit %>% 
  filter(V2 == origenes[1,1] & V3 == origenes[1,2] | V2 == origenes[1,2] & V3 == origenes[1,1] )


for (i in 2:nrow(origenes)){
  k <- conect.wit %>%
    filter(V2 == origenes[i,1] & V3 == origenes[i,2] | V2 == origenes[i,2] & V3 == origenes[i,1] )
  
  origenes.comp <- origenes.comp %>%
    rbind(k)
} 

# Portales
portales <-  conect.wit %>%
  filter(SUMA.x == 1) %>%
  rbind(filter(conect.wit, SUMA.y == 1)) %>%
  mutate(port = ifelse(SUMA.x == 1, V2, V3)) %>%
  select(port) %>%
  distinct() %>%
  add_rownames() 

# Estaciones
estaciones <- conect.wit %>%
  filter(SUMA.x < 3) %>%
  rbind(filter(conect.wit, SUMA.y < 3)) %>%
  mutate(stop = ifelse(SUMA.x < 3, V2, V3)) %>%
  select(stop) %>%
  distinct()

} #FIN DERIVACION ELEMENTOS

###
# Ordenando ejes

#sin.oirg <- conect.wit %>%
#  anti_join(origenes.comp, by = c("V1"))

#edges2 <- conect.wit %>%
#  filter(V2 == origenes[1,1] & V3 == origenes[1,2] | V2 == origenes[1,2] & V3 == origenes[1,1] )


# indicador util con anterior
#for (i in origenes$ori2){
#  edges.filt <- conect.wit %>%
#    filter(V2 == i | V3 == i )
  
#  edges2 <- edges2 %>%
#    rbind(edges.filt)%>%
#    distinct()
#} 
#### DIFERNECIANDO SI ORIGEN Y DESTINO Y UNIENDO ####
prueba <- conect.wit %>%
  semi_join(origenes, by = c("V2"="ori1")) %>%
  arrange(V2)

prueba1 <- conect.wit %>%
  semi_join(origenes, by = c("V3"="ori1"))%>%
  arrange(V2)

final.a <- prueba[1,]

for (i in 1:nrow(prueba)) {

  final <- prueba[i,]
  
  while (TRUE) {
    if (final[nrow(final), 7] == 1 | final[nrow(final), 7] > 2 ) 
      break
    else {
      
      edges.filt <- conect.wit %>%
        filter(V2 == final[nrow(final), 3] )
      
      final <- final %>%
        rbind(edges.filt)%>%
        distinct()
    }    
  }
  final.a <- final.a %>%
    rbind(final)%>%
    distinct()
}

final.b <- prueba1[1,]

for (i in 1:nrow(prueba1)) {
  
  final <- prueba1[i,]
  
  while (TRUE) {
    if (final[nrow(final), 6] == 1 | final[nrow(final), 6] > 2 ) 
      break
    else {
      
      edges.filt <- conect.wit %>%
        filter(V3 == final[nrow(final), 2] )
      
      final <- final %>%
        rbind(edges.filt)%>%
        distinct()
    }    
  }
  final.b <- final.b %>%
    rbind(final)%>%
    distinct()
}

# Esta es la lista continua de ejes
ejes.cont <- final.a %>%
  rbind(final.b) %>%
  distinct()

# Estos son los origenes que se conectan
conteo <- final.a %>%
  #rbind(final.b)
  semi_join(final.b, by = c("V1")) %>%
  mutate(origen = ifelse(SUMA.x > 2, 
                         V2,
                         ifelse(SUMA.y > 2,
                                V3,
                                0))) %>%
  filter(origen != 0)

#### EJES INMEDIATOS DESDE ORIGENES Y GRAFICA RELACION DE ORIGENES ####
{
  
  from_nodes <- conteo %>%
    select(origen) %>%
    add_rownames() %>%
    mutate(rowname = as.numeric(rowname),
           par = rowname %% 2) %>%
    filter(par == 1 ) %>%
    select(origen)
  
  to_nodes <- conteo %>%
    select(origen) %>%
    add_rownames() %>%
    mutate(rowname = as.numeric(rowname),
           par = rowname %% 2) %>%
    filter(par == 0) %>%
    select(origen)
  
  #Graficando
  
  links <- from_nodes %>%
    cbind(to_nodes)
  names(links) = c("source", "target")
  network <- graph_from_data_frame(d=links, directed=T) 
  
  # plot it
  plot(network)

  } #FIN DE GRAFICA

  #### ARREGLO MANUAL 1 DE NUMERACION SEGUN GRAFIA ####
{
#   ejes.ord <- links %>%
#   mutate(source = as.numeric(source),
#          target = as.numeric(target),
#          id.suma = source + target )
# 
# origenes.id <- origenes %>%
#   mutate(id.ori = ifelse(ori1==23,
#                      1,
#                      ifelse(ori1==44,
#                             2,
#                             ifelse(ori1==135,
#                                    3,
#                                    ifelse(ori1==111,
#                                           4,
#                                           ifelse(ori1==47,
#                                                  5,
#                                                  ifelse(ori1==96,
#                                                         6,""))))))) %>%
#   arrange(id.ori)


origenes.id <- origenes %>%
  mutate(id.ori = ifelse(ori1==23,
                         1,
                         ifelse(ori1==43,
                                2,
                                ifelse(ori1==96,
                                       3,
                                       ifelse(ori1==47,
                                              4,
                                              ifelse(ori1==81,
                                                     5,
                                                     ifelse(ori1==111,
                                                            6,ifelse(ori1==44,
                                                                     7,
                                                                     ifelse(ori1==135,
                                                                            8,""))))))))) %>%
  arrange(id.ori)



# ejes.ord <- links %>%
#   mutate(source = as.numeric(source),  ESTOS ESTABAN BLOQUEADOS
#          target = as.numeric(target),
#          id.suma = source + target )
} #FIN ARREGLO MANUAL 1
{
conteo1 <- final.a %>%
  #rbind(final.b)
  semi_join(final.b, by = c("V1"))

ejes.base <- conteo1 %>%
  left_join(select(origenes.id, ori1, id.ori), by = c("V2"="ori1")) %>%
  distinct() %>%
  left_join(select(origenes.id, ori1, id.ori), by = c("V3"="ori1")) %>%
  distinct() %>%
  add_rownames() 

m <- ejes.base %>%
  mutate(rowname = as.numeric(rowname),
         id.ori.x = as.numeric(id.ori.x),
         id.ori.y = as.numeric(id.ori.y)) %>%
  filter( !is.na(id.ori.x) )

n <- ejes.base %>%
  mutate(rowname = as.numeric(rowname),
         id.ori.x = as.numeric(id.ori.x),
         id.ori.y = as.numeric(id.ori.y))
  

o <- m[1,]

  for (i in 1:nrow(m)) {
    
      final <- m[i,]
      
      a <- m[i,1] %>%
        as.numeric()
      b <- m[i+1,1] %>%
        as.numeric()
      c <- final[1,9] %>%
        as.numeric()
      
      if (i == nrow(m))
        
        o <- o %>%
        rbind(final)
      
      
      else {
        edges.filt <- ejes.base[a:b,] %>%
          mutate(id.ori.x = c,
                 rowname = as.numeric(rowname)) #%>%
         # anti_join(final, by = c("rowname"))
        edges.filt <- edges.filt[1:nrow(edges.filt)-1,]
        
        d <- edges.filt[nrow(edges.filt),10] %>%
          as.numeric()
        
        edges.filt <- edges.filt %>%
         mutate(id.ori.y = d)  
        
        final <- final %>%
          #filter(rowname=="a")%>%
          rbind(edges.filt)
        
        
        o <- o %>%
          rbind(final)
      }
    
    
  }
o <- o %>%
  distinct() %>%
  filter(!is.na(id.ori.y))
 

d <- ejes.base[nrow(ejes.base),1] %>%
  as.numeric()

e <- ejes.base[nrow(o),1] %>%
  as.numeric()

f <- ejes.base[nrow(o)+1,9] %>%
  as.numeric()

g <- ejes.base[nrow(ejes.base),10] %>%
  as.numeric()

edges.filt <- ejes.base[e:nrow(ejes.base),] %>%
  mutate(id.ori.x = f,
         id.ori.y = g)

edges.filt <- edges.filt[-1,]

o <- o %>%
  rbind(edges.filt)


### Ordenando por origen de conectores

caminos.conectores <- o %>%
  arrange(id.ori.x,id.ori.y, rowname) %>%
  mutate(origen = ifelse(id.ori.x < id.ori.y,
                         id.ori.x,
                         id.ori.y),
         destino = ifelse(id.ori.y < id.ori.x,
                         id.ori.x,
                         id.ori.y),
         rowname = as.numeric(rowname),
         id.orden = paste(origen, destino, sep = "."))
}
##### INICIO CAMINOS CONECTORES CORREGIDOS Y UTILES #####
{ 

  caminos.conectores.corr <- data_frame(rowname=NA,V1=NA,V2=NA,V3=NA,V4=NA,V5=NA,SUMA.x=NA,SUMA.y=NA,id.ori.x=NA,id.ori.y=NA,origen=NA,destino=NA,id.orden=NA,V2.1=NA,V3.1=NA)
  
  for (i in unique(caminos.conectores$id.orden)) {
    
    camino.filt <- caminos.conectores %>%
      filter(id.orden == i)
    
    if (camino.filt$id.ori.x > camino.filt$id.ori.y) {
      camino.filt <- camino.filt %>%
        mutate(V2.1 = V3,
               V3.1 = V2) %>%
        arrange(-rowname) 
        
    }
    else {
      camino.filt <- camino.filt %>%
        mutate(V2.1 = V2,
               V3.1 = V3)
    }
    
    caminos.conectores.corr <- caminos.conectores.corr %>%
      rbind(camino.filt)
  }
  
  caminos.conectores.corr <- caminos.conectores.corr %>%
    filter(!is.na(V2))
  
  
  # for (i in unique(caminos.conectores$id.orden)) {
  #   
  #   if (i == "3.4" | i == "4.5") {
  #     camino.filt <- caminos.conectores %>%
  #       filter(id.orden == i) %>%
  #       mutate(V2.1 = V2,
  #              V3.1 = V3)
  #     
  #     caminos.conectores.corr <- caminos.conectores.corr %>%
  #       rbind(camino.filt)
  #     }
  #   
  #   else {
  #     if (i == "4.6") {
  #       camino.filt <- caminos.conectores %>%
  #         filter(id.orden == i) %>%
  #         arrange(-rowname) %>%
  #         mutate(origen = 6,
  #                destino = 4,
  #                V2.1 = V3,
  #                V3.1 = V2)
  #       caminos.conectores.corr <- caminos.conectores.corr %>%
  #         rbind(camino.filt) %>%
  #         filter(!is.na(rowname))
  #     }
  #     
  #     else {
  #       camino.filt <- caminos.conectores %>%
  #         filter(id.orden == i) %>%
  #         arrange(-rowname) %>%
  #         mutate(V2.1 = V3,
  #                V3.1 = V2)
  #       caminos.conectores.corr <- caminos.conectores.corr %>%
  #         rbind(camino.filt) %>%
  #         filter(!is.na(rowname)) 
  #     }
  #     
  #   }
  # 
  #   
  # }
  # 
  # caminos.conectores.corr <- caminos.conectores.corr %>% # añadiendo rownames para mantener orden
  #   add_rownames() %>%
  #   arrange(origen, destino)
  
  conectores.numerados.1 <- caminos.conectores.corr %>%
    mutate(V2.1 == as.character(V2.1)) %>%
    select(V2.1) %>%
    distinct() %>%
    add_rownames() %>%
    mutate(new.id = as.numeric(rowname)-1) %>%
    select(V2.1, new.id)
  
  conectores.numerados.2 <- caminos.conectores.corr %>%
    anti_join(conectores.numerados.1, by = c("V3.1" = "V2.1")) %>%
    select(V3.1) %>%
    distinct() %>%
    mutate(new.id = as.numeric(nrow(conectores.numerados.1)))
  
  names(conectores.numerados.2) = c("V2.1", "new.id")
  
  conectores.numerados.1 <- conectores.numerados.1 %>%
    rbind(conectores.numerados.2)
    
  
  
     ####    ESTA PARTE SE ACTUALIZO
  # conectores.numerados.2 <- caminos.conectores.corr$V3.1 
  # names(conectores.numerados.2)  <- c("V2.1")
  # 
  # conectores.numerados <- data.frame(V2.1=NA, new.id = 0)
  # 
  # for (i in 1:nrow(caminos.conectores.corr)) {
  #   conectores.numerados.1 <- data.frame(vertices = caminos.conectores.corr[i,15], new.id = i) %>%
  #     add_row(V2.1 =caminos.conectores.corr[i,16], new.id = i+1)
  #   
  #   conectores.numerados <- conectores.numerados %>%
  #     rbind(conectores.numerados.1) %>%
  #     filter(!is.na(V2.1)) %>%
  #     as.data.frame()%>%
  #     distinct()
  #   
  # }
  # conectores.numerados <- as.data.frame(conectores.numerados)%>%
  #   mutate(V2.1 = as.character(V2.1),
  #          new.id = as.character(new.id))%>%
  #   distinct()
  # 
  # #### nuevos id para vertices de caminos conectores ####
  # conectores.numerados <- conectores.numerados[1:nrow(conectores.numerados)-1,] %>% # se descarta la ultima relacion ya que repite new id para un origen
  #   mutate(new.id = as.numeric(new.id)-1)
  
     ### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  caminos.conectores.corr <- caminos.conectores.corr %>%
    left_join(conectores.numerados.1, by = c("V2.1")) %>%
    left_join(conectores.numerados.1, by = c("V3.1" = "V2.1" ))
  
  edges.conectores <- caminos.conectores.corr %>%
    add_rownames() %>%
    select(rowname, new.id.x, new.id.y, V4, V5)
    
  names(edges.conectores) <-c("V1", "V2", "V3", "V4", "V5")
  
  edges.conectores <- edges.conectores %>% ###################################### Arreglo para corregir numeracion 
    mutate(V1 = as.numeric(V1)-1)#,
           # V2 = as.numeric(V2)-1,
           # V3 = as.numeric(V3)-1)
    
  #Graficando
  
  links.1 <- edges.conectores %>%
    mutate(source = V2,
           target = V3) %>%
    select(source, target)
  names(links.1) = c("source", "target")
  network.1 <- graph_from_data_frame(d=links.1, directed=T) 
  
  # plot it
  plot(network.1, vertex.size=5,vertex.label.dist=0,vertex.label.cex=0.5,edge.arrow.size=.2)

} #fin caminos conectores corregidos y utiles


#### INICIO AÑADIENDO TRONCALES A LOS ORIGENES ###############################################################################
{
  
  ##reconstruccion troncales
  
  troncales <- ejes.cont %>%
    anti_join(caminos.conectores.corr, by = c("V1"))
  
  new.troncales <- troncales
  new.inicio.troncal <- troncales %>%
    filter(!is.na(new.id.x) | !is.na(new.id.y))
  
  new.orden.troncal <- new.troncales %>%
    filter(is.na(V1))
  
  for (r in 8:9) {
    new.inicio.troncal.1 <- filter(new.inicio.troncal, is.na(new.inicio.troncal[,r]))
    for (i in new.inicio.troncal.1$V1) {
      
      new.orden.bucle <- new.troncales %>%
        filter(V1 == i)
      
      
      while (TRUE) {
        
        if (new.orden.bucle[nrow(new.orden.bucle), 6] == 1 | new.orden.bucle[nrow(new.orden.bucle), 7] == 1 ) {
          break
        }
        else {
          
          if (r == 8) {
            new.orden.filt <- filter(new.troncales, V3 == new.orden.bucle[nrow(new.orden.bucle), 2]) %>%
              mutate(new.id.y = new.orden.bucle[nrow(new.orden.bucle), 9])
            
            new.orden.bucle <- new.orden.bucle %>%
              rbind(new.orden.filt)
            
          }
          else {
            new.orden.filt <- filter(new.troncales, V2 == new.orden.bucle[nrow(new.orden.bucle), 3]) %>%
              mutate(new.id.x = new.orden.bucle[nrow(new.orden.bucle), 8])
            
            new.orden.bucle <- new.orden.bucle %>%
              rbind(new.orden.filt)
          }
          
          
          
        }
      }
      
      new.orden.troncal <- new.orden.troncal %>%
        rbind(new.orden.bucle)
      
    }
    
  }
  
  new.orden.troncal <- new.orden.troncal %>%
    mutate(V2.1 = ifelse(is.na(new.id.x),
                         V2,
                         V3),
           V3.1 = ifelse(is.na(new.id.y),
                         V2,
                         V3),
           new.id.x = ifelse(is.na(new.id.x),
                             new.id.y,
                             new.id.x),
           new.id.y = ifelse(is.na(new.id.y),
                             new.id.x,
                             new.id.y)) %>%
    add_rownames() %>%
   
    mutate(rowname1 = ifelse(nchar(rowname) == 1,
                            paste("0", rowname, sep = ""),
                            rowname)) %>%
    arrange(new.id.y, rowname1) %>%
    select(V1, V2.1, V3.1, V4, V5) 
  
  new.id.troncales <- new.orden.troncal %>%
    select(V2.1) %>%
    add_rownames() %>%
    mutate(new.id = as.numeric(nrow(conectores.numerados.1)-1) + as.numeric(rowname)) %>%
    select(V2.1, new.id)
  
  
  new.all.id <- conectores.numerados.1 %>%
    rbind(new.id.troncales)
  
  new.orden.troncal <- new.orden.troncal %>%
    left_join(new.all.id, by = c("V2.1")) %>%
    left_join(new.all.id, by = c("V3.1" = "V2.1")) %>%
    select(V1, new.id.x, new.id.y, V4, V5) 
  
  names(new.orden.troncal) = c("V1", "V2", "V3", "V4", "V5")
  #Graficando
  
  links.troncales <- new.orden.troncal %>%
    mutate(source = V2,
           target = V3) %>%
    select(source, target)
  names(links.troncales) = c("source", "target")
  network.1 <- graph_from_data_frame(d=links.troncales, directed=T) 
  
  # plot it
  plot(network.1, vertex.size=5,vertex.label.dist=0,vertex.label.cex=0.5,edge.arrow.size=.2)
}

   
 #### LISTA COMPLETA DE EDGES CORREGIDOS #######################################################################################
 
 
 
 TOTAL.EDGES <- edges.conectores %>%
   rbind(new.orden.troncal) %>% #(numerados.completo.troncales) %>%
   mutate(V1 = as.numeric(V1),
          V2 = as.numeric(V2),
          V3 = as.numeric(V3))
 
 #Graficando
 
 links.1 <- TOTAL.EDGES %>%
   mutate(source = V2,
          target = V3) %>%
   select(source, target)
 names(links.1) = c("source", "target")
 network.1 <- graph_from_data_frame(d=links.1, directed=T) 
 
 # plot it
 plot(network.1, vertex.size=5,vertex.label.dist=0,vertex.label.cex=0.5,edge.arrow.size=.2)



  #### ACTUALIZANDO VERTICES ####
{
vertices1 <- read.csv("/home/dfsandovalp/WORK/transporteBog/streets/vertices.csv", header = F)


new.vertices <- vertices1 %>%
  mutate(V1 = as.character(V1)) %>%
  semi_join(new.all.id, by = c("V1"="V2.1")) %>%
  left_join(new.all.id, by = c("V1"="V2.1")) 



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
}

##                        HASTA AQUIIIIIIIIIIIIIIIIIII

 
 
 write.table(TOTAL.EDGES, "/home/dfsandovalp/WORK/transporteBog/streets/new.edges.csv", sep = "\t", col.names = FALSE, row.names = FALSE )
 write.table(VERTEX, "/home/dfsandovalp/WORK/transporteBog/streets/new.vertices.csv", sep = "\t", col.names = FALSE, row.names = FALSE)








