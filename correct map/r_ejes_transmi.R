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
  edges1 <- read.csv("/home/dfsandovalp/WORK/transporteBog/streets/edges.csv", header = F)  %>%
  mutate(#V6 = paste(V2,"-",V3, sep = ""),
         #V7 = paste(V3,"-",V2, sep = ""),
         V2 = as.character(V2),
         V3 = as.character(V3)) %>%
  filter(V5 == 1)
  

count.vert1 <- edges1[3]
names(count.vert1) = c("V2")
count.vert <- edges1[2] %>%
  rbind(count.vert1) %>%
  group_by(V2) %>%
  summarise(SUMA = n())

# Tabla con origenes
conect.wit <- edges1 %>%
  left_join(count.vert, by = c("V2")) %>%
  left_join(count.vert, by = c("V3" = "V2"))

# Origenes
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
  ejes.ord <- links %>%
  mutate(source = as.numeric(source),
         target = as.numeric(target),
         id.suma = source + target )

origenes.id <- origenes %>%
  mutate(id.ori = ifelse(ori1==23,
                     1,
                     ifelse(ori1==44, 
                            2,
                            ifelse(ori1==135,
                                   3,
                                   ifelse(ori1==111,
                                          4,
                                          ifelse(ori1==47,
                                                 5,
                                                 ifelse(ori1==96,
                                                        6,""))))))) %>%
  arrange(id.ori)
} #FIN ARREGLO MANUAL 1

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

##### INICIO CAMINOS CONECTORES CORREGIDOS Y UTILES #####
{ 

  caminos.conectores.corr <- data_frame(rowname=NA,V1=NA,V2=NA,V3=NA,V4=NA,V5=NA,SUMA.x=NA,SUMA.y=NA,id.ori.x=NA,id.ori.y=NA,origen=NA,destino=NA,id.orden=NA,V2.1=NA,V3.1=NA)
  
  
  for (i in unique(caminos.conectores$id.orden)) {
    
    if (i == "3.4" | i == "4.5") {
      camino.filt <- caminos.conectores %>%
        filter(id.orden == i) %>%
        mutate(V2.1 = V2,
               V3.1 = V3)
      
      caminos.conectores.corr <- caminos.conectores.corr %>%
        rbind(camino.filt)
      }
    
    else {
      if (i == "4.6") {
        camino.filt <- caminos.conectores %>%
          filter(id.orden == i) %>%
          arrange(-rowname) %>%
          mutate(origen = 6,
                 destino = 4,
                 V2.1 = V3,
                 V3.1 = V2)
        caminos.conectores.corr <- caminos.conectores.corr %>%
          rbind(camino.filt) %>%
          filter(!is.na(rowname))
      }
      
      else {
        camino.filt <- caminos.conectores %>%
          filter(id.orden == i) %>%
          arrange(-rowname) %>%
          mutate(V2.1 = V3,
                 V3.1 = V2)
        caminos.conectores.corr <- caminos.conectores.corr %>%
          rbind(camino.filt) %>%
          filter(!is.na(rowname)) 
      }
      
    }
  
    
  }
  
  caminos.conectores.corr <- caminos.conectores.corr %>% # añadiendo rownames para mantener orden
    add_rownames() %>%
    arrange(origen, destino)
  
  conectores.numerados.1 <- caminos.conectores.corr$V2.1 
  conectores.numerados.2 <- caminos.conectores.corr$V3.1 
  names(conectores.numerados.2)  <- c("V2.1")
  
  conectores.numerados <- data.frame(V2.1=NA, new.id = 0)
  
  for (i in 1:nrow(caminos.conectores.corr)) {
    conectores.numerados.1 <- data.frame(vertices = caminos.conectores.corr[i,15], new.id = i) %>%
      add_row(V2.1 =caminos.conectores.corr[i,16], new.id = i+1)
    
    conectores.numerados <- conectores.numerados %>%
      rbind(conectores.numerados.1) %>%
      filter(!is.na(V2.1)) %>%
      as.data.frame()%>%
      distinct()
    
  }
  conectores.numerados <- as.data.frame(conectores.numerados)%>%
    mutate(V2.1 = as.character(V2.1),
           new.id = as.character(new.id))%>%
    distinct()
  
  #### nuevos id para vertices de caminos conectores ####
  conectores.numerados <- conectores.numerados[1:nrow(conectores.numerados)-1,] %>% # se descarta la ultima relacion ya que repite new id para un origen
    mutate(new.id = as.numeric(new.id)-1)
  
  caminos.conectores.corr <- caminos.conectores.corr %>%
    left_join(conectores.numerados, by = c("V2.1")) %>%
    left_join(conectores.numerados, by = c("V3.1" = "V2.1" ))
  
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
  plot(network.1, vertex.size=5,vertex.label.dist=3,vertex.label.cex=0.5,edge.arrow.size=.2)

} #fin caminos conectores corregidos y utiles


#### INICIO AÑADIENDO TRONCALES A LOS ORIGENES ####
{
 troncales <- conect.wit %>%
   anti_join(caminos.conectores.corr,  by = c("V1")) %>%
   left_join(conectores.numerados,  by = c("V2"="V2.1")) %>%
   left_join(conectores.numerados,  by = c("V3"="V2.1"))
 
 origen.troncal <- troncales %>%
   filter(!is.na(new.id.x) | !is.na(new.id.y))
 
 # identificando salidas y destinos para luego unir
 
 #salida
 {
   troncales.secuencia.x <- troncales %>% 
     filter(!is.na(new.id.x))
   
   troncales.secuencia.left <- filter(troncales.secuencia.x[1,], is.na(V2) )
   
   for (i in 1:nrow(troncales.secuencia.x)) {
     
     troncales.secuencia1 <- troncales.secuencia.x[i,]
     
     while (TRUE) {
       
       if (troncales.secuencia1[nrow(troncales.secuencia1), 6] == 1 | troncales.secuencia1[nrow(troncales.secuencia1), 7] == 1 ) {
         break
       }
       else {
         
         troncal.filt <- filter(troncales, V2 == troncales.secuencia1[nrow(troncales.secuencia1), 3])
         
         troncales.secuencia1 <- troncales.secuencia1 %>%
           rbind(troncal.filt)
           
       }
     }
     
     troncales.secuencia.left <- troncales.secuencia.left %>%
       rbind(troncales.secuencia1)
     
   }
   origen.portal.left <- troncales.secuencia.left %>%
     #filter(SUMA.x != 2 | SUMA.y != 2) %>%
     mutate(origen = ifelse(!is.na(new.id.x),
                            new.id.x,
                            ""),
            destino = ifelse(SUMA.y == 1,
                             V3,
                             ""),
            izq = "SI",
            der = "")
  }
 
 #destino
 {
   troncales.secuencia.y <- troncales %>% 
     filter(!is.na(new.id.y))
   
   troncales.secuencia.right <- filter(troncales.secuencia.y[1,], is.na(V2) )
   
   for (i in 1:nrow(troncales.secuencia.y)) {
     
     troncales.secuencia1 <- troncales.secuencia.y[i,]
     
     while (TRUE) {
       
       if (troncales.secuencia1[nrow(troncales.secuencia1), 6] == 1 | troncales.secuencia1[nrow(troncales.secuencia1), 7] == 1 ) {
         break
       }
       else {
         
         troncal.filt <- filter(troncales, V3 == troncales.secuencia1[nrow(troncales.secuencia1), 2])
         
         troncales.secuencia1 <- troncales.secuencia1 %>%
           rbind(troncal.filt)
         
       }
     }
     
     troncales.secuencia.right <- troncales.secuencia.right %>%
       rbind(troncales.secuencia1)
     
   }
   
   origen.portal.right <- troncales.secuencia.right %>%
     #filter(SUMA.x != 2 | SUMA.y != 2) %>%
     mutate(origen = ifelse(!is.na(new.id.y),
                            new.id.y,
                            ""),
            destino = ifelse(SUMA.x == 1,
                             V2,
                             ""),
            izq = "",
            der = "SI")
 }
 
 
#### IDENTIFICANDO ORIGEN Y PORTAL ####
 {
   origen.portal <- origen.portal.left %>%
     rbind(origen.portal.right) %>%
     add_rownames() %>%
     add_rownames() 
   
   
   marcando.origen.portal <- origen.portal %>%
     filter(origen != "") 
   
   marcando.origen.portal1 <- filter(marcando.origen.portal[1,], is.na(V1))
   
  #for (i in 8:9) {
  for (i in 1:nrow(marcando.origen.portal)) {
      
    base <- marcando.origen.portal[i,]
    
    if (i == nrow(marcando.origen.portal)) {
      
      base.filt <- origen.portal[as.numeric(marcando.origen.portal1[nrow(marcando.origen.portal1),1]):as.numeric(origen.portal[nrow(origen.portal),1]),] 
        
      base.filt <- base.filt %>%
        mutate(origen = as.numeric(base[nrow(base), "origen"]))
      
      base <- base %>%
        rbind(base.filt) #%>%
        #distinct()
      base <- base[-1,] %>%
        mutate(destino = as.numeric(base.filt[nrow(base.filt), "destino"]))
      base<- base[-1,]
    }
    else {}
    
    while (TRUE) {
      
      if (base[nrow(base), 13] != "" | i == nrow(marcando.origen.portal)) {
       
        break
      }
      else {
        base.filt <- origen.portal[as.numeric(marcando.origen.portal[i,1]):as.numeric(marcando.origen.portal[i+1,1])-1,] %>%
          mutate(origen = as.numeric(base[nrow(base), "origen"]))
        
        base.filt <- base.filt[-1,] %>%
          mutate(destino = as.numeric(base.filt[nrow(base.filt), "destino"]))
        
        #base.filt <- base.filt[-1,]
        
        base <- base %>%
          rbind(base.filt) %>%
          distinct()
       
      }
    }
    
    marcando.origen.portal1 <- marcando.origen.portal1 %>%
      rbind(base)
    
  }
   
  marcando.origen.portal1 <- marcando.origen.portal1 %>%
    mutate(destino = ifelse(V1 == 0,
                            14,
                            destino))%>%
    filter(destino != "") %>%
    arrange(origen, destino, rowname) %>%
    add_rownames() %>%
    mutate(id.destino = paste(origen, destino, sep = "."))
  
  
  troncales.numerados <- data.frame(V2=NA)
  
  for (i in 1:nrow(marcando.origen.portal1)) {
    
    if (marcando.origen.portal1[i, "izq"] == "SI") {
      
      base <- data.frame(vertices = marcando.origen.portal1[i,5]) %>%
        add_row(V2 = marcando.origen.portal1[i, 6])
      
      troncales.numerados <- troncales.numerados %>%
        rbind(base)
    }
    else {
      if (marcando.origen.portal1[i, "der"] == "SI") {
        
        base <- data.frame(vertices = marcando.origen.portal1[i,6]) %>%
          add_row(V3 = marcando.origen.portal1[i, 5])
        
        names(base) <- c("V2") 
        
        troncales.numerados <- troncales.numerados %>%
          rbind(base)
      }
      
    }
    
    
  }
  
}
 
 troncales.numerados11 <- troncales.numerados %>%
   mutate(vertices = as.character(V2)) %>%
   add_rownames() %>%
 select(vertices) %>%
   # group_by(vertices) %>%
   # summarise(count = n())
   anti_join(conectores.numerados, by = c("vertices" = "V2.1")) %>%
   add_rownames()
 
 unicos.para.numerar <- troncales.numerados11 %>%
   select(vertices) %>%
   distinct() %>%
   add_rownames() %>%
   mutate(id = as.numeric(rowname)+nrow(edges.conectores)-2) %>%
   select(vertices, id) 
 
 unicos.para.numerar <- unicos.para.numerar[-1,] %>%
   select(vertices, id) #%>%
   #rename(V2.1=vertices, new.id=id)
 
 numerados.completo.troncales <- marcando.origen.portal1 %>%
   left_join(unicos.para.numerar, by = c("V2" = "vertices")) %>%
   left_join(unicos.para.numerar, by = c("V3" = "vertices")) %>%
   mutate(id.x = ifelse(is.na(id.x),
                        origen,
                        id.x),
          id.y = ifelse(is.na(id.y),
                        origen,
                        id.y), 
          eje.id = as.numeric(rowname)+(nrow(edges.conectores)-1)
          ) %>%
   select(eje.id, id.x, id.y, V4, V5) %>%
   rename(V1=eje.id, V2=id.x, V3=id.y)
   
 #### LISTA COMPLETA DE EDGES CORREGIDOS ####
 
 
 
 TOTAL.EDGES <- edges.conectores %>%
   rbind(numerados.completo.troncales) %>%
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
 
 
 
}




write.table(TOTAL.EDGES, "/home/dfsandovalp/WORK/transporteBog/prueba_mapa/edgesTOTAL.csv", sep = "\t", col.names = FALSE, row.names = FALSE )


#### ACTUALIZANDO VERTICES ####

unicos.para.numerar <- unicos.para.numerar %>%
  rename(V2.1=vertices, new.id=id)

NEW.VERTEX.ID <- conectores.numerados %>%
  rbind(unicos.para.numerar) #%>%
  select(new.id, V2.1) %>%
  rename(V1=new.id, V2=V2.1)


vertices1 <- read.csv("/home/dfsandovalp/WORK/transporteBog/streets/vertices.csv", header = F)

vertices.origen <- vertices1 %>%
  mutate(V1 = as.character(V1))%>%
  #semi_join(conectores.numerados, by = c("V1"="V2.1")) %>%
  left_join(NEW.VERTEX.ID, by = c("V1"="V2.1")) %>%
  select(new.id, V2, V3) %>%
  mutate(new.id = as.numeric(new.id),
         new.id = ifelse(is.na(new.id),
                         nrow(vertices.origen)+1,
                         new.id)) %>%
  arrange(new.id) %>%
  rename(V1=new.id) %>%
  as.data.frame()
  

write.table(format(vertices.origen, scientific = T), "/home/dfsandovalp/WORK/transporteBog/prueba_mapa/verticesTOTAL.csv", sep = "\t", col.names = FALSE, row.names = FALSE )

#write.csv(vertices.origen, "/home/dfsandovalp/WORK/transporteBog/prueba_mapa/verticesConectores.csv", col.names = FALSE, row.names = FALSE )

nodos.conectores <- m[,1:ncol(m)-1] %>%
  left_join(caminos.conectores, by = c("rowname","V1","V2","V3","V4","V5","SUMA.x","SUMA.y","id.ori.x" )) %>%
  arrange(id.ori.x,id.ori.y, rowname) 

origen.destinino.conect <- ejes.base %>%
  filter(!is.na(id.ori.x) | !is.na(id.ori.y)) %>%
  mutate(rowname = as.numeric(rowname)) %>%
  left_join(caminos.conectores, by = c("rowname","V1","V2","V3","V4","V5","SUMA.x","SUMA.y")) %>%
  arrange(origen, destino, rowname) 


