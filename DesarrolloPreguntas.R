###############################################################################################################################
####################################Ejercicio 1################################################################################
set.seed(10)
sample(c("SI","NO"), 10, replace = T)

votos <-  function ( total , votosSI , votosNO ){
  
  if ( votosSI  >= (( total * 0.5 ) + 1 )) {
    print( " Gana el SI " )
  } else {
    print( " votosSI no cumple con el Quorum " )
    if ( votosSI  >=  total * 0.3 ) {
      print( " votosSI cumple con el 30% de Quorum " )
      if ( votosSI  >=  total * 0.3  &  votosNO  <  total * 0.3 ) {
        print( " Gana el SI " )
      } else {
        if ( votosNO  >= (( total * 0.5 ) + 1 )) {
          print( " Gana el NO " )
        } else {
          print( " votosNO cumple con un 30% de Quorum " )
          if ( votosSI  ==  votosNO ) {
            print( "se produjo un empate, Gana el SI " )
          } else {
            if ( votosSI  >  votosNO ) {
              print( " Gana el SI " )
            }else {
              print( " Gana el NO " )
            }
          }
        }
      }
    } else {
      print( " Gana el NO " )
    }
  }
}
total
###################################################################################################################################
#################################Ejercicio 2###########################################################################################

# Definir lista de documentos a procesar
listaDocumentos <- list(c("mp", "Juan", "Christofer"),
                        c("of", "av01", "ampr"),
                        c("of","av01", "ante"),
                        c("of", "av08", "arme"),
                        c("of","av02", "ante"), 
                        c("of", "avo7", "ampr"),
                        c("of","av03","dape"),
                        c("of","av01","meca"),
                        c("of","avo2","dape"),
                        c("mp", "Antonia"),
                        c("mp", "Christian", "Mario"), 
                        c("mp", "Jose", "Pedro", "Antonela"),
                        c("of", "av05", "meca"),
                        c("of", "av04", "dape"),
                        c("of", "av02", "arme"))

# Definir funcion que toma como parámetro de entrada una lista de documentos y entrega la cantidad de mps que existen con cierta cantidad de niños.
contar_ninos_por_mp <- function(lista_de_documentos){
  
  # Definir variables útiles
  aux_list <- list() # Lista de listas. Cada lista dentro de ésta guardará como nombre la cantidad de niños en un mp. El valor será la cantidad de mp(s) que  existen con esa cantidad de niños.
  
  # Recorrer la lista de documentos
  for (documento in lista_de_documentos){
    if (documento[1] == 'mp'){ # Si el documento corresponde a una Medida de Protección (mp)
      
      cantidad_ninos <- as.character(length(documento)-1)           # Obtener la cantidad de niños del mp. Guardar el número como 'string'.
      if (cantidad_ninos %in% names(aux_list)){                     # Si existe un mp con esa cantidad de niños en la lista auxiliar, 
        aux_list[[cantidad_ninos]] <- aux_list[[cantidad_ninos]]+1  # añadir 1 al valor de la cantidad de mps con ese número de niños.
      } else {
        aux_list[[cantidad_ninos]] <- 1                             # Si no existe un mp con esa cantidad de niños, se agrega a la lista auxiliar ese nombre y se le asigna el valor 1 (hay 1 mp con esa cantidad de niños).
      }
    }
  }
  
  for (name in names(aux_list)){
    cat("Se cuentan con", aux_list[[name]],"mp de", name,"niños\n")
  }
}

# Ejecutar función definida y entregarle como parámetro la lista de documentos
contar_ninos_por_mp(listaDocumentos)

