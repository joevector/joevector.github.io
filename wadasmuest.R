# Probabilidades de seleccion (dato del problema)
P <- data.frame(id=1:4,y=c(2,1,5,4))

pij <- matrix(c(0.40,0.20,0.10,0.10,
                0.20,0.65,0.30,0.15,
                0.10,0.30,0.55,0.15,
                0.10,0.15,0.15,0.40),ncol=4)

pares <- combn(1:4,2)

pij[upper.tri(pij)]

htvar <- function(pij,th){
  for(i in 1:n){
    sum(diag(pij*pij[i,-])
  }
}
