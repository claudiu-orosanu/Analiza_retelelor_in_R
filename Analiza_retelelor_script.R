# Vizualizarea retelelor


# instalarea resurselor necesare
# library(devtools) 
# install_github("DougLuke/UserNetR")

# pachetul 'statnet' va fi folosit 
# pentru analizarea retelei Morno
library(statnet) 
# importam pachetul de date
library(UserNetR) 
data(Moreno)

#extragem date in functie de sex-ul membrilor
gender <- Moreno %v% "gender"
#plotam un graficul pentru reteau Moreno
#vertex.cex - grosimea nodurilor
plot(Moreno, vertex.col = gender + 2, vertex.cex = 1.2)

# vom afla dimensiunea retelei (numarul de noduri)
network.size(Moreno)
## [1] 33


# pentru a afla mai multe informatii desprea retea 
#putem folosi functia 'summary',Setand proprietatea 
#print.adj cu fals oprim afisarea informatiilor despre 
#adiacente 
summary(Moreno, print.adj = FALSE)


#densitatea unui graf neorientat: (2*L)/(k*(k-1))
den_hand <- 2*46/(33*32) 
den_hand
## [1] 0.0871

#pentru a calcula densitatea avem metoda 'gden'.
gden(Moreno)
## [1] 0.0871

#O retea poate fi impartita in mai multe grupe(componente),
components(Moreno)



#Extragem cea mai mare componenta din reteau Moreno
#(care contine 31 de noduri conectate) intr-o noua
#matrice. Caile cele mai scurte sunt calculate de functia 
#'geodist()'. Maximul cailor cele mai scurte este extras
#'#obtinandu-se diametrul retelei.
lgc <- component.largest(Moreno,result="graph") 
gd <- geodist(lgc) 
max(gd$gdist)
## [1] 11

#Transitivitatea este calculata de functia 'gtrans'
gtrans(Moreno,mode="graph")
## [1] 0.28



#Pentru a crea o un obiect al unei retele folosim functia
#'network()', dar mai intai trebui creata o matrice de 
#'#adiacenta
netmat1 <- rbind(c(0,1,1,0,0), 
                 c(0,0,1,1,0), 
                 c(0,1,0,0,0), 
                 c(0,0,0,0,0), 
                 c(0,0,1,0,0)) 
rownames(netmat1) <- c("A","B","C","D","E") 
colnames(netmat1) <- c("A","B","C","D","E") 
net1 <- network(netmat1,matrix.type="adjacency") 
class(net1)
summary(net1)


#vizualizarea retelei noi create
gplot(net1, vertex.col = 2, displaylabels = TRUE)

#Aceasi retea poate fi creata cu urmatorul cod:
netmat2 <- rbind(c(1,2), 
                 c(1,3), 
                 c(2,3), 
                 c(2,4), 
                 c(3,2), 
                 c(5,3)) 
net2 <- network(netmat2,matrix.type="edgelist") 
network.vertex.names(net2) <- c("A","B","C","D","E") 
summary(net2)

#tansformarea intr-o matrice de tip 'sociomatrix'
as.sociomatrix(net1)
class(as.sociomatrix(net1))
#tansformarea intr-o lista de muchii
all(as.matrix(net1) == as.sociomatrix(net1))
as.matrix(net1,matrix.type = "edgelist")


#setam atribute(sex-ul membrilor) pentru nodurile retelei
set.vertex.attribute(net1, "gender", c("F", "F", "M", "F", "M"))
#vom seta un vector numeric ca si atribut
net1 %v% "alldeg" <- degree(net1) 
list.vertex.attributes(net1)


# afisarea atributului 'gender'
get.vertex.attribute(net1, "gender")
net1 %v% "alldeg"


#setam un numar ales aleator fiecarei muchii din retea
# si afisam aceasta informatie
list.edge.attributes(net1)
set.edge.attribute(net1,"rndval", 
                   runif(network.size(net1),0,1)) 
list.edge.attributes(net1)
## [1] "na" "rndval"
summary(net1 %e% "rndval")
## Min. 1st Qu. Median Mean 3rd Qu. Max. 
## 0.163 0.165 0.220 0.382 0.476 0.980
summary(get.edge.attribute(net1,"rndval"))
## Min. 1st Qu. Median Mean 3rd Qu. Max. 
## 0.163 0.165 0.220 0.382 0.476 0.980

#Cream o matrice de adiacenta in care fiecare muchie
#reprezinta un nivel de 'like'(0,1,2,3) intre mebrii retelei
netval1 <- rbind(c(0,2,3,0,0), 
                 c(0,0,3,1,0), 
                 c(0,1,0,0,0), 
                 c(0,0,0,0,0), 
                 c(0,0,2,0,0)) 
netval1 <- network(netval1,matrix.type="adjacency", 
                   ignore.eval=FALSE,names.eval="like") 
network.vertex.names(netval1) <- c("A","B","C","D","E") 
list.edge.attributes(netval1)
## [1] "like" "na"
get.edge.attribute(netval1, "like")
## [1] 2 1 3 3 2 

#Afisarea matricei de legatura
as.sociomatrix(netval1)
#Afisarea matricei care contine numarul de like-uri
as.sociomatrix(netval1,"like")


#Libraria 'igraph' este folosita pentru accesa informatii 
#despre retea, asemanator cu libraria 'network'.
detach(package:statnet) 
library(igraph)

#Cream un graf folosind o matrice de adiacenta, unde
# D indica un graf orientat, N indica faptul ca nodurile
#au nume. De asemenea avem numarul de noduri (5) dar si
#numarul de muchii (6)
inet1 <- graph.adjacency(netmat1) 
class(inet1)
## [1] "igraph"
summary(inet1)
## IGRAPH DN-- 5 6 -
## + attr: name (v/c)
str(inet1)
## IGRAPH DN-- 5 6 -
## + attr: name (v/c) 
## + edges (vertex names): 
## [1] A->B A->C B->C B->D C->B E->C
inet2 <- graph.edgelist(netmat2) 
summary(inet2)
## IGRAPH D--- 5 6 -

# Pentru a asigura posibilitatea de folosi functii din
#'statnet' pe date din obiectele retelelor care au fost
#'#construite cu 'igraph' sau vice-versa, folosim 
#'#libraria 'intergraph' 
library(intergraph) 
class(net1)
## [1] "network"
net1igraph <- asIgraph(net1) 
class(net1igraph)
## [1] "igraph"
str(net1igraph)
## IGRAPH D--- 5 6 -
## + attr: alldeg (v/n), gender (v/c), na 
## | (v/l), vertex.names (v/c), na (e/l), 
## | rndval (e/n) ## + edges: 
## [1] 1->2 3->2 1->3 2->3 5->3 2->4

#Cream o lista cu muchii pe care o salvam intr-un fisier
#CSV dupa care citim din fisier si il transformam intr-un
#obiect
detach("package:igraph", unload=TRUE) 
library(statnet)

netmat3 <- rbind(c("A","B"), 
                 c("A","C"), 
                 c("B","C"),
                 c("B","D"), 
                 c("C","B"), 
                 c("E","C")) 
net.df <- data.frame(netmat3) 
net.df
##   X1 X2 
## 1  A  B 
## 2  A  C 
## 3  B  C 
## 4  B  D 
## 5  C  B 
## 6  E  C
write.csv(net.df, file = "MyData.csv", 
          row.names = FALSE) 
net.edge <- read.csv(file="MyData.csv") 
net_import <- network(net.edge, 
                      matrix.type="edgelist") 
summary(net_import)
gden(net_import)


#get.inducedSubgraph() este o functie care returneaza
# un nou obiect al retelei care este bazat pe criteriul de
#filtrare
n1F <- get.inducedSubgraph(net1, 
                           which(net1 %v% "gender" == "F")) 
n1F[,]
gplot(n1F,displaylabels=TRUE)
#Se creaza un subset al retelei cu nodurile care au nota (variabila deg) 
#mai mare decat 1. %s% reprezinta o prescurtare pentru 
deg <- net1 %v% "alldeg" 
n2 <- net1 %s% which(deg > 1)
gplot(n2,displaylabels=TRUE)

#importam un nou set de date
#Utilizand functia 'isolates' putem detecta numarul de noduri izolate
data(ICTS_G10) gden(ICTS_G10)
## [1] 0.0112
length(isolates(ICTS_G10))
## [1] 96

#stergem nodurile izolate
n3 <- ICTS_G10 
delete.vertices(n3,isolates(n3)) 
gden(n3)
length(isolates(n3))

#utilizam setul de date DHHS
data(DHHS) d <- DHHS 
gden(d)
## [1] 0.312
# functia par() este folosita pentru a configura setarile legate
# de plotarea graficelor (fonturi, culori, axe, titluri)

# Parametri functie
# mar - margini
op <- par(mar = rep(0, 4)) 
#plotam noul grafic
gplot(d,gmode="graph",edge.lwd=d %e% 'collab', 
      edge.col="grey50",vertex.col="lightblue", 
      vertex.cex=1.0,vertex.sides=20) 
par(op)

#cream o matrice cu primii 6 membrii ai graficului
as.sociomatrix(d)[1:6,1:6]
#extragem valorile muchiilor 
list.edge.attributes(d)
#cream o matrice numai cu colaborarile dintre cei 6
as.sociomatrix(d,attrname="collab")[1:6,1:6]

#afisam toate legaturile posibile din graf
table(d %e%"collab")

#extragem intr-o matrice toate colaborarile care valoarea >=3
d.val <- as.sociomatrix(d,attrname="collab") 
#pe restul le setam cu 0
d.val[d.val < 3] <- 0 
d.filt <- as.network(d.val, directed=FALSE, 
                     matrix.type="a",ignore.eval=FALSE, 
                     names.eval="collab")
summary(d.filt,print.adj=FALSE)
gden(d.filt)
#afisam noul grafic
op <- par(mar = rep(0, 4)) 
gplot(d.filt,gmode="graph",displaylabels=TRUE, 
      vertex.col="lightblue",vertex.cex=1.3, 
      label.cex=0.4,label.pos=5, displayisolates=FALSE) 
par(op)

#symmerize transforma un graf orientat intr-unul neorientat
net1mat <- symmetrize(net1,rule="weak") 
net1mat
net1symm <- network(net1mat,matrix.type="adjacency") 
#denumin nodurile din folosite in matrice
network.vertex.names(net1symm) <- c("A","B","C","D","E") 
summary(net1symm)

# functia par() este folosita pentru a configura setarile legate
# de plotarea graficelor (fonturi, culori, axe, titluri)

# Parametri functie
# mar - margini
# mfrow - combinarea mai multor grafice intr-o matrice

## salvam in variabila op setarile initiale
op <- par(mar = rep(0, 4),mfrow=c(1,2))

# plotam un grafic in forma de cerc pe baza setului de date Moreno
# vertex.cex - grosimea nodurilor
plot(Moreno,mode="circle",vertex.cex=1.5)

# plotam un grafic folosind algoritmul fruchtermanreingold pe acelasi set de date
plot(Moreno,mode="fruchtermanreingold",vertex.cex=1.5)

# revenim la setarile de plotare initiale (inainte de modificarea anterioara)
par(op)


op <- par(mar = c(0,0,4,0),mfrow=c(1,2))
# plotam un grafic neorientat pe baza setului de date Moreno, 
# unde nodurile sunt aranjate in mod aleator
# gmode = "graph" este pentru a considera graful ca fiind neorientat
gplot(Moreno,gmode="graph",mode="random",vertex.cex=1.5,main="Random layout")

# plotam un grafic neorientat pe baza setului de date Moreno, unde nodurile
# sunt aranjate folosind algoritmul fruchtermanreingold
gplot(Moreno,gmode="graph",mode="fruchtermanreingold",vertex.cex=1.5,main="Fruchterman-Reingold")
par(op)


op <- par(mar=c(0,0,4,0),mfrow=c(2,3))
# mai jos sunt diferite plotari ale retelei Bali

# edge.col - culoarea muchiilor
# main - titlul graficului
# mode - modul de plotare

gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='circle',main="circle")

gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='eigen',main="eigen")

gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='random',main="random")

gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='spring',main="spring")

gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='fruchtermanreingold',main='fruchtermanreingold')

gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='kamadakawai',main='kamadakawai')

par(op)


# salvam coordonatele graficului generat de functia gplot in variabila mycoords1
mycoords1 <- gplot(Bali,gmode="graph",vertex.cex=1.5)
mycoords1

# cream un alt set de coordonate identic cu primul set 
mycoords2 <- mycoords1

# modificam al doilea set de coordonate
mycoords2[,2] <- mycoords1[,2]*1.5
mycoords2

op <- par(mar=c(4,3,4,3),mfrow=c(1,2))

# plotam graficul folosind primul set de coordonate
# ylim - valorile de pe axa y intre care vor fi plotate nodurile
gplot(Bali,gmode="graph",vertex.cex=1.5,
      coord=mycoords1, suppress.axes = FALSE,
      ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1),
      main="Original coordinates")


# plotam graficul folosind al doilea set de coordonate, cel modificat
gplot(Bali,gmode="graph",vertex.cex=1.5,
      coord=mycoords2,suppress.axes = FALSE,
      ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1),
      main="Modified coordinates")

par(op)


# demontam libraria statnet
detach(package:statnet)
# montam librariile igraph si intergraph
library(igraph)
library(intergraph)

# transformam obiectul de tip "network" din libraria statnet intr-un
# obiect de tip "igraph" din libraria "igraph"
iBali <- asIgraph(Bali)

op <- par(mar=c(0,0,3,0),mfrow=c(1,3))

# plotam reteaua Bali, care e stocata intr-un obiect "igraph", in mai multe moduri
plot(iBali,layout=layout_in_circle,main="Circle")
plot(iBali,layout=layout_randomly,main="Random")
plot(iBali,layout=layout_with_kk,main="Kamada-Kawai")
par(op)


detach(package:igraph)
detach(package:intergraph)
library(statnet)

# plotam reteaua Bali folosind culoarea "slateblue2" pentru noduri
gplot(Bali,vertex.col="slateblue2",gmode="graph")

# obtinem valorile rgb pentru culoarea predefinita "slateblue2"
col2rgb('slateblue2')

# configurarea culorii nodurilor folosind functia rgb
gplot(Bali,vertex.col=rgb(122,103,238,maxColorValue=255),gmode="graph")

# configurarea culorii nodurilor folosind un string hexazecimal
gplot(Bali,vertex.col="#7A67EE",gmode="graph")

# cream o matrice aleatoare 300x300 care reprezinta un graf
ndum <- rgraph(300,tprob=0.025,mode="graph")

op <- par(mar = c(0,0,2,0),mfrow=c(1,2))

# plotam graful aleator ndum folosind noduri opace
gplot(ndum,gmode="graph",
      vertex.cex=2, vertex.col=rgb(0,0,139,maxColorValue=255),
      edge.col="grey80",edge.lwd=0.5,
      main="Fully opaque")

# plotam graful aleator ndum folosind noduri transparente
gplot(ndum,gmode="graph",
      vertex.cex=2,vertex.col=rgb(0,0,139,alpha=80,maxColorValue=255),
      edge.col="grey80",edge.lwd=0.5,
      main="Partly transparent")
par(op)


# obtinem un vector care contine, pentru fiecare nod (membru), rolul acestuia in retea
rolelab <- get.vertex.attribute(Bali,"role")

op <- par(mar=c(0,0,0,0))
# plotam graficul retelei Bali folosind culori diferite pentru noduri (in functie de rol)
# label - etichetele nodurilor
# displayLabels - afiseaza sau nu etichetele
plot(Bali,usearrows=FALSE,vertex.cex=1.5,label=rolelab,displaylabels=T,vertex.col="role")
par(op)

# afisam paleta de culori default din R
palette()

# incarcam libraria RColorBrewer
library(RColorBrewer)
# afisam o paleta de 5 culori din gama "Dark2"
display.brewer.pal(5, "Dark2")

op <- par(mar=c(0,0,0,0))
# salvam paleta de culori in variabila my_pal
my_pal <- brewer.pal(5,"Dark2")
# cream un factor care contine rolurile fiecarui membru ca si categorii
rolecat <- as.factor(get.vertex.attribute(Bali,"role"))
# plotam graficul retelei Bali folosind paleta de culori definita anterior
plot(Bali,vertex.cex=1.5,label=rolelab,displaylabels=T,vertex.col=my_pal[rolecat])
par(op)


op <- par(mar=c(0,0,0,0))
# formele nodurilor (numarul de laturi ale poligonului prin care reprezentat un nod)
sidenum <- 3:7
# plotam graficul retelei Bali asignand forme diferite nodurilor in functie de rol
plot(Bali,usearrows=FALSE,vertex.cex=4,displaylabels=F,vertex.sides=sidenum[rolecat])
par(op)


op <- par(mar = c(0,0,2,0),mfrow=c(1,3))
# plotam graficul retelei Bali asignand marimi diferite nodurilor
plot(Bali,vertex.cex=0.5,main="Too small")
plot(Bali,vertex.cex=2,main="Just right")
plot(Bali,vertex.cex=6,main="Too large")
par(op)

# vector ce contine gradul fiecarui nod (cate muchii ies/intra din/in el)
# (masoara cat de central este un nod)
deg <- degree(Bali,gmode="graph")
deg

# de cati pasi este nevoie pentru a ajunge in toate celelalte noduri, plecand de la un anumit nod 
# (masoara cat de central este un nod)
cls <- closeness(Bali,gmode="graph")
cls

# cate drumuri de cost minim trec printr-un anumit nod
# (masoara cat de central este un nod)
bet <- betweenness(Bali,gmode="graph")
bet

op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
# plotam graficul folosind valorile actuale ale vectorului deg
plot(Bali,usearrows=T,vertex.cex=deg,main="Raw")
# plotam graficul folosind valorile ajustate ale vectorului deg
plot(Bali,usearrows=FALSE,vertex.cex=log(deg),main="Adjusted")
par(op)


op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
# plotam graficul folosind valorile actuale ale vectorului cls
plot(Bali,usearrows=T,vertex.cex=cls,main="Raw")
# plotam graficul folosind valorile ajustate ale vectorului cls
plot(Bali,usearrows=FALSE,vertex.cex=4*cls,main="Adjusted")
par(op)

op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
# plotam graficul folosind valorile actuale ale vectorului bet
plot(Bali,usearrows=T,vertex.cex=bet,main="Raw")
# plotam graficul folosind valorile actuale ale vectorului bet
plot(Bali,usearrows=FALSE,vertex.cex=sqrt(bet+1),main="Adjusted")
par(op)

# functia rescale primeste un vector de numere si valorile *low* si *high*. 
# la final, fiecare numar se va gasi in intervalul [low,high].
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}

# plotam graficul folosind functia rescale pentru a ajusta valorile gradelor fiecarui nod
plot(Bali,vertex.cex=rescale(deg,1,6),main="Adjusted node sizes with rescale function.")


# obtinem numele fiecarui membru al retelei
get.vertex.attribute(Bali,"vertex.names")
op <- par(mar = c(0,0,0,0))
# plotam graficul afisand etichete cu numele membrului in  dreptul fiecarui nod
# label.cex - grosimea textului 
# pad - cat de departe se afla textul fata de nod
# label.col - culoarea textului 
plot(Bali,displaylabels=TRUE,label.cex=0.8,pad=0.4,label.col="darkblue")
par(op)

op <- par(mar = c(0,0,0,0))
# obtinem rolul fiecarui membru al retelei
rolelab <- get.vertex.attribute(Bali,"role")
# plotam graficul afisand etichete cu rolul membrului din retea
plot(Bali,usearrows=FALSE,label=rolelab,displaylabels=T,label.col="darkblue")
par(op)

op <- par(mar = c(0,0,0,0))
# IC este un atribut la nivel de muchii care determina gradul de interactiune intre doua noduri
IClevel <- Bali %e% "IC"
# edge.lwd - grosimea muchiei
plot(Bali,vertex.cex=1.5,edge.lwd=1.5*IClevel)
par(op)



op <- par(mar = c(0,0,0,0))

# numarul de muchii din retea
n_edge <- network.edgecount(Bali)

# vector ce contine categoria (1,2,3) pentru fiecare muchie in parte
edge_cat <- sample(1:3,n_edge,replace=T)

# paleta de culori asociata categoriilor
linecol_pal <- c("blue","red","green")

# plotam graficul, asignand muchiilor o culoare in functie de categoria acestora 
plot(Bali,vertex.cex=1.5,vertex.col="grey25",
     edge.col=linecol_pal[edge_cat],
     edge.lwd=2)

par(op)


op <- par(mar = c(0,0,0,0))

# numarul de muchii din retea
n_edge <- network.edgecount(Bali)

# vector ce contine categoria (1,2,3) pentru fiecare muchie in parte
edge_cat <- sample(1:3,n_edge,replace=T)

# tipurile de muchie
line_pal <- c(2,3,4)

# plotam graficul, utilizand un tip de linie pentru fiecare muchie, in functie de categorie
gplot(Bali,vertex.cex=0.8,gmode="graph",vertex.col="gray50",
      edge.lwd=1.5,
      edge.lty=line_pal[edge_cat])

par(op)



op <- par(mar = c(0,0,0,0))

# paleta de 5 culori din gama "Dark2"
my_pal <- brewer.pal(5,"Dark2")

# factor in care categoriile sunt rolurile nodurilor
rolecat <- as.factor(get.vertex.attribute(Bali,"role"))

# plotam graficul, asignand o culoare diferita nodurilor in functie de categorie
plot(Bali,vertex.cex=rescale(deg,1,5),vertex.col=my_pal[rolecat])

# cream legenda care va fi afisata in coltul stanga-jos
# legend - rolurile nodurilor
# col - culorile asociate rolurilor
# pch - simbolurile care apar in legenda
# pt.cex - marimea simbolurilor
# bty - tipul figurii geometrice ce inconjoara legenda
legend("bottomleft",
       legend=c("BM","CT","OA","SB","TL"),
       col=my_pal,
       pch=19,
       pt.cex=1.5,
       bty="n",
       title="Terrorist Role")

par(op)


# 3. Tehnici avansate de plotare

library(intergraph)
library(igraph)

# cream un obiect de tip "igraph" ce contine reteaua Bali
iBali <- asIgraph(Bali)

# deschidem o fereastra in care poate fi editat graficul (pozitia/culoarea/marimea nodurilor; 
# marimea/culoarea muchiilor)
Coord <- tkplot(iBali, vertex.size=3,
                vertex.label=V(iBali)$role,
                vertex.color="darkgreen")

# editarea plotului in fereastra se face inainte de rularea urmatoarelor doua comenzi
# obtinem coordonatele modificate de utilizator
MCoords <- tkplot.getcoords(Coord)

# plotam graficul folosind noile coordonate
plot(iBali, layout=MCoords, vertex.size=5,vertex.label=NA, vertex.color="lightblue")




# montam libraria networkD3
library(networkD3)

# nodurile sursa
src <- c("A","A","B","B","C","E")

# nodurile destinatie
target <- c("B","C","C","D","B","C")

# cream un data frame cu doua coloane ce reprezinta lista de adiacenta pentru retea
net_edge <- data.frame(src, target)

# plotam graficul interactiv folosind functia simpleNetwork
simpleNetwork(net_edge)

# salvam graficul intr-un fisier HTML de sine statator
net_D3 <- simpleNetwork(net_edge)
saveNetwork(net_D3,file = 'Net_test1.html',selfcontained=TRUE)


# lista de adiacenta a muchiilor din reteaua Bali
iBali_edge <- get.edgelist(iBali)

# scadem 1 pentru ca id-urile nodurilor sa inceapa de la 0
iBali_edge <- iBali_edge - 1

# cream un data frame ce contine lista de adiacenta
iBali_edge <- data.frame(iBali_edge)

# cream un data frame cu 3 coloane: 
# id-ul unui nod, grupul acestuia (rolul), marimea nodului (in functie de grad)
iBali_nodes <- data.frame(NodeID=as.numeric(V(iBali)-1),
                          Group=V(iBali)$role,
                          Nodesize=(degree(iBali)))

# cream graficul interactiv ce modeleaza reteaua Bali
# Links - lista de adiacenta (data frame)
# Nodes - nodurile cu cele 3 caracteristici (data frame)
# NodeID - coloana care contine id-urile nodurilor (din Nodes)
# Nodesize - coloana care contine marimea nodurilor (din Nodes)
# Group - coloana care contine grupul nodurilor (din Nodes)
# opacity - gradul de transparenta al nodurilor
# legend - afiseaza legenda sau nu
forceNetwork(Links = iBali_edge,
             Nodes = iBali_nodes,
             Source = "X1", Target = "X2",
             NodeID = "NodeID", Nodesize = "Nodesize",
             radiusCalculation="Math.sqrt(d.nodesize)*3",
             Group = "Group", opacity = 0.8,
             legend=TRUE)



# incarcam libraria visNetwork
library(visNetwork)

# obtinem lista de adiacenta a retelei Bali
iBali_edge <- get.edgelist(iBali)
# punem lista de adiacenta intr-un data frame cu doua coloane (from, to)
iBali_edge <- data.frame(from = iBali_edge[,1],to = iBali_edge[,2])

# cream un data frame cu o singura coloana (id) ce contine toate nodurile
iBali_nodes <- data.frame(id = as.numeric(V(iBali)))

# plotam graficul interactiv cu ajutorul functiei visNetwork()
visNetwork(iBali_nodes, iBali_edge, width = "100%")



# instalam pachetul arcdiagram
install_github("gastonstat/arcdiagram")

library(arcdiagram)
library(igraph)
library(intergraph)

# incarcam setul de date Simpsons
data(Simpsons)

# convertim obiectul Simpsons la "igraph"
iSimp <- asIgraph(Simpsons)

# obtinem lista de adiacenta a retelei Simpsons
simp_edge <- get.edgelist(iSimp)

# plotam un graf de tip arc pe baza retelei Simpsons
# acest graf evidentiaza legaturile dintre personaje
arcplot(simp_edge)

# imbunatatim graficul anterior

# vector ce contine grupul fiecarui nod
# 1-familie; 2-colegi de munca; 3-colegi de scoala; 4-vecini
s_grp <- V(iSimp)$group

# culoarea asociata fiecarui grup
s_col = c("#a6611a", "#dfc27d","#80cdc1","#018571")

# culorile pentru fiecare nod, in functie de grupul din care face parte
cols = s_col[s_grp]

# vector ce contine gradul fiecarui nod
node_deg <- degree(iSimp)

# plotam graficul de tip arc imbunatatit
# lwd.arcs - grosimea arcurilor (muchiilor)
# cex.nodex - marimea nodurilor
# labels - eticheta fiecarui nod
# col.labels - culoarea textului din etichete
# fond - marimea fontului
# col.nodes - culoarea nodurilor
arcplot(simp_edge, 
        lwd.arcs=2, 
        cex.nodes=node_deg/2,
        labels=V(iSimp)$vertex.names,
        col.labels="darkgreen",
        font=1,
        pch.nodes=21,
        line=1,
        col.nodes = cols,
        bg.nodes = cols, 
        show.nodes = TRUE)



library(statnet)
library(circlize)

# incarcam setul de date FIFA_Nether
data(FIFA_Nether)

# matricea de adiacenta ce contine si costurile muchiilor (numarul de pase dintre jucatori)
FIFAm <- as.sociomatrix(FIFA_Nether,attrname='passes')

# numele nodurilor (jucatorilor)
names <- c("GK1","DF3","DF4","DF5","MF6","FW7","FW9","MF10","FW11","DF2","MF8")

# asignam nume randurilor si coloanelor matricei de adiacenta
rownames(FIFAm) = names
colnames(FIFAm) = names
FIFAm

# renuntam la muchiile cu costul mai mic ca 10 (mai putin de 10 pase)
FIFAm[FIFAm < 10] <- 0
FIFAm

# plotam graficul de tip "chord"
chordDiagram(FIFAm)

# imbunatatim graficul
# asignam o culoare fiecarei categorii de jucator (portar, fundas, mijlocas, atacant)
grid.col <- c("#AA3939",rep("#AA6C39",4),rep("#2D882D",3),rep("#226666",3))

# directional - muchiilor care pleaca din nod sunt mai departe de cerc
# grid.col - culoarea fiecarui nod
# order - ordinea in care sunt plotate nodurile
chordDiagram(FIFAm,
             directional = TRUE,
             grid.col = grid.col,
             order=c("GK1","DF2","DF3","DF4","DF5","MF6","MF8","MF10","FW7","FW9","FW11"))


# cream iar matricea de adiacenta
FIFAm <- as.sociomatrix(FIFA_Nether,attrname='passes')
colnames(FIFAm) <- c("GK1","DF3","DF4","DF5","MF6","FW7","FW9","MF10","FW11","DF2","MF8")
rownames(FIFAm) <- c("GK1","DF3","DF4","DF5","MF6","FW7","FW9","MF10","FW11","DF2","MF8")

# functie ce interpoleaza culorile date ca parametri pentru a crea noi culori
palf <- colorRampPalette(c("#669999", "#003333"))

# plotam graficul de tip "heatmap", care evidentiaza cei mai activi jucatori
# (din punct de vedere al paselor date/primite)
heatmap(FIFAm[,11:1],
        Rowv = NA,
        Colv = NA,
        col = palf(60),
        scale="none")