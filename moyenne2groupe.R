cat("test de student: comparaison d deux groupes independants")
repeat{
  n1=as.numeric(readline("entrer la taille du premier groupe:"))
  if(!is.na(n1) && n1<=30 && n1>1){
    break
  }
  cat("ereeur: la taille doit etre >1 et inferieur ou egal a 30" )
}
repeat{
  cat("\n entrer",n1,"valeurs dupremier groupe separe par des virgules\n")
  data1=readline("groupe 1:")
  x1=as.numeric(strsplit(data1,",")[[1]])
  if(length(x1)==n1 && !any(is.na(x1))){
    break
  }
  cat("erreur: vous devez entrer exactement",n1,"valeurs numeriques\n\n")
}
repeat{
  n2=as.numeric(readline("entrer la taille du deuxieme groupe:"))
  if(!is.na(n2 )&& n2>1 && n1<=30){
    break
  }
  cat("ereeur: la taille doit etre n2>1 et inferieur ou egal a 30\n\n" )
}
repeat{
  cat("\n entrer",n2,"valeurs du deuxieme groupe separe par des virgules\n")
  data2=readline("groupe 2:")
  x2=as.numeric(strsplit(data2,",")[[1]])
  if(length(x2)==n2 && !any(is.na(x2))){
    break
  }
  cat("erreur: vous devez entrer exactement",n2,"valeurs numeriques\n\n")
}
cat("\n choisissez le degre de signification")
cat("1 : 0.01\n")
cat("2 : 0.05\n")
cat("3 : 0.02\n")
cat("4 : 0.1\n ")
choix_alpha=as.numeric(readline("votre choix:"))
if (choix_alpha==1){
  alpha=0.01
}else if (choix_alpha==2){
  alpha=0.05
  break
}else if(choix_alpha==3){
  alpha=0.02
  break
}else if(choix_alpha==4){
  alpha=0.1
  break
}else{
  cat("choix invalide") 
}
repeat{
cat("\n type de tst :\n")
cat("1: bilateral \n")
cat("2:unilateral gauche \n")
cat(" 3: unilateral droit \n")
choix=as.numeric(readline("votre choix:"))
if(choix_test %in% c("1","2","3")){
  break
}
cat("choix invalide")
}
m1=mean(x1)
m2=mean(x2)
s1=sd(x1)
s2=sd(x2)
sp2=(((n1-1)*s1^2)+((n2-1)*s2^2))/(n1+n2-2)
t_stat=(m1-m2)/sqrt(sp2*((1/n1)+(1/n2)))
ddl=n1+n2-2
if(choix_test=="1"){
  p_value=2*(1-pt(abs(t_stat),df=ddl))
}else if(choix_test=="2"){
  P_value=pt(t_stat,df=ddl)
}else{
  p_value=1-pt(t_value, df=ddl)
}
cat("resultats")
cat("\n moyenne groupe 1:",m1,"\n")
cat("\n moyenne groupe 2:",m2,"\n")
cat("\n exart-type groupe 1:",s1,"\n")
cat("\n ecart-type groupe 2:",s2,"\n")
cat("\n statistiques t:",t_stat,"\n")
cat("\n degre de liberte :",ddl,"\n")
cat("\n p_value:",p_value,"\n")
cat("\n alpha:",alpha,"\n")
if(p_value<alpha){
  cat("\n decision:on rejette H0\n")
  cat("les moyennes des groupes sont differentes\n")
}else{
  cat("\n decision : o accepte H0\n")
  cat("les moyennes des dex groupes sot similaires\n")
}