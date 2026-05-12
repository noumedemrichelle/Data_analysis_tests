cat("=== TEST DE SUTDENT COMPARAISON DE DEU PROPOSITIONS===\n\n ")
repeat{
  n1=as.numeric(readline("entrerla taile du remier echantillon n1:"))
  if(!is.na(n1)&& n1<=30 && n1>0){
    break
  }
  cat("erreur: n1 doit etre un nombre positif et inferieur a 30\n\n")
}
repeat{
  x1=as.numeric(readline("entrer le nombre de succes x1 :"))
  if(!is.na(x1) && x1>=0 && x1 <=n1){
    break
  }
  cat("erreur: x1 doit etre compris entre 0 et n1\n\nn")
}
repeat{
  n2=as.numeric(readline("entrerla taile du deuxieme echantillon n2:"))
  if(!is.na(n2) && n2<=30 && n2>0){
    break
  }
  cat("erreur: n2 doit etre un nombre positif et inferieur a 30\n\n")
}
repeat{
  x2=as.numeric(readline("entrer le nombre de succes x2 :"))
  if(!is.na(x2) && x2>=0 && x2 <=n2){
    break
  }
  cat("erreur: x2 doit etre compris entre 0 et n2\n\nn")
}
repeat{
  cat("\n choisir alpha :\n")
  cat("1=0.01\n")
  cat("2=0.05\n")
  cat("3=0.1\n")
  cat("4=0.025\n")
  choix_alpha=readline("votre choix:")
  if (choix_alpha=="1"){
    alpha=0.01
    break
  }
  if (choix_alpha=="2"){
    alpha=0.05
    break
  }
  if(choix_alpha=="3"){
    alpha=0.1
    break
  }
  if(choix_alpha=="4"){
    alpha=0.025
    break
  }
  cat("choix invalide\n\n")
}
p1=x1/n2
p2=x2/n2
p=(x1+x2)/(n1+n2)
z=(p1-p2)/sqrt(p*(1-p)*((1/n1)+(1/n2)))
p_value=2*(1-pnorm(abs(z)))
cat("resultat")
cat("\n proportion 1:",p1,"\n")
cat("\n proportion 2:",p2,"\n")
cat("\n statistique z:",z,"\n")
cat("\n p_value:",p_value,"\n")
cat("\n alpha:",alpha,"\n")
if(p_value<alpha){
  cat("\n decision :on rejette H0\n")
  cat("\n les deux propositions sont differentes\n")
}else{
  cat("\n decision :on accepte H0\n")
  cat("\n les deux propositions statistiques sont similaires\n")
}