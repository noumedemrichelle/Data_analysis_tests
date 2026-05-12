cat("== test d'independance de khi2==\n\n")
repeat{
  r=as.numeric(readline("entrer le nombres de lignes"))
  if(!is.na(r) && r>=2){
    break
  }
  cat("erreur: la nombre de lignes doit etre >=2\n\n")
}
repeat{
  c=as.numeric(readline("entrer le nombre de colonne:"))
  if(!is.na(c) && c>=2){
    break
  }
  cat("erreur: le nombre de colonnes doit etre >=2\n\n")
}
total_valeurs=r*c
cat ("\n vous devez entrer", total_valeurs,"valeurs separees par des virgules.\n")
cat("exemple : 10,12,15,8,....\n\n")
repeat{
  data_input=readline("entrer les effectifs observes :")
  valeurs=as.numeric(strsplit(data_input,",")[[1]])
  if(length(valeurs)== total_valeurs && !any(is.na(valeurs)) && all(valeurs>=0)){
    break
  }
  cat("erreur:vous devez entrer exactement", total_valeurs,"valeurs numeriques positives\n\n")
}
print(r)
print(c)
print(valeurs)
print(length(valeurs))
o=matrix(valeurs, 
         nrow = r,
         ncol = c,
         byrow = TRUE)
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
tot_lignes=rowSums(o)
tot_colonnes=colSums(o)
total_general=sum(o)
E=outer(tot_lignes, tot_colonnes)/total_general
chi2=sum((0-E)^2/E)
ddl=(r-1)*(c-1)
p_value=1-pchisq(chi2, df=ddl)
cat("\n========RESULTATS========\n")
cat("\n tableau observe:\n")
print(0)
cat("\n tableau theorique: \n ")
print(round(E,2))
cat("\n khi2 calcule:",chi2,"\n")
cat("degres de liberte:",ddl,"\n")
cat("p-value :",p_value,"\n")
cat("alpha :",alpha,"\n")
if(p_value<alpha){
  cat("\n decision: on rejette H0\n")
  cat(" les valeurs sont dependantes")
}else{
  cat("\n decion on accepte H0\n")
  cat("les variables sont independantes\n")
}