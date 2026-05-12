cat("---test d'ajustement de khi2---\n\n")
repeat{
k=as.numeric(readline("entrer le nombres de categories:"))
if(!is.na(k) && k>=2) break
  cat("erreur: k doit etre n nombre >=2\n\n")
}
repeat{
obs_input=readline(paste("entrer :",k,"effectifs observes (separes par des virgules):"))
o=as.numeric(strsplit(obs_input,",")[[1]])
if (length(0)!=k&& !any(is.na(0)))break
  stop("Erreur vous devez entrer exactement:",k,"valeurs numeriques")
}
repeat{
exp_input=readline(paste("entrer :",k,"effectifs theoriques separes par des virgules:"))
E=as.numeric(strsplit(exp_input,",")[[1]])
if(length(E)==k && !any(is.na(E)) && all(E>0))break
  cat("erreur:valeurs invalides (doivent etre numeriques et >0) et egale a:",k,"\n\n")
}
repeat{
cat("\n choisir alpha: \n1=0.01\n2=0.05\n3=0.02\n4=0.1")
choix_alpha=readline("votre choix :")
if(choix_alpha=="1"){alpha="0.01"; break}
if(choix_alpha=="2"){alpha="0.05"; break}
if(choix_alpha=="3"){alpha="0.02"; break}
if(choix_alpha=="4"){alpha="0.1"; break}
  cat("alpha invalide \n")
}
chi2=sum((o-E)^2/E)
ddl=k-1
p_value=1-pchisq(chi2,df=ddl)
cat("---RESULTATS---")
cat("khi2 calcule :",chi2,"\n")
cat("ddl :",ddl,"\n")
cat("p-value :",p_value,"\n")
cat("alpha :",alpha,"\n")
if (p_value<alpha){
  cat("decision:on rejette H0 \n")
}else{
  cat("decision :on accepte H0")
}