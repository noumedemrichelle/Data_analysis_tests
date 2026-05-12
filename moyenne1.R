cat("===TEST t DE STUDENT(n<30)===\n")
n=as.numeric(readline("Entrer le nombre d'echantillon"))
if (is.na(n||n<=1)){
  stop("Erreur : n un nombre >1")
}else if(n>=30){
  stop("Erreur : normalement on utilise un test Z.")
}
cat ("\n vous devez etrer",n,"valeurs separees par des virgules.\n")
cat("Exemle : 12,13,15,14\n\n")
data_input=readline("entrer vos valeurs:")
x=as.numeric(strsplit(data_input,",")[[1]])
if (length(x) !=n){
  stop(paste("erreur : certaines valeurs ne sont pas numerique"))
}
if ( any(is.na(x))){
  stop("Erreur : certaines valeurs ne sont pas numeriques")
}


mu0= as.numeric(readline("entrer la moyenne theorique (mu0) :"))
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
}else if(choix_alpha==3){
  alpha=0.02
}else if(choix_alpha==4){
  alpha=0.1
}else{
 stop("choix invalide") 
}

cat("\n type de tst :\n")
cat("1: bilateral \n")
    cat("2:unilateral gauche \n")
   cat(" 3: unilateral droit \n")
choix_test=as.numeric(readline("votre choix:"))

x_bar=mean(x)
s=sd(x)
t_stat=(x_bar-mu0)/(s/sqrt(n))
ddl=n-1
if ( choix==1){
  p_value=2*(1-pt(abs(t_stat), df=ddl))
} else if( choix==2){
  p_value=pt(t_stat,df=ddl)
} else if (choix==3){
  p_value=1-pt(t_stat,df=ddl)
}else{
  stop("choix invalide")
}
cat("\n---Resultats---\n")
cat("valeurs =",x,"\n")
cat("moyenne observer x_bar=",x_bar,"\n")
cat(" ecart type s =",s,"\n")
cat("t calcule t_stat=",t_stat,"\n")
cat("ddl =",ddl,"\n")
cat("p_value=",p_value,"\n")
cat("alpha =",alpha,"\n")
if(p_value<alpha){
  cat("decision: on rejette H0\n")
}else{
  cat(" decision: on accepte H0\n")
  
}
  
