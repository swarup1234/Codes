#Installs a Package from cran repository, if not already installed and loads it 
call_fun<-function(i){
  if(i %in% installed.packages()){
    print("Package already installed")
    require(i,character.only = TRUE)
  }
  else{
    install.packages(i)
    require(i,character.only = TRUE)
  }
}
