#install.packages("tidyr")
#install.packages("svMisc")
#require(tidyr)
#require(svMisc)


#******************* End Function ANN ****************************************************

tempoBaseInicio<-proc.time()

tempoGeral<-list()



#**************** Tratar bases ***********************************************************************
tratarBases<-function(dataBase,sizeDataBase,tipo){
  print("******** Tratando a base  ******************************")
  
  dataSet <- list()
  
  print("Excluindo missings das sequencias da base de treino******************************************************")
  for (i in 1:sizeDataBase){ 
    aux <- matrix(unlist(dataBase[i,]))
    x <- c((na.omit(aux)))
    dataSet[i] = list(x)
    print(paste("Construindo base de ", sizeDataBase, tipo, "na linha ", i, 
                ", com sequencia de tamanho : ", length(dataSet[[i]] )))
  }#Fim for
  return(dataSet)
}#********************** Fim base **************************************************************


#***************************Carregando bases de treino e teste***********************************************
print(paste("Carregando bases de treino e teste....."))
print("Aguarde.....")

trainLabels100 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_100labels.txt", header=FALSE)
trainParam100  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_100param.txt", header=FALSE)


trainLabels200 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_200labels.txt", header=FALSE)
trainParam200  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_200param.txt", header=FALSE)


trainLabels300 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_300labels.txt", header=FALSE)
trainParam300  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_300param.txt", header=FALSE)

trainLabels400 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_400labels.txt", header=FALSE)
trainParam400  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_400param.txt", header=FALSE)

trainLabels500 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_500labels.txt", header=FALSE)
trainParam500  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_500param.txt", header=FALSE)

trainLabels600 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_600labels.txt", header=FALSE)
trainParam600  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_600param.txt", header=FALSE)

trainLabels700 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_700labels.txt", header=FALSE)
trainParam700  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_700param.txt", header=FALSE)

trainLabels800 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_800labels.txt", header=FALSE)
trainParam800  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_800param.txt", header=FALSE)

trainLabels900 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_900labels.txt", header=FALSE)
trainParam900  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_900param.txt", header=FALSE)

trainLabels1000 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_1000labels.txt", header=FALSE)
trainParam1000  <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Treino_1000param.txt", header=FALSE)

testLabels1000 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000labels.txt", header=FALSE)
testParam1000 <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000param.txt", header=FALSE)

trainLabels <- list(b1=trainLabels100,b2=trainLabels200,b3=trainLabels300,b4=trainLabels400,b5=trainLabels500,
                    b6=trainLabels600,b7=trainLabels700,b8=trainLabels800,b9=trainLabels900,b10=trainLabels1000)

#testLabels1000_10d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_10dblabels.txt", header=FALSE)
#testParam1000_10d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_10dbparam.txt", header=FALSE)

#testLabels1000_20d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_20dblabels.txt", header=FALSE)
#testParam1000_20d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_20dbparam.txt", header=FALSE)

#testLabels1000_30d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_30dblabels.txt", header=FALSE)
#testParam1000_30d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_30dbparam.txt", header=FALSE)

#testLabels1000_40d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_40dblabels.txt", header=FALSE)
#testParam1000_40d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_40dbparam.txt", header=FALSE)

#testLabels1000_50d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_50dblabels.txt", header=FALSE)
#testParam1000_50d <- read.csv("/home/jean/Dropbox/pos/doutorado/experimentos/KNN-DTW/Teste_1000_50dbparam.txt", header=FALSE)
#**********************************************************************************************************
#Bases de Treino***************************************************
#Base 100
tipo = "treino sem ruído"
dataBase <- trainParam100 
sizeBase <- nrow(dataBase)
train100param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Base 200
dataBase <- trainParam200 
sizeBase <- nrow(dataBase)
train200param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Base 300
dataBase <- trainParam300 
sizeBase <- nrow(dataBase)
train300param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Base 400
dataBase <- trainParam400 
sizeBase <- nrow(dataBase)
train400param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Base 500
dataBase <- trainParam500 
sizeBase <- nrow(dataBase)
train500param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Base 600
dataBase <- trainParam600 
sizeBase <- nrow(dataBase)
train600param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Base 700
dataBase <- trainParam700 
sizeBase <- nrow(dataBase)
train700param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Base 800
dataBase <- trainParam800 
sizeBase <- nrow(dataBase)
train800param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Base 900
dataBase <- trainParam900 
sizeBase <- nrow(dataBase)
train900param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Base 1000
dataBase <- trainParam1000 
sizeBase <- nrow(dataBase)
train1000param <- tratarBases(dataBase,sizeBase,tipo) 
#***********************************************
#Bases de teste ********************************************************************
#Base 1000 sem ruído
tipo = "teste sem ruído"
dataBase <- testParam1000  
sizeBase <- nrow(dataBase)
test1000param <-tratarBases(dataBase,sizeBase,tipo) 


trainParam <- list(b1=train100param,b2=train200param,b3=train300param,b4=train400param,b5=train500param,
                          b6=train600param,b7=train700param,b8=train800param,b9=train900param,b10=train1000param)

#****************************************************************************************************
#Base 1000 com ruído de 10db
#tipo = "teste com ruído de 10db"
#dataBase <- testParam1000_10d
#sizeBase <- nrow(dataBase)
#test1000param10db <- tratarBases(dataBase,sizeBase,tipo) 
#****************************************************************************************************
#Base 1000 com ruído de 20db
#tipo = "teste com ruído de 20db"
#dataBase <- testParam1000_20d
#sizeBase <- nrow(dataBase)
#test1000param20db <-tratarBases(dataBase,sizeBase,tipo)  
#****************************************************************************************************
#Base 1000 com ruído de 30db
#tipo = "teste com ruído de 30db"
#dataBase <- testParam1000_30d
#sizeBase <- nrow(dataBase)
#test1000param30db <- tratarBases(dataBase,sizeBase,tipo) 
#****************************************************************************************************
#Base 1000 com ruído de 40db
#tipo = "teste com ruído de 40db"
#dataBase <- testParam1000_40d
#sizeBase <- nrow(dataBase)
#test1000param40db <- tratarBases(dataBase,sizeBase,tipo) 
#****************************************************************************************************
#Base 1000 com ruído de 50db
#tipo = "teste com ruído de 50db"
#dataBase <- testParam1000_50d
#sizeBase <- nrow(dataBase)
#test1000param50db <- tratarBases(dataBase,sizeBase,tipo) 
#****************************************************************************************************
#*****************************************************************************************

tempoBases<-proc.time() - tempoBaseInicio
tempoBasesFinal<-tempoBases[[3]]



#********************* Probabilidade - Algoritmo EM (Baum-Welch) ********************************************
#maxVerossTest<-function(x,S,m,delta,gamma,mu,sigma2,I=1000,criterio=10^(-6)){
maxVerossBase<-function(x,m,delta,gamma,mu,sigma2,I,criterio){
  S <- 1
  s <- 1
  
  for(i in 1:I){#for inicio-> Iteração ************************************************
    #    print(paste(i,"ª iteração."))
    
    la<-vector("list",S)
    lb<-vector("list",S)
    llk<-rep(0,times=S)
    
    
    fb <-log.alphabeta.norm.uni(x,m,gamma,delta,mu,sigma2)
    la[[s]] <-fb$la
    lb[[s]] <-fb$lb
    c <-max(la[[s]][,T[s]])
    llk[s]<-c+log(sum(abs(la[[s]][,T[s]]-c)))
    
    
    sum.T<-sum(T)
    sum.llk<-sum(llk)
    np <- (m-1)+(m-1)*m + 2*m
    
    #Cálculo AIC e BIC
    AIC <- -2*(sum.llk-np)
    BIC <- -2*sum.llk+np*log(sum.T)
    return(list(mu=mu,sigma2=sigma2,gamma=gamma,
                delta=delta,mllk=sum.llk,AIC=AIC,
                BIC=BIC,iter=i))
    
    
  }#For fim Iteração ******************************************************************************
  
  print(paste("O algoritmo não convergiu depois de ",I," iterações."))
  
  return(NA)
  
}#final função

#********************* Probabilidade Parte do algoritmo EM ********************************************
EM.hmm.norm.uni<-function(x,S,m,delta,gamma,mu,sigma2,I=1000,
                          criterio=10^(-6)){
  mu.next <-mu
  sigma2.next <-sigma2
  delta.next <-delta
  gamma.next <-gamma
  T<-rep(0,times=S)
  for(s in 1:S){
    #T[s]<-length(x[[s]])
    T[s]<-S
  }
  
  for(i in 1:I){#for inicio-> Iteração ************************************************
    probs<-matrix(NA,T[s],m)
    lallprobs<-vector("list",S)
    la<-vector("list",S)
    lb<-vector("list",S)
    llk<-rep(0,times=S)
    
    for(s in 1:S){
      
      for(t in 1:T[s]){
        P<-matriz.P(x[[s]][t],mu,sigma2)
        probs[t,]<-log(diag(P))
      }
      
      
      lallprobs[[s]]<-probs
      fb <-log.alphabeta.norm.uni(x[[s]],m,gamma,delta,
                                  mu,sigma2)
      la[[s]] <-fb$la
      lb[[s]] <-fb$lb
      c <-max(la[[s]][,T[s]])
      llk[s]<-c+log(sum(exp(abs(la[[s]][,T[s]]-c))))
    }
    u.1 <- rep (0, times=m)
    F <-matrix(0,ncol=m,nrow=m)
    for(s in 1:S){
      for (j in 1:m){
        for (k in 1:m){
          F[j,k] <- F[j,k] + gamma[j,k]*
            sum((la[[s]][j,1:(T[s]-1)]+
                   lallprobs[[s]][2:T[s],k]+
                   lb[[s]][k,2:T[s]]-llk[s]))
        }
      }
      u.1 <- u.1 + (la[[s]][,1]+lb[[s]][,1]-llk[s])
    }
    
    for (j in 1:m){#inicio for
      
      aux.div<-0
      aux.mu<-0
      aux.sigma2<-0
      
      for(s in 1:S){
        aux.div<- aux.div + sum((la[[s]][j,]
                                 +lb[[s]][j,]-llk[s]))
        aux.mu<- aux.mu + sum((la[[s]][j,]+
                                 lb[[s]][j,]-llk[s]) * x[[s]] )
      }
      mu.next[j] <- aux.mu / aux.div
      
      
      for(s in 1:S){
        aux.sigma2<- aux.sigma2 + sum((la[[s]][j,]+
                                         lb[[s]][j,]-llk[s])*(x[[s]]-
                                                                mu.next[j])^2 )
      }
      sigma2.next[j]<- aux.sigma2 / aux.div
      
    }#Fim for
    
    gamma.next <- F/apply(F,1,sum)
    delta.next <- u.1/sum(u.1)
    crit <- sum(abs(mu-mu.next))+sum(abs(sigma2-sigma2.next))+
      sum(abs(gamma-gamma.next))+
      sum(abs(delta-delta.next))
    if(is.nan(crit)==TRUE){
      
      return(NA)
    }
    else{
      if(crit<criterio){
        sum.T<-sum(T)
        sum.llk<-sum(llk)
        np <- (m-1)+(m-1)*m + 2*m
        #Cálculo AIC e BIC
        AIC <- -2*(sum.llk-np)
        BIC <- -2*sum.llk+np*log(sum.T)
        return(list(mu=mu,sigma2=sigma2,gamma=gamma,
                    delta=delta,mllk=sum.llk,AIC=AIC,
                    BIC=BIC,iter=i))
      }
      mu <- mu.next
      sigma2 <- sigma2.next
      gamma <- gamma.next
      delta <- delta.next
    }
    
  }#For fim Iteração ******************************************************************************
  print(paste("O algoritmo não convergiu depois de ",I," iterações."))
  return(NA)
}#final função



#****************************** Matriz.P **************************************

matriz.P<-function(x, mu, sigma2){
  m<-length(mu)
  P<-matrix(rep(0, times=m*m),ncol=m,nrow=m)
  for(i in 1:m){
    P[i,i]<-dnorm(x, mean=mu[i], sd=sqrt(sigma2[i]))
  }
  return(P)
}

#********** distribuiçãoDirichlet ************************************************
r.dirichlet<-function(tetha){
  dim<-length(tetha)
  y<-rep(0,times=dim)
  for(i in 1:dim){
    y[i]<-rgamma(1,shape=tetha[i],scale=1)
  }
  x<-y/sum(y)
  return(x)
}

#**************** Log das probabilidades *********************************************
log.alphabeta.norm.uni<-function(x,m,gamma,delta,mu,sigma2){
  T<-length(x)
  lalpha<-lbeta<-matrix(NA,m,T)
  allprobs<-matrix(NA,T,m)
  for(t in 1:T){
    P<-matriz.P(x[t],mu,sigma2)
    allprobs[t,]<-diag(P)
  }
  foo <-delta*allprobs[1,]
  sumfoo<-sum(foo)
  lscale<-log(sumfoo)
  foo <-foo/sumfoo
  lalpha[,1]<-log(foo)+lscale
  for(t in 2:T){
    foo <-foo%*%gamma*allprobs[t,]
    sumfoo<-sum(foo)
    lscale<-lscale+log(sumfoo)
    foo <-foo/sumfoo
    lalpha[,t]<-log(foo)+lscale
  }
  lbeta[,T]<-rep(0,m)
  foo <-rep(1/m,m)
  lscale<-log(m)
  for(t in (T-1):1){
    foo <-gamma%*%(allprobs[t+1,]*foo)
    lbeta[,t]<-log(foo)+lscale
    sumfoo<-sum(foo)
    foo <-foo/sumfoo
    lscale<-lscale+log(sumfoo)
  }
  list(la=lalpha,lb=lbeta)
}

#************** Gera médias e variâncias iniciais  ************************************
#mu -> média
#sigma2 -> variância
mu.sigma2.iniciais<-function(x,S,m){
  if(S==1){
    y<-t(x[[1]])
  }
  else{
    y<-t(x[[1]])
    for(s in 2:S){
      y<-cbind(y,t(x[[s]]))
    }
  }
  y<-sort(y)
  T<-length(y)
  mu<-rep(0,times=m)
  sigma2<-rep(0,times=m)
  for(i in 1:m){
    inicio<-round( (i-1)*T/m + 1 )
    fim<-round( i*T/m )
    mu[i]<-runif(1,y[inicio],y[fim])
  }
  sigma2<-(runif(m, min=(1/4)*sd(y)^2,max=4*sd(y)^2))
  return(list(mu=mu,sigma2=sigma2))
}

#************** Transição ergótica ******************
#gamma-> matriz de transição - 
#delta-> distribuição incial *************************

trans.ergotica<-function(m){
  gamma<-matrix(rep(0,times=m^2),ncol=m,nrow=m)
  I<-diag(m)
  verifica<-0
  while(verifica==0){
    for(i in 1:m){
      gamma[i,]<-r.dirichlet(diag(I))
    }
    if(det(gamma-I)==0){
      verifica<-1
    }
  }
  delta<-solve(t(I-gamma+1),rep(1,m))
  return(list(gamma=gamma,delta=delta))
}

#************* Funções gerais ******************************************************************
#******************************************************************************************

#********* Separando classes das bases *******************************************

classBase<-function(labels,param,c,sizeBase){
  class <- list()
  label <- list()
  T<-sizeBase
  for (i in 1:T){ 
    if(labels[i,1] == c){
      class[i] <- param[i]
      label[i] <- c
    }
    #  print(paste("Label ",  label[i]))
  }
  
  #Excluindo NULLs do list
  classB <- Filter(Negate(is.null),class)
  labelB <- Filter(Negate(is.null),label)
  # print(paste("A classe ", c, "tem ", length(classB), "instâncias da base de ", sizeBase))
  #return(classB)
  return(list(classB=classB,labelB=labelB))
  
}
#************************ Separando Classes *************************************************

#Cálculo das estimativas da base de treino e testes.
runEstimations<-function(labels,dataBase,sizeBaseEst,orderModel){
  #print("**************************************************************************")
  #print(paste("Teste: analisando a base de teste de tamanho",sizeBaseEst))
  #print("Aguarde......")
  #print("***************************************************************************")
  mu <- list()
  sigma2<- list()
  gamma <- list()
  delta <- list()
  mllk <- list()
  mmllk <- list()
  AIC <- list()
  BIC <- list()
  iter <- list()
  
  for (i in 1:sizeBaseEst){
    valores <- maxVerossBase(dataBase[[i]],orderModel, baseGammaDelta$delta,baseGammaDelta$gamma,
                             baseMuSigma2$mu,baseMuSigma2$sigma2,I=10000,criterio=10^(-6))
    
    mu[i] <- valores$mu
    sigma2[i]<- valores$sigma2
    gamma[i] <- valores$gamma
    delta[i] <- valores$delta
    mllk[i] <- valores$mllk 
    AIC[i] <- valores$AIC
    BIC[i] <- valores$BIC
    iter[i] <- valores$iter
    
  }
  
  aux <- mean(as.numeric(aux <- Filter(Negate(is.null),mllk[[1]])))
  mmllk  <- as.numeric(rep(aux,length(mllk)))
  
  
  return(list(mu=mu, sigma2=sigma2, gamma=gamma, mu=mu, 
              delta=delta, mllk=mllk, mmllk=mean(mmllk), AIC=AIC, BIC=BIC,
              iter=iter))
  
  print(paste("Gama: ", gamma()))
}

#**************** Fim cálculo de estimativas ************************************************




######################### Begin Function ANN ###################################

# apresentações de resultados do RNA_HMM
#*************************************************************************************************
#*****Apresentação dos resultados *********************************************

apresentacaoRNA_HMM<-function(){
  
  #***************Obtendo resultados ****************************
  
  for (j in 1:nrBase){
    x1<-list()
    for (i in 1:Ii){
      if(j){
        aux<-rALLAccuracyItANN[[i]][[j]]$errorTotalBaseTest
        x1[i]<-aux
        #Menor resultado na base
        menorResultPorBase[j]<-list(min(as.numeric(x1)))
        #resultados das bases
        resultsBases[j]<-list(x1)
        #melhor resultado de Iteração da base
        melhorItBase[j]<-i 
        #media da bases
        mediaPorBase[j]<-list(mean(as.numeric(x1)))
        #Melhor resultado da base 
        #tempo por base
        
      }
      
    }
    
  }
  
  
  
  #*************Base de 100 x 1000 sem ruído ***************************************************
  for (i in 1:Ii){
    
    for (j in 1:nrBase){
      #*******Resultados por treino *********************************************************************
      sizeTrain = length(baseTrain<-trainParam[[j]])
      print("")
      print(paste("************Resultados da base: ", sizeTrain, " do ", i ,"º treino ******************"))
      print("*****************************************************************")
      print("")
      print("************************Resultados dos Parâmetros do HMM ************************************")
      print("")
      print("************Parâmetros iniciais do HMM**************************************************")
      print("****************************************************************************************")
      print(paste("Média inicial: ", baseMuSigma2$mu))
      print("*****************************************************")
      print(paste("Variância inicial ", baseMuSigma2$sigma2))
      print("*****************************************************")
      print(paste("Probabilidade de estado inicial Gaussiana: ", inicialGammaDelta[[j]]$delta))
      print("*****************************************************")
      print(paste("Probabilidade transição inicial: ", inicialGammaDelta[[j]]$gamma))
      print("*************************************************")
      print("****************************************************************************************")
      print(paste("Média final: ", rALLAccuracyItANN[[i]][[j]]$pHMM[[j]]$mu[j]))
      print("*****************************************************")
      print(paste("Variância final: ", rALLAccuracyItANN[[i]][[j]]$pHMM[[j]]$sigma2[j]))
      print("*****************************************************")
      print(paste("Probabilidade de estado final : ", rALLAccuracyItANN[[i]][[j]]$pHMM[[j]]$delta[j]))
      print("*****************************************************")
      print(paste("Probabilidade transição final: ",rALLAccuracyItANN[[i]][[j]]$pHMM[[j]]$gamma[j]))
      print("*************************************************")
      print(paste("Média de estimação da máxima verrosimilhança (Mv) por classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("****************************************************************************************")
      for (m in 1:10){
        print(paste("Base ", sizeTrain, " = Mv da Classe: ", m , " =  ",  rALLAccuracyItANN[[i]][[j]]$mmllk2[[m]]))
        print("********************************************************")
      }
      print("****************************************************************************************")
      
      print("")
      print("")
      print("*******************Resultados dos Parâmetros da RNA ************************************")
      print("************Parâmetros da RNA***********************************************************")
      print("****************************************************************************************")
      print(paste("Número de epócas: ", rALLAccuracyItANN[[i]][[j]]$pANN[[1]]$epocas))
      print("*****************************************************")
      print(paste("Taxa de aprendizagem ", rALLAccuracyItANN[[i]][[j]]$pANN[[1]]$taxaAprendizagem))
      print("*****************************************************")
      print(paste("Momento: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$momento))
      print("*****************************************************")
      print(paste("Pesos Iniciais 0: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$pesos0))
      print("*************************************************")
      print(paste("Pesos Iniciais 1: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$pesos1))
      print("*************************************************")
      print(paste("Pesos Finais 0: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$pesos01))
      print("*************************************************")
      print(paste("Pesos Finais 1: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$pesos11))
      
      
      print("************Acurácia da RNA**************************************************")
      print("****************************************************************************************")
      print(paste("TREINO = Taxa de erro por classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("****************************************************************************************")
      for (m in 1:10){
        print(paste("Base ", sizeTrain, " = Erro da Classe: ", m , " =  ",  rALLAccuracyItANN[[i]][[j]]$tgErrorANNTestTotal[m]))
        print("********************************************************************")
      }
      print("***********************************************************************")
      print(paste("TREINO = Taxa de erro  da base : ", sizeTrain, " do ", i ,"º treino =", rALLAccuracyItANN[[i]][[j]]$errorTotalBaseTrain))
      print("***********************************************************************")
      # print(paste("TESTE = Taxa de erro TESTE das classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("***********************************************************************")
      print(paste("TESTE = Taxa de erro da base : ", sizeTrain, " do ", i ,"º treino = ", rALLAccuracyItANN[[i]][[j]]$errorTotalBaseTest))
      print("********************************************************************************************")
      print("")
      print("***********************************************************************")
      print(paste("Tempo de Processamento da base : ", sizeTrain, " =  ", (as.numeric(tempoBases[[i]][[j]])/360)*10, " no ", i, "º treino"))
      print("********************************************************************************************")
      print("")
      
      #**********End results***********************************************************  
      
      
    } #********end for 2 **********************************************
    
    
  } #end
  
  print("**********************************************")
  print("Resultados Gerais do Algoritmo RNN_HMM")
  print("**********************************************")
  
  
  
  #***************Apresentação resultados ****************************
  print("Apresentação de Resultados")
  print("**********************************************************************************")
  print(paste("Tempo de Processamento para Carregamento das Bases", as.numeric(tempoBasesFinal/60), "Minutos"))
  print("**********************************************************************************")
  print("")
  
  for (j in 1:nrBase){
    sizeTrain = length(baseTrain<-trainParam[[j]])
    print(paste("Resultados da base de ",  sizeTrain))
    print("****************************************************************************")
    print("****************************************************************************")
    for (i in 1:Ii){
      #resultados das bases
      print(paste(i, "ª iteração"))
      print(paste("Tempo de Execução :", (as.numeric(tempoBases[[i]][[j]]/3600))*100, "minutos" ))
      print(paste("Taxa de Erro = ", resultsBases[[j]][[i]]))
      print("**********************************************")
      #resultados das bases
      
    }
    
    print(paste("Menor taxa de Erro nas ", Ii , " Iterações =" ,menorResultPorBase[j]))
    print("**********************************************")
    print("**********************************************")
    print(paste("Media da Taxa de Erro da base em todas iterações = ", mediaPorBase[j]))
    print("****************************************************************************")
    print("****************************************************************************")
    print("")
    
  }
  
  s<-100
  b<-0
  for (j in 1:nrBase){
    if(as.numeric(mediaPorBase[j]) < s)
      s = mediaPorBase[j] 
    b = j
    #print(s)
  }
  
  print(paste(" Menor media entre as bases é  :" , s , " da base de", length(baseTrain<-trainParam[[b]])))
  print("****************************************************************************")
  print("****************************************************************************")
  
  
}#***** RNA FIM - Apresentação dos resultados ***************************************




#*********Individual *********************************************************************************

#***************** Apresentação Individual ***************************************

apresentacaoRNA_HMMInd<-function(int1,base1){
  
  int<-int1
  base<-base1
  #***************Obtendo resultados ****************************
  
  
  
  #*************Base de 100 x 1000 sem ruído ***************************************************
  for (i in int:int){
    
    for (j in base:base){
      #*******Resultados por treino *********************************************************************
      sizeTrain = length(baseTrain<-trainParam[[j]])
      print("")
      print(paste("************Resultados da base: ", sizeTrain, " do ", i ,"º treino ******************"))
      print("*****************************************************************")
      print("")
      print("************************Resultados dos Parâmetros do HMM ************************************")
      print("")
      print("************Parâmetros iniciais do HMM**************************************************")
      print("****************************************************************************************")
      print(paste("Média inicial: ", baseMuSigma2$mu))
      print("*****************************************************")
      print(paste("Variância inicial ", baseMuSigma2$sigma2))
      print("*****************************************************")
      print(paste("Probabilidade de estado inicial Gaussiana: ", inicialGammaDelta[[j]]$delta))
      print("*****************************************************")
      print(paste("Probabilidade transição inicial: ", inicialGammaDelta[[j]]$gamma))
      print("*************************************************")
      print("****************************************************************************************")
      print(paste("Média final: ", rALLAccuracyItANN[[i]][[j]]$pHMM[[j]]$mu[j]))
      print("*****************************************************")
      print(paste("Variância final: ", rALLAccuracyItANN[[i]][[j]]$pHMM[[j]]$sigma2[j]))
      print("*****************************************************")
      print(paste("Probabilidade de estado final : ", rALLAccuracyItANN[[i]][[j]]$pHMM[[j]]$delta[j]))
      print("*****************************************************")
      print(paste("Probabilidade transição final: ",rALLAccuracyItANN[[i]][[j]]$pHMM[[j]]$gamma[j]))
      print("*************************************************")
      print(paste("Média de estimação da máxima verrosimilhança (Mv) por classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("****************************************************************************************")
      for (m in 1:10){
        print(paste("Base ", sizeTrain, " = Mv da Classe: ", m , " =  ",  rALLAccuracyItANN[[i]][[j]]$mmllk2[[m]]))
        print("********************************************************")
      }
      print("****************************************************************************************")
      
      print("")
      print("")
      print("*******************Resultados dos Parâmetros da RNA ************************************")
      print("************Parâmetros da RNA***********************************************************")
      print("****************************************************************************************")
      print(paste("Número de epócas: ", rALLAccuracyItANN[[i]][[j]]$pANN[[1]]$epocas))
      print("*****************************************************")
      print(paste("Taxa de aprendizagem ", rALLAccuracyItANN[[i]][[j]]$pANN[[1]]$taxaAprendizagem))
      print("*****************************************************")
      print(paste("Momento: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$momento))
      print("*****************************************************")
      print(paste("Pesos Iniciais 0: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$pesos0))
      print("*************************************************")
      print(paste("Pesos Iniciais 1: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$pesos1))
      print("*************************************************")
      print(paste("Pesos Finais 0: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$pesos01))
      print("*************************************************")
      print(paste("Pesos Finais 1: ", rALLAccuracyItANN[[i]][[j]]$pANN[[j]]$pesos11))
      
      
      print("************Acurácia da RNA**************************************************")
      print("****************************************************************************************")
      print(paste("TREINO = Taxa de erro por classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("****************************************************************************************")
      for (m in 1:10){
        print(paste("Base ", sizeTrain, " = Erro da Classe: ", m , " =  ",  rALLAccuracyItANN[[i]][[j]]$tgErrorANNTestTotal[m]))
        print("********************************************************************")
      }
      print("***********************************************************************")
      print(paste("TREINO = Taxa de erro  da base : ", sizeTrain, " do ", i ,"º treino =", rALLAccuracyItANN[[i]][[j]]$errorTotalBaseTrain))
      print("***********************************************************************")
      # print(paste("TESTE = Taxa de erro TESTE das classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("***********************************************************************")
      print(paste("TESTE = Taxa de erro da base : ", sizeTrain, " do ", i ,"º treino = ", rALLAccuracyItANN[[i]][[j]]$errorTotalBaseTest))
      print("********************************************************************************************")
      print("")
      print("***********************************************************************")
      print(paste("Tempo de Processamento da base : ", sizeTrain, " =  ", as.numeric(tempoBases[[i]][[j]])/60, " no ", i, "º treino"))
      print("********************************************************************************************")
      print("")
      print(paste("Tempo de Processamento para Carregamento das Bases", as.numeric(tempoBasesFinal/60), "Minutos"))
      
      #**********End results***********************************************************  
      
      
    } #********end for 2 **********************************************
    
    
  } #end
  
  
  
}#***** HMM - FIM - Apresentação dos resultados ***************************************




#***************** Apresentação indidividual Fim *********************************


#********************** Apresentação Resumida ****************************************





#*****************************************************************************************************



#********************* Funções gerais ANN******************************************
sigmoide = function (soma){
  return(1/(1 + exp(-soma)))
}

sigmoideDerivada = function (sig){
  return(sig * (1 - sig))
}


stepFunction = function(soma){
  
  if(soma >= 1){
    return(1)
  }
  return(0)
}


#************************Treinamento da Rede Neural Perceptron ***********************
trainANN_P<-function(classPar1,classLabel1,ep,ta,mo){
  
  a <- classPar1$mllk
  b <- classPar1$mmllk
  c <- classPar1$AIC
  d <- classPar1$BIC
  e <- classPar1$mu
  f <- classPar1$sigma2
  g <- classPar1$gamma
  h <- classPar1$delta
  
  ep1<-ep
  ta1<-ta
  mom1<-mo
  
  
  #inp <- as.numeric(cbind(a,b,c,d,e,f,g,h))
  inp <- as.numeric(cbind(a))
  #input <- matrix(c(inp), nrow = length(a), ncol = 8, byrow = T)
  input <- matrix(c(inp), nrow = length(a), ncol = 2, byrow = T)
  
  entradas = input
  saidas = as.numeric(classLabel1)
  
  #pesos <- matrix(c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
  pesos <- matrix(c(0.0))
  pesos0 <- matrix(runif(6, min = -1, max = 1), nrow = 2, ncol = 3, byrow = T)
  pesos01 <- pesos0
  pesos1 <- matrix(runif(3, min = -1, max = 1), nrow = 3, ncol = 1, byrow = T)
  pesos11 <- pesos1
  
  
  epocas <- 10000
  taxaAprendizagem <- 0.3
  momento <- 1
  errorTotal <-list()
  
  calculaSaida = function(registro){
    soma <- registro%*%pesos
    return(stepFunction(soma))
    #return(sigmoide(soma))
  }
  
  for (j in 1:epocas){
    erroTotal = 0
    for(i in 1:length(saidas)){
      saidaCalculada <- calculaSaida(c(entradas[i,]))
      erro = abs(saidas[i]) - saidaCalculada
      erroTotal <- erroTotal + erro
      for(j in 1:length(pesos)){
        pesos[j] <- pesos[j] + (taxaAprendizagem * entradas[i,j] * erro)
        #print(paste("Pesos atualizados :", pesos[j]))
      }
      
    } 
    #print(paste("Total de erros :", erroTotal))
  } 
  
  
  for(i in 1:length(saidas)){
    #print(calculaSaida(entradas[i,]))
    calculaSaida(entradas[i,])
  }
  
  return(erroTotal)
  
}

#************************Treinamento da Rede Neural Perceptron (Back Propagation) **************
trainANN_MLP<-function(classPar1,classLabel1,ep,ta,mo,mmllk2){
  
  #trainANN_MLP<-function(classPar1,classLabel1,e,t,m){
  
  a <- classPar1$mllk
 # b <- classPar1$mmllk
  c <- classPar1$AIC
  d <- classPar1$BIC
  e <- classPar1$mu
  f <- classPar1$sigma2
  g <- classPar1$gamma
  h <- classPar1$delta
  
  ep1<-ep
  ta1<-ta
  mo1<-mo
  
  
  b<-list()
  modelTrain = mmllk2
  for (i in 1:sizeBase){
    b[i]<-modelTrain
  }#forEND
  
  #print(str(b))
  #print(str(b))
  
  #inp <- as.numeric(cbind(a,b,c,d,e,f,g,h))
  inp <- as.numeric(cbind(a))
 
  #input <- matrix(c(inp), nrow = length(a), ncol = 8, byrow = T)
  input <- matrix(c(inp), nrow = length(a), ncol = 1, byrow = T)
  
  entradas = input
  saidas = as.numeric(classLabel1)
  
  #pesos <- matrix(c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
  #pesos <- matrix(c(0.0,0.0))
  pesos <- matrix(c(0.0))
  pesos0 <- matrix(runif(6, min = -1, max = 1), nrow = 1, ncol = 3, byrow = T)
  pesos01 <- pesos0
  pesos1 <- matrix(runif(3, min = -1, max = 1), nrow = 3, ncol = 1, byrow = T)
  pesos11 <- pesos1
  
  
  epocas <- ep1
  taxaAprendizagem <- ta1
  momento <- mo1
  errorTotal <-list()
  
  for (j in 1:epocas){
    camadaEntrada <- entradas
    somaSinapse0 <- camadaEntrada %*% pesos0
    camadaOculta <- sigmoide(somaSinapse0)
    
    somaSinapse1 <-  camadaOculta %*% pesos1
    camadaSaida <- sigmoide(somaSinapse1)
    
    erroCamadaSaida <- saidas - camadaSaida
    mediaAbsoluta <- mean(abs(erroCamadaSaida))
    errorTotal[j] <- mediaAbsoluta
    #print(paste("Erro agora : ", mediaAbsoluta))
    
    derivadaSaida <- sigmoideDerivada(camadaSaida) 
    deltaSaida <- erroCamadaSaida * derivadaSaida
    
    pesos1Transposta <- t(pesos1)
    deltaSaidaXPeso <-  deltaSaida %*%  pesos1Transposta
    deltaCamadaOculta <- deltaSaidaXPeso * sigmoideDerivada(camadaOculta)
    
    camadaOcultaTransposta <- t(camadaOculta)
    pesosNovo1 <- camadaOcultaTransposta %*% deltaSaida
    pesos1 <- (pesos1 * momento) + (pesosNovo1 * taxaAprendizagem)
    
    camadaEntradaTransposta <- t(camadaEntrada)
    pesosNovo0 <- camadaEntradaTransposta %*% deltaCamadaOculta
    pesos0 <- (pesos0 * momento) + (pesosNovo0 * taxaAprendizagem)
    
  }
  
  calculaSaida = function(registro){
    soma <- registro%*%pesos
    return(stepFunction(soma))
    #return(sigmoide(soma))
  }
  
  #print(paste("ErroFora:" , errorTotal[[epocas]]))
  for(i in 1:length(saidas)){
    #print(calculaSaida(entradas[i,]))
    calculaSaida(entradas[i,])
  }
  
  #print(paste("P0",str(pesos0)))
  #print(paste("P1",str(pesos1)))
  
 # print(paste("Treino", pesos))
  
  return(list(errorTotal=errorTotal[[epocas]], pesos=pesos,pesos0=pesos0, pesos1=pesos1, pesos01=pesos01, pesos11=pesos11,
              epocas=epocas, taxaAprendizagem=taxaAprendizagem, momento=momento))
  
}

#************************Teste da Rede Neural ***********************

testANN_MLP<-function(classPar1,classLabel1,parTest1,ep,ta,mo,mmllk2,p00,p11){
#testANN_MLP<-function(classPar1,classLabel1,parTest1,ep,ta,mo,mmllk2,p00,p11){
  
  
  a <- classPar1$mllk
  #b <- classPar1$mmllk
  #b <- mmllk2
  c <- classPar1$AIC
  d <- classPar1$BIC
  e <- classPar1$mu
  f <- classPar1$sigma2
  g <- classPar1$gamma
  h <- classPar1$delta
  
  ep1<-ep
  ta1<-ta
  mo1<-mo
  
  p1<-p00
  p2<-p11
  
  b<-list()
  modelTrain = mmllk2
  
  
  
  for (i in 1:sizeBase){
    b[i]<-modelTrain
  }
  
 
  #inp <- as.numeric(cbind(a,b,c,d,e,f,g,h))
  inp <- as.numeric(cbind(a,b))
  #input <- matrix(c(inp), nrow = length(a), ncol = 8, byrow = T)
  input <- matrix(c(inp), nrow = length(a), ncol = 2, byrow = T)
  
  entradas = input
  saidas = as.numeric(classLabel1)
  
  #pesos <- matrix(c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
  pesos <- matrix(c(0.0,0.0))
 # pesos0 <- matrix(runif(6, min = -1, max = 1), nrow = 2, ncol = 3, byrow = T)
 # pesos1 <- matrix(runif(3, min = -1, max = 1), nrow = 3, ncol = 1, byrow = T)
  
   pesos0 <-p1
   pesos1 <-p2
 
  
  epocas <- ep1
  taxaAprendizagem <- ta1
  momento <- mo1
  errorTotal <-list()
  
  for (j in 1:epocas){
    camadaEntrada <- entradas
    somaSinapse0 <- camadaEntrada %*% pesos0
    camadaOculta <- sigmoide(somaSinapse0)
    
    somaSinapse1 <-  camadaOculta %*% pesos1
    camadaSaida <- sigmoide(somaSinapse1)
    
    erroCamadaSaida <- saidas - camadaSaida
    mediaAbsoluta <- mean(abs(erroCamadaSaida))
    errorTotal[j] <- mediaAbsoluta
    #print(paste("Erro agora : ", mediaAbsoluta))
    
    derivadaSaida <- sigmoideDerivada(camadaSaida) 
    deltaSaida <- erroCamadaSaida * derivadaSaida
    
    pesos1Transposta <- t(pesos1)
    deltaSaidaXPeso <-  deltaSaida %*%  pesos1Transposta
    deltaCamadaOculta <- deltaSaidaXPeso * sigmoideDerivada(camadaOculta)
    
    camadaOcultaTransposta <- t(camadaOculta)
    pesosNovo1 <- camadaOcultaTransposta %*% deltaSaida
    pesos1 <- (pesos1 * momento) + (pesosNovo1 * taxaAprendizagem)
    
    camadaEntradaTransposta <- t(camadaEntrada)
    pesosNovo0 <- camadaEntradaTransposta %*% deltaCamadaOculta
    pesos0 <- (pesos0 * momento) + (pesosNovo0 * taxaAprendizagem)
    
  }
  
  calculaSaida = function(registro){
    soma <- registro%*%pesos
    return(stepFunction(soma))
    #return(sigmoide(soma))
  }
  
  #print(paste("ErroFora:" , errorTotal[[epocas]]))
  for(i in 1:length(saidas)){
    #print(calculaSaida(entradas[i,]))
    calculaSaida(entradas[i,])
  }
  
  return(errorTotal[[epocas]])
  
}

#***************** Teste Geral ********************************************************************
#mllkTest,modelClass,baseLabelTest,p00,p11,p0,p1,ep1,ta1,mo1
#testANN_MLPGeral<-function(sampleClass,modelClass,p00,p11,p0,p1,ep,ta,mo){
testANN_MLPGeral<-function(sampleClass,modelClass,label1,p01,p11,ep,ta,mo){
  
  par1<-sampleClass
  ep1<-ep
  ta1<-ta
  mo1<-mo
  
  p0 = p01

  #print(p0)
  
  p1 = p11
  
  x0 = unlist(p0)
  #l1 = c(x0[1],x0[3],x0[5])
  l1 = c(x0[1],x0[2],x0[3])
  
 
  pesos0 = rbind(l1)
  
  x1 = unlist(p1)
  c1 = c(x1[1])
  c2 = c(x1[2])
  c3 = c(x1[3])
  pesos1 = rbind(c1,c2,c3)
  
  
  par2<-list()
  modelTrain = modelClass
  
  for (i in 1:sizeBase){
    par2[i]<-modelTrain
  }
  
  #inp <- as.numeric(cbind(a,b,c,d,e,f,g,h))
  inp <- as.numeric(cbind(par1))
  #input <- matrix(c(inp), nrow = length(a), ncol = 8, byrow = T)
  input <- matrix(c(inp), nrow = length(par1), ncol = 1, byrow = T)
  
  entradas = input
  
  label = matrix()
  
  sizeTest = length(testLabels1000)
  
  for (i in 1:sizeTest){
    label[i]<-testLabels1000[i,]
  }
  
  saidas = as.numeric(label)
  
  #pesos <- matrix(c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
   #pesos <- matrix(c(0.0,0.0))
   pesos <- matrix(c(0.0))
  # pesos0 <- matrix(runif(6, min = -1, max = 1), nrow = 2, ncol = 3, byrow = T)
  # pesos1 <- matrix(runif(3, min = -1, max = 1), nrow = 3, ncol = 1, byrow = T)
  
  #pesos0 <-p0
  #pesos1 <-p1
  
  
  epocas <- ep1
  taxaAprendizagem <- ta1
  momento <- mo1
  errorTotal <-list()
  
  for (j in 1:epocas){
    camadaEntrada <- entradas
    somaSinapse0 <- camadaEntrada %*% pesos0
    camadaOculta <- sigmoide(somaSinapse0)
    
    somaSinapse1 <-  camadaOculta %*% pesos1
    camadaSaida <- sigmoide(somaSinapse1)
    
    erroCamadaSaida <- saidas - camadaSaida
    mediaAbsoluta <- mean(abs(erroCamadaSaida))
    errorTotal[j] <- mediaAbsoluta
    #print(paste("Erro agora : ", mediaAbsoluta))
    
    derivadaSaida <- sigmoideDerivada(camadaSaida) 
    deltaSaida <- erroCamadaSaida * derivadaSaida
    
    pesos1Transposta <- as.numeric(t(pesos1))
    deltaSaidaXPeso <-  deltaSaida %*%  pesos1Transposta
    deltaCamadaOculta <- deltaSaidaXPeso * sigmoideDerivada(camadaOculta)
    
    camadaOcultaTransposta <- t(camadaOculta)
    pesosNovo1 <- camadaOcultaTransposta %*% deltaSaida
    pesos1 <- (pesos1 * momento) + (pesosNovo1 * taxaAprendizagem)
   # pesos1 <- p1
    
    camadaEntradaTransposta <- t(camadaEntrada)
    pesosNovo0 <- camadaEntradaTransposta %*% deltaCamadaOculta
    pesos0 <- (pesos0 * momento) + (pesosNovo0 * taxaAprendizagem)
    #pesos0 <- p0
    
  }
  calculaSaida = function(registro){
    soma <- registro%*%pesos
    return(stepFunction(soma))
    #return(sigmoide(soma))
  }
  
  #print(paste("ErroFora:" , errorTotal[[epocas]]))
  for(i in 1:length(saidas)){
    #print(calculaSaida(entradas[i,]))
    calculaSaida(entradas[i,])
  }
  
  return(errorTotal[[epocas]])
}


#***************** Teste Geral final ***************************************************************

avaliacaoBaseRNA<-function(order,labelsBaseTrain,paramBaseTrain,labelsBaseTest,paramBaseTest,ep,ta,mo){
  #Tratar base individualmente *****************************************************
  #Variáveis globais
  orderBase<-order
  sizeTrainT<-10
  
  #****************************************** #Bases*********************************************************************
  #Preparação da avaliação (Treinamento e teste)
  #train
  baseTrain<-paramBaseTrain
  baseLabelTrain<-labelsBaseTrain
  sizeTrain<-length(baseTrain)
  
  #Parâmtros RNN
  ep1<-ep
  ta1<-ta
  mo1<-mo
  
  pesos0<-list()
  pesos1<-list()
  
  cEstimations <- list()
  cErrorANNTrain <- list()
  cErrorANNTrainTotal <- list()
  
  #Test
  baseTest<-paramBaseTest
  baseLabelTest<-labelsBaseTest
  sizeTest<-length(baseTest)
  
  tEstimations <- list()
  cErrorANNTest <- list()
  cErrorANNTestTotal <- list()
  
  tgEstimations <- list()
  tgErrorANNTestTotal <- list()
  
  pHMM <- list()
  pANN <- list()
  mmllk2 <-list()
  amostrasTrain<-list()
  #amostrasTest<-list()
  print("********************************************************************************")
  print("Iniciando o processando base de Teste...Aguarde********")
  
  tgEstimations<-runEstimations(baseLabelTest,baseTest,sizeTest,orderBase)
  mllkTest<-tgEstimations$mllk
  
  print("Finaliziando o processando base de Teste ***")
  print("********************************************************************************")
 
  
  #Tratar classes individualmente em cada base *****************************************************
  for (i in 1:sizeTrainT){
    c = classBase(baseLabelTrain,baseTrain,i,sizeTrain) 
    cParTrain = c$classB
    cLabelTrain = c$labelB
    cSizeTrain = length(cParTrain)
    #aTrain<-cSizeTrain
    #amostrasTrain[i]<-aTrain
   # print(paste("Base Treino->", sizeTrain, " classe ", i, " tem ", cSizeTrain, "amostras"))
  #  print("******************************************************************************************")
    #  print(paste("Treinamento: calculando estimativas da classe", i, "da base de", sizeTrain))
    print(paste("Base->", sizeTrain, " ->  Processando Classe:", i, "****"))
    
    cEstimations <- runEstimations(cLabelTrain,cParTrain,cSizeTrain,orderBase)
    mmllk2[i]<-cEstimations$mmllk
    pHMM[i] <- list(cEstimations)
    modelClass<-mmllk2[i]
   
     erro <-trainANN_MLP(cEstimations,cLabelTrain,ep1,ta1,mo1,mmllk2)
     auxP0<-erro$pesos0
     pesos0[i]<-list(auxP0)
     auxP1<-erro$pesos1
     pesos1[i]<-list(auxP1)
   
    pANN[i] <- list(erro)
    # print(paste("pANN", str(erro)))
    cErrorANNTrain[i]<-erro$errorTotal
    
   # print(paste("Base ", sizeTrain, " -> Erros da classe ", i, " =", cErrorANNTrain[i]))
    cErrorANNTrainTotal[i] <- cErrorANNTrain[i]
    
    #******************************   Testes **************************************************
    #c = classBase(baseLabelTest,baseTest,i,sizeTest) 
    #cParTest = c$classB
    #cLabelTest = c$labelB
    #cSizeTest = length(cParTest)
    #print(paste("Base Teste->", sizeTrain, " classe ", i, " tem ", cSizeTrain, "amostras"))
    #print("******************************************************************************************")
    #parTest = erro
    #aTest<-cSizeTest
    #amostrasTest[i]<-aTest
    
    
    #tEstimations<-runEstimations(cLabelTest,cParTest,cSizeTest,orderBase)
    #erro <- trainANN_P(cEstimations,cLabelTrain)
  #  erroT<-testANN_MLP(tEstimations,cLabelTest,parTest,ep1,ta1,mo1,mmllk2,p0,p1)
   # cErrorANNTest[i] <-  erroT
  #  cErrorANNTestTotal[i] <- cErrorANNTest[i]
    
    #  print("***********************************************************************************************") 
    
    
    #******************************  Fim  Testes **************************************************
    
    
    #print("***********************************************************************************************") 
  }
  
  
 
  for (i in 1:sizeTrainT){
    
    modelClass = mmllk2[i]
    #print(paste("Pesos :", pe = pesos[i]))
    p0 = pesos0[i]
    p1 = pesos1[i]
    labels = baseLabelTest
 
   tgErrorANNTestTotal[i]<-testANN_MLPGeral(mllkTest,modelClass,labels,p0,p1,ep1,ta1,mo1)
   
   print(paste("Base ", sizeTrain, " -> Erros da classe ", i, " =", tgErrorANNTestTotal[i]))
   
  }
  #print("***********************************************************************************************") 
  # ******************************* Train **************************************************
  
  errorTotalBaseTrain <- mean(as.numeric(aux <- Filter(Negate(is.null),cErrorANNTrainTotal)))
  print(paste("Media de erros de treinamento da base", sizeTrain, " = ", errorTotalBaseTrain, "%"))
  #print("***********************************************************************************************") 
  
  #******************************* Teste *************************** 
 # errorTotalBaseTest <- mean(as.numeric(aux <- Filter(Negate(is.null),cErrorANNTestTotal)))
  #print("***********************************************************************************************") 
  #print(paste("Total de erros de teste da base", sizeTrain, " = ", errorTotalBaseTest, "%"))
  #print("***********************************************************************************************") 
  
  #******************************* Teste *************************** 
  errorTotalBaseTestG <- mean(as.numeric(aux <- Filter(Negate(is.null), tgErrorANNTestTotal)))
  #print("***********************************************************************************************") 
  print(paste("Média de erros de teste da base", sizeTrain, " = ", errorTotalBaseTestG, "%"))
  #print("***********************************************************************************************") 
  
  
  #return(list(pHMM=pHMM,pANN=pANN,cErrorANNTrain=cErrorANNTrain,errorTotalBaseTrain=errorTotalBaseTrain,
   #           cErrorANNTest=cErrorANNTest, errorTotalBaseTest=errorTotalBaseTest,mmllk2=mmllk2,
    #          amostrasTrain=amostrasTrain,amostrasTest=amostrasTest))
  
  return(list(pHMM=pHMM,pANN=pANN,cErrorANNTrain=cErrorANNTrain,errorTotalBaseTrain=errorTotalBaseTrain,
              errorTotalBaseTestG=errorTotalBaseTestG,mmllk2=mmllk2,pesos0=pesos0,pesos1=pesos1,
              tgEstimations=tgEstimations,tgErrorANNTestTotal=tgErrorANNTestTotal))
  
  
  
  
}#FIm função
#***********Fim base individual *************************************************************

#************************************End ANN Functions *****************************************



# ################# Funções HMM #################################################

#*****Apresentação dos resultados *********************************************

apresentacaoHMM<-function(){
  
  #***************Obtendo resultados ****************************
  
  for (j in 1:nrBase){
    x1<-list()
    for (i in 1:Ii){
      if(j){
        aux<-rALLAccuracyIt[[i]][[j]]$errorTotalBaseTest
        x1[i]<-aux
        #Menor resultado na base
        menorResultPorBase[j]<-list(min(as.numeric(x1)))
        #resultados das bases
        resultsBases[j]<-list(x1)
        #melhor resultado de Iteração da base
        melhorItBase[j]<-i 
        #media da bases
        mediaPorBase[j]<-list(mean(as.numeric(x1)))
        #Melhor resultado da base 
        #tempo por base
        
      }
      
    }
    
  }
  
  
  
  #*************Base de 100 x 1000 sem ruído ***************************************************
  for (i in 1:Ii){
    
    for (j in 1:nrBase){
      #*******Resultados por treino *********************************************************************
      sizeTrain = length(baseTrain<-trainParam[[j]])
      print("")
      print(paste("************Resultados da base: ", sizeTrain, " do ", i ,"º treino ******************"))
      print("*****************************************************************")
      print("")
      print("************************Resultados dos Parâmetros do HMM ************************************")
      print("")
      print("************Parâmetros iniciais do HMM**************************************************")
      print("****************************************************************************************")
      print(paste("Média inicial: ", baseMuSigma2$mu))
      print("*****************************************************")
      print(paste("Variância inicial ", baseMuSigma2$sigma2))
      print("*****************************************************")
      print(paste("Probabilidade de estado inicial Gaussiana: ", inicialGammaDelta[[j]]$delta))
      print("*****************************************************")
      print(paste("Probabilidade transição inicial: ", inicialGammaDelta[[j]]$gamma))
      print("*************************************************")
      print("****************************************************************************************")
      print(paste("Média final: ", rALLAccuracyIt[[i]][[j]]$pHMM[[j]]$mu[j]))
      print("*****************************************************")
      print(paste("Variância final: ", rALLAccuracyIt[[i]][[j]]$pHMM[[j]]$sigma2[j]))
      print("*****************************************************")
      print(paste("Probabilidade de estado final : ", rALLAccuracyIt[[i]][[j]]$pHMM[[j]]$delta[j]))
      print("*****************************************************")
      print(paste("Probabilidade transição final: ",rALLAccuracyIt[[i]][[j]]$pHMM[[j]]$gamma[j]))
      print("*************************************************")
      print(paste("Média de estimação da máxima verrosimilhança (Mv) por classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("****************************************************************************************")
      for (m in 1:10){
        print(paste("Base ", sizeTrain, " = Mv da Classe: ", m , " =  ",  rALLAccuracyIt[[i]][[j]]$mmllk2[[m]]))
        print("********************************************************")
      }
      print("****************************************************************************************")
      
      
      print("************Acurácia do HMM**************************************************")
      print("****************************************************************************************")
      print(paste("TREINO = Taxa de erro por classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("****************************************************************************************")
      for (m in 1:10){
        print(paste("Base ", sizeTrain, " = Erro da Classe: ", m , " =  ",  rALLAccuracyIt[[i]][[j]]$tgErrorHMMTestTotal[m]))
        print("********************************************************************")
      }
      print("***********************************************************************")
      print(paste("TREINO = Taxa de erro  da base : ", sizeTrain, " do ", i ,"º treino =", rALLAccuracyIt[[i]][[j]]$errorTotalBaseTrain))
      print("***********************************************************************")
      # print(paste("TESTE = Taxa de erro TESTE das classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("***********************************************************************")
      print(paste("TESTE = Taxa de erro da base : ", sizeTrain, " do ", i ,"º treino = ", rALLAccuracyIt[[i]][[j]]$errorTotalBaseTest))
      print("********************************************************************************************")
      print("")
      print("***********************************************************************")
      print(paste("Tempo de Processamento da base : ", sizeTrain, " =  ", (as.numeric(tempoBases[[i]][[j]])/360)*10, " minutos no ", i, "º treino"))
      print("********************************************************************************************")
      print("")
      
      #**********End results***********************************************************  
      
      
    } #********end for 2 **********************************************
    
    
  } #end
  
  print("**********************************************")
  print("Resultados Gerais do Algoritmo HMM")
  print("**********************************************")
  
  
  
  #***************Apresentação resultados ****************************
  print("Apresentação de Resultados")
  print("**********************************************************************************")
  print(paste("Tempo de Processamento para Carregamento das Bases", as.numeric(tempoBasesFinal/60), "Minutos"))
  print("**********************************************************************************")
  print("")
  
  for (j in 1:nrBase){
    sizeTrain = length(baseTrain<-trainParam[[j]])
    print(paste("Resultados da base de ",  sizeTrain))
    print("****************************************************************************")
    print("****************************************************************************")
    for (i in 1:Ii){
      #resultados das bases
      print(paste(i, "ª iteração"))
      print(paste("Tempo de Execução da Iteração:", as.numeric(tempoBases[[i]][[j]]/60), "minutos" ))
      print(paste("Taxa de Erro Train = ", rALLAccuracyIt[[i]][[j]]$errorTotalBaseTrain))
      print(paste("Taxa de Erro Teste = ", resultsBases[[j]][[i]]))
      print("**********************************************")
      #resultados das bases
      
    }
    
    print(paste("Menor taxa de Erro nas ", Ii , " Iterações =" ,menorResultPorBase[j]))
    print("**********************************************")
    print("**********************************************")
    print(paste("Media da Taxa de Erro da base em todas iterações = ", mediaPorBase[j]))
    print("****************************************************************************")
    print("****************************************************************************")
    print("")
    
  }
  
  s<-100
  b<-0
  for (j in 1:nrBase){
    if(as.numeric(mediaPorBase[j]) < s)
      s = mediaPorBase[j] 
    b = j
    #print(s)
  }
  
  print(paste(" Menor media entre as bases é  :" , s , " da base de", length(baseTrain<-trainParam[[b]])))
  print("****************************************************************************")
  print("****************************************************************************")
  
  
}#***** HMM - FIM - Apresentação dos resultados ***************************************



#Resultado individual ***************************************************************************

#***************** Apresentação Individual ***************************************

apresentacaoHMMInd<-function(int1,base1){
  
  int<-int1
  base<-base1
  #***************Obtendo resultados ****************************
  
  
  
  #*************Base de 100 x 1000 sem ruído ***************************************************
  for (i in int:int){
    
    for (j in base:base){
      #*******Resultados por treino *********************************************************************
      sizeTrain = length(baseTrain<-trainParam[[j]])
      print("")
      print(paste("************Resultados da base: ", sizeTrain, " do ", i ,"º treino ******************"))
      print("*****************************************************************")
      print("")
      print("************************Resultados dos Parâmetros do HMM ************************************")
      print("")
      print("************Parâmetros iniciais do HMM**************************************************")
      print("****************************************************************************************")
      print(paste("Média inicial: ", baseMuSigma2$mu))
      print("*****************************************************")
      print(paste("Variância inicial ", baseMuSigma2$sigma2))
      print("*****************************************************")
      print(paste("Probabilidade de estado inicial Gaussiana: ", inicialGammaDelta[[j]]$delta))
      print("*****************************************************")
      print(paste("Probabilidade transição inicial: ", inicialGammaDelta[[j]]$gamma))
      print("*************************************************")
      print("****************************************************************************************")
      print(paste("Média final: ", rALLAccuracyIt[[i]][[j]]$pHMM[[j]]$mu[j]))
      print("*****************************************************")
      print(paste("Variância final: ", rALLAccuracyIt[[i]][[j]]$pHMM[[j]]$sigma2[j]))
      print("*****************************************************")
      print(paste("Probabilidade de estado final : ", rALLAccuracyIt[[i]][[j]]$pHMM[[j]]$delta[j]))
      print("*****************************************************")
      print(paste("Probabilidade transição final: ",rALLAccuracyIt[[i]][[j]]$pHMM[[j]]$gamma[j]))
      print("*************************************************")
      print(paste("Média de estimação da máxima verrosimilhança (Mv) por classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("****************************************************************************************")
      for (m in 1:10){
        print(paste("Base ", sizeTrain, " = Mv da Classe: ", m , " =  ",  rALLAccuracyIt[[i]][[j]]$mmllk2[[m]]))
        print("********************************************************")
      }
      print("****************************************************************************************")
      
      
      print("************Acurácia do HMM**************************************************")
      print("****************************************************************************************")
      print(paste("TREINO = Taxa de erro por classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("****************************************************************************************")
      for (m in 1:10){
        print(paste("Base ", sizeTrain, " = Erro da Classe: ", m , " =  ",  rALLAccuracyIt[[i]][[j]]$tgErrorHMMTestTotal[m]))
        print("********************************************************************")
      }
      print("***********************************************************************")
      print(paste("TREINO = Taxa de erro  da base : ", sizeTrain, " do ", i ,"º treino =", rALLAccuracyIt[[i]][[j]]$errorTotalBaseTrain))
      print("***********************************************************************")
      # print(paste("TESTE = Taxa de erro TESTE das classes na base : ", sizeTrain, " do ", i ,"º treino"))
      print("***********************************************************************")
      print(paste("TESTE = Taxa de erro da base : ", sizeTrain, " do ", i ,"º treino = ", rALLAccuracyIt[[i]][[j]]$errorTotalBaseTest))
      print("********************************************************************************************")
      print("")
      print("***********************************************************************")
      print(paste("Tempo de Processamento da base : ", sizeTrain, " =  ", as.numeric(tempoBases[[i]][[j]])/60, " minutos no ", i, "º treino"))
      print("********************************************************************************************")
      print("")
      print(paste("Tempo de Processamento para Carregamento das Bases", as.numeric(tempoBasesFinal/60), "Minutos"))
      
      #**********End results***********************************************************  
      
      
    } #********end for 2 **********************************************
    
    
  } #end
  
  
  
}#***** HMM - FIM - Apresentação dos resultados ***************************************




#***************** Apresentação indidividual Fim *********************************


#********************** Apresentação Resumida ****************************************





#Resultado individual fim *************************************************************************






#**************** Treino HMMM ********************************************************
trainHMM<-function(classPar1,classLabel1){
  
  a <- classPar1$mllk
  b <- classPar1$mmllk
  c <- classPar1$AIC
  d <- classPar1$BIC
  e <- classPar1$mu
  f <- classPar1$sigma2
  g <- classPar1$gamma
  h <- classPar1$delta
  
  labels = as.numeric(classLabel1)
  mllk = as.numeric(a)
  mmllk = as.numeric(b)
  
  sizeBase = length(mllk)
  sizeModel =  length(mmllk)
  
  #print(paste("Tamanho modelo:",   sizeModel))
  #print(paste("Tamanho base:",     sizeBase ))
  
  av <- 0
  a<-0
  
  #***Aqui por enquanto retornando somente maxima verossimilhança
  #***Mas pode retornar AIC e BIC (fazer testes ainda)
  for (i in 1:sizeBase){
    for (j in 1:sizeModel){
      
     # if(mllk[[i]] > mmllk[[j]] && labels[i,1] == j)
      if(mllk[[i]] > mmllk[[j]] && labels[[i]] == j)
        a<-a+1
    }#forEND
    # print(paste("Teste", i, "Treino", j, "Valor",a))
    
  }#forEND
  
  av<-a/sizeBase
  #print(paste("Base", sizeTrain, " -> Acurácia: acertos: ", av))
  acertos<-av
  
  return(acertos)
  
}

#**************** Treino HMMM ********************************************************
testHMM<-function(classPar1,classLabel1,mmllk2){
  
  a <- classPar1$mllk
  b <- classPar1$mmllk
  c <- classPar1$AIC
  d <- classPar1$BIC
  e <- classPar1$mu
  f <- classPar1$sigma2
  g <- classPar1$gamma
  h <- classPar1$delta
  
  labels = as.numeric(classLabel1)
  mllk = as.numeric(a)
 # mmllk = as.numeric(b)
  mmllk = as.numeric(mmllk2)
  
  sizeBase = length(mllk)
  sizeModel =  length(mmllk)
  
 # print(paste("Tamanho modelo:",   str(sizeModel)))
#  print(paste("Tamanho base:",     str(sizeBase )))
  
  #print(paste("Amostras:",   mmllk))
  #print(paste("Modelo:",     mllk ))
  
  av <- 0
  a<-0
  
  #***Aqui por enquanto retornando somente maxima verossimilhança
  #***Mas pode retornar AIC e BIC (fazer testes ainda)
  for (i in 1:sizeBase){
    for (j in 1:sizeModel){
      
      # if(mllk[[i]] > mmllk[[j]] && labels[i,1] == j)
      if(mllk[[i]] > mmllk[[j]] && labels[[i]] == j)
        a<-a+1
    }#forEND
    # print(paste("Teste", i, "Treino", j, "Valor",a))
    
  }#forEND
  
  av<-a/sizeBase
  #print(paste("Base", sizeTrain, " -> Acurácia: acertos: ", av))
  acertos<-av
  
  return(acertos)
}

#**************** Treino HMMM ********************************************************
#testHMMG<-function(mllk1,mmllk1,label){
testHMMG<-function(mvTest,mvClass,rotulosTeste,class){
    
    labels = rotulosTeste
    mvTeste =  mvTest 
    mvClasse = as.numeric(mvClass)
    classe = class
    
    sizeBase = length(mvTeste)
  
    av <- 0
    a<-0
    
    #***Aqui por enquanto retornando somente maxima verossimilhança
    #***Mas pode retornar AIC e BIC (fazer testes ainda)
    for (i in 1:sizeBase){
     
      mvAmostra = as.numeric(mvTeste[[i]])
     
        if(mvAmostra > mvClasse && labels[i,1] == classe)
          a<-a+1
    
    }#forEND
    
    av<-a/sizeBase
    acertos<-av
    return(acertos)
    
}

avaliacaoBaseHMM<-function(order,labelsBaseTrain,paramBaseTrain,labelsBaseTest,paramBaseTest){
  #Tratar base individualmente *****************************************************
  #Variáveis globais
  orderBase<-order
  sizeTrainT<-10
 
  #****************************************** #Bases*********************************************************************
  
  #Preparação da avaliação (Treinamento e teste)
  #train
  baseTrain<-paramBaseTrain
  baseLabelTrain<-labelsBaseTrain
  sizeTrain<-length(baseTrain)
  
  cEstimations <- list()
  cErrorHMMTrain<-list()
  cErrorHMMTrainTotal <- list()
  #amostrasTrain<-list()
 
  #Test
  baseTest<-paramBaseTest
  baseLabelTest<-labelsBaseTest
  sizeTest<-length(baseTest)
  
  tEstimations <- list()
  cErrorHMMTest <- list()
  cErrorHMMTestTotal <- list()
  #amostrasTest<-list()
  
  
  tgEstimations <- list()
  tgErrorHMMTestTotal <- list()
  
  pHMM <- list()
  mmllk2 <-list()
  
  mllkTrain<-list()
  mllkTest<-list()
  
  print("***********************************************************************************************") 
  print("Processando base de teste...Aguarde.*******")
  
  tgEstimations<-runEstimations(baseLabelTest,baseTest,sizeTest,orderBase)
   mllkTest<-tgEstimations$mllk
   
   print("Finaliziando o processando base de Teste ***")
   print("********************************************************************************")
   
 
#Tratar classes individualmente em cada base *****************************************************
  for (i in 1:sizeTrainT){
   c = classBase(baseLabelTrain,baseTrain,i,sizeTrain) 
   cParTrain = c$classB
   cLabelTrain = c$labelB
   cSizeTrain = length(cParTrain)
  # aTrain<-cSizeTrain
   #amostrasTrain[i]<-aTrain
   print(paste("Base->", sizeTrain, " ->  Processando Classe:", i, "****"))
   
 # print(paste("Base-> ", sizeTrain, " -> classe ", i, "nº amostras: ", cSizeTrain))
  #print("***********************************************************************************************") 
#  print(paste("Treinamento: calculando estimativas da classe", i, "da base de", sizeTrain))

  cEstimations <- runEstimations(cLabelTrain,cParTrain,cSizeTrain,orderBase)
  mmllk2[i]<-cEstimations$mmllk
  pHMM[i] <- list(cEstimations)
  #print(paste("pHMM", str(pHMM[i])))
  #print(paste("pHMM", str(cEstimations)))
  erro <- trainHMM(cEstimations,cLabelTrain)
  cErrorHMMTrain[i] <-erro

 # print(paste("Total de erros da classe ", i, " = ", cErrorANNTrain[i], "% da base ", sizeTrain, " com ", cSizeTrain, " amostras"))
 
 # print(paste("Base Treino-> ",sizeTrain, " -> Erros classe ", i, " = ", cErrorHMMTrain[i], "%"))
  print("***********************************************************************************************") 
  cErrorHMMTrainTotal[i] <- cErrorHMMTrain[i]
  
  #******************************   Testes Associativo **************************************************
  #c = classBase(baseLabelTest,baseTest,i,sizeTest) 
  #cParTest = c$classB
  #cLabelTest = c$labelB
  #cSizeTest = length(cParTest)
  #print(paste("Base Teste:-> ", sizeTrain, " -> classe ", i, "nº amostras: ", cSizeTest))
  #print("***********************************************************************************************") 
  #parTest = erro
  #aTest<-cSizeTest
  #amostrasTest[i]<-aTest
  #tEstimations<-runEstimations(cLabelTest,cParTest,cSizeTest,orderBase)
  #erroT<-testHMM(tEstimations,cLabelTest,mmllk2)
  #cErrorHMMTest[i]<-erroT
  #cErrorHMMTestTotal[i] <- cErrorHMMTest[i]
#  print("***********************************************************************************************") 
  #******************************  Fim  Testes Associativo **************************************************
  
  
  }
  
#Teste com a Base de teste
  for (i in 1:sizeTrainT){
    mvModel = as.numeric(mmllk2[i])
    tgErrorHMMTestTotal[i]<-testHMMG(mllkTest,mvModel,baseLabelTest,i)
    print(paste("Base Treino-> ",sizeTrain, " -> Erros classe ", i, " = ", tgErrorHMMTestTotal[i], "%"))
  }
  
 
  
  # ******************************* Train **************************************************
  print("***********************************************************************************************") 
  errorTotalBaseTrain <- mean(as.numeric(aux <- Filter(Negate(is.null),cErrorHMMTrainTotal)))
  print(paste("Total de erros de treinamento da base", sizeTrain, " = ", errorTotalBaseTrain, "%"))
  #print("***********************************************************************************************") 
  
  #******************************* Teste Associativo *************************** 
 # errorTotalBaseTest <- mean(as.numeric(aux <- Filter(Negate(is.null),cErrorHMMTestTotal)))
  #print("***********************************************************************************************") 
  #print(paste("Total de erros de teste associativo da base", sizeTrain, " = ", errorTotalBaseTest, "%"))
  #print("***********************************************************************************************") 
  
  
  #******************************* Teste Geral *************************** 
  errorTotalBaseTestG <- mean(as.numeric(aux <- Filter(Negate(is.null),tgErrorHMMTestTotal)))
  #print("***********************************************************************************************") 
  print(paste("Total de erros Geral de teste da base", sizeTrain, " = ", errorTotalBaseTestG, "%"))
  print("***********************************************************************************************") 
  
  
 
 # return(list(pHMM=pHMM,cErrorHMMTrain=cErrorHMMTrain,errorTotalBaseTrain=errorTotalBaseTrain,
  #            cErrorHMMTest=cErrorHMMTest, errorTotalBaseTest=errorTotalBaseTest, mmllk2=mmllk2,
   #           amostrasTrain=amostrasTrain,amostrasTest=amostrasTest,errorTotalBaseTestG))
 
  
  
   return(list(pHMM=pHMM,cErrorHMMTrain=cErrorHMMTrain,errorTotalBaseTrain=errorTotalBaseTrain,
              mmllk2=mmllk2,errorTotalBaseTestG=errorTotalBaseTestG,tgEstimations=tgEstimations,
              tgErrorHMMTestTotal=tgErrorHMMTestTotal))
  
  
  
  
  
}#FIm função
#***********Fim base individual *************************************************************

#**************** HMM FIM funções *****************************************************




#Início da avaliação*********************************************************************
print("**************************************************************************************************")
print("Início da classificação de faltas do tipo curto-circuíto em Linhas de transmissão de energia")
print("**************************************************************************************************")
print("**************************************************************************************************")
print("Preparando o sistema HMM para classificação de faltas em linhas de transmissão na base de teste sem ruído")
print("Aguarde......")
print("**************************************************************************************************")

#Global
resultsBases<-list()
melhorItBase<-list()
menorResultPorBase<-list()
mediaPorBase<-list()

melhorItBases<-list()
melhorPorBases<-list()
mediasBases<-list()
menorMediaDasBases<-100


#Tempos
tempoBases<-list()

menorBase = 100
menorBases = 100

iter<-5
nrBase<-10
orderBase<-2
Ii<-iter

#Parametros RNN

ep<-2000
ta<-0.3
mo<-1

#ParametersResult
#HMM extractor
rParHMM<-list()
#ANN
rParANN<-list()
#Accuracy Train/Test
rALLAccuracy<-list()
rALLAccuracyIt<-list()
rALLAccuracyItANN<-list()

#Avaliação com base de 1000 sem ruído *****************************************************
inicialGammaDelta<-list()
inicialMuSigma2<-list()
baseTest<-test1000param 
baseLabelTest<-testLabels1000 
sizeTest<-length(baseTest)


#*************Base de 100 x 1000 sem ruído ***************************************************
for (i in 1:Ii){
  
  aux<-list()
  auxT<-list()
  
  for (j in 1:nrBase){
    print("**************************************************************************************************")
    
    inicioTempo = proc.time()
    baseTrain<-trainParam[[j]]
    baseTrainLabel<-trainLabels[[j]]
    sizeTrain<-length(baseTrain)
    print(paste("Base de", sizeTrain, i,"º treino"))
    baseGammaDelta<-trans.ergotica(orderBase)
    baseMuSigma2<-mu.sigma2.iniciais(baseTrain,sizeTrain,orderBase)
    
    inicialGammaDelta[j]<-list(baseGammaDelta)
    inicialMuSigma2[j]<-list(baseMuSigma2)
    #aux[j] = rALLAccuracy[j]<-list(avaliacaoBaseRNA(orderBase,baseTrainLabel,baseTrain,baseLabelTest,baseTest,ep*i,ta,mo))
    aux[j] = rALLAccuracy[j]<-list(avaliacaoBaseHMM(orderBase,baseTrainLabel,baseTrain,baseLabelTest,baseTest))
    auxT[j] = proc.time() - inicioTempo
    z = aux
    t = auxT
    #rALLAccuracyItANN[i]<-list(z)
    rALLAccuracyIt[i]<-list(z)
    tempoBases[i]<-list(t)
  } #********end for 2 **********************************************
  
  
} #end


#apresentacaoRNA_HMM()
apresentacaoHMM()
