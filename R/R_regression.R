#================SIMPLE===========
library(matlib)
reloadS <- function(){SIMPLE()}
sim_or_mult=readline("[1] SIMPLE    [2] MULTIPLE      ")
if(sim_or_mult== '1'){
  SIMPLE <- function(){
    #=======DATA INPUT========================================================================
    choise1 = 'g'
    choise2 = 'g'
    choise3 = 'g'
    inp_1=readline("[1] Enter raw data     [2] Enter summarized data ")
    if(inp_1 == '1'){
      inp = readline("Enter X values seperated by spaces ' ' : ")
      inp2 = readline("Enter Y values seperated by spaces ' ' : ")
      x = as.vector(strsplit(inp, "\\s+")[[1]])
      y = as.vector(strsplit(inp2, "\\s+")[[1]])
      if (length(x)!= length(y)) {
        print("!!!! len X is not equal to len Y !!!!")
        reloadS()
        
        
        
      }
      df = data.frame(matrix( ncol = 2,nrow = length(x)))
      colnames(df) <- c('X','Y')
      43
      df['X'] <- x
      df['Y'] <- y
      df$X <- as.numeric(df$X)
      df$Y <- as.numeric(df$Y)
      
      print(df,row.names = FALSE)
      n = length(x)
      sigma_x = sum(df$X)
      sigma_y = sum(df$Y)
      x_bar = sum(df$X)/n
      y_bar = sum(df$Y)/n
      sigma_xy = sum(df$X * df$Y)
      sigma_x_sq = sum(df$X * df$X)
      sigma_y_sq = sum(df$Y * df$Y)
    }else {if(inp_1 == '2') {
      n = as.numeric(readline("n : "))
      sigma_x = as.numeric(readline("sigma_x : "))
      sigma_y = as.numeric(readline("sigma_y : "))
      x_bar = sigma_x/n
      y_bar = sigma_y/n
      sigma_xy = as.numeric(readline("sigma_xy : "))
      sigma_x_sq = as.numeric(readline("sigma_x_sq : "))
      sigma_y_sq = as.numeric(readline("sigma_y_sq : "))
      
    }
      else{print("wrong choice")
        invokeRestart("abort") 
      }}
    sp="  "
    ne="\n"
    ln="=========================================="
    cat(ne,ln,ne)
    #========SUMMARY STATS=========
    
    
    df_res1 = data.frame(matrix( ncol = 8,nrow = 1))
    colnames(df_res1) <- c('n','x_bar','y_bar','sigma_x','sigma_y','sigma_x_sq','sigma_y_sq','sigma_xy')
    
    df_res1['n'] <- n
    df_res1['x_bar'] <- x_bar
    df_res1['y_bar'] <- y_bar
    df_res1['sigma_x'] <- sigma_x
    df_res1['sigma_y'] <- sigma_y
    df_res1['sigma_x_sq'] <- sigma_x_sq
    df_res1['sigma_y_sq'] <- sigma_y_sq
    df_res1['sigma_xy'] <- sigma_xy
    print(df_res1,row.names = FALSE)
    cat(ne,ln,ne)
    #===========S**==============================================================
    
    Sxx = sigma_x_sq-(n*(x_bar^2))
    Syy = sigma_y_sq-(n*(y_bar^2))
    Sxy = sigma_xy-(n*x_bar*y_bar)
    df_res2 = data.frame(matrix( ncol = 3,nrow = 1))
    colnames(df_res2) <- c('Sxx','Syy','Sxy')
    
    df_res2['Sxx'] <- Sxx
    df_res2['Syy'] <- Syy
    df_res2['Sxy'] <- Sxy
    
    print(df_res2,row.names = FALSE)
    cat(ne,ln,ne)
    
    #===========B!=================================================================
    B1_hat = Sxy/Sxx
    B0_hat = y_bar-(x_bar*B1_hat)
    SSR = B1_hat*Sxy
    SSE = Syy-SSR
    MSE = SSE/(n-2)
    df_res3 = data.frame(matrix( ncol = 2,nrow = 1))
    colnames(df_res3) <- c('B1_hat','B0_hat')
    
    df_res3['B1_hat'] <- B1_hat
    df_res3['B0_hat'] <- B0_hat
    
    
    print(df_res3,row.names = FALSE)
    cat(ne,ln,ne)
    #===========LINE========================================================================
    cat(" Regression line :   Y' = ",B0_hat," + ", B1_hat ," X ")
    cat(ne,ln,ne)
    #=============ACCURACY====================================================================
    Rsquared = B1_hat*Sxy/Syy
    r =sqrt(Rsquared)
    df_res5 = data.frame(matrix( ncol = 2,nrow = 1))
    colnames(df_res5) <- c('R^2','r')
    
    df_res5['R^2'] <- Rsquared
    df_res5['r'] <- r
    print(df_res5,row.names = FALSE)
    cat("  ",Rsquared*100,"% of the variation of Y is expressed by our model ")
    cat(ne,ln,ne)
    #========INTERVAL CALCULATOR=================================================================================
    
    conf_b1 <- function() {
      significance_level =as.numeric(readline("enter significance level (Alpha):   "))
      t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
      upper_ <- (B1_hat+((t_c)*(sqrt((MSE/Sxx)))))
      lower_ <- (B1_hat-((t_c)*(sqrt((MSE/Sxx)))))
      cat('Range : [ ',lower_,', ',upper_,' ]')
      cat(ne)
    }
    conf_b0 <- function() {
      significance_level =as.numeric(readline("enter significance level (Alpha):   "))
      t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
      upper_ <- (B0_hat+((t_c)*(sqrt(MSE*(1/n+(x_bar^2/Sxx))))))
      lower_ <- (B0_hat-((t_c)*(sqrt(MSE*(1/n+(x_bar^2/Sxx))))))
      cat('Range : [ ',lower_,', ',upper_,' ]')
      cat(ne)
    }
    ypredict <- function() {
      choise3 = readline('mean response ?? : y/n ')
      
      
      if(choise3 == 'y'){
        xnode = as.numeric(readline('Enter X value : '))
        y_est = ((B1_hat*xnode)+B0_hat)
        significance_level =as.numeric(readline("enter significance level (Alpha):   "))
        t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
        
        se=sqrt((MSE*((1/n)+((xnode-x_bar)^2/Sxx))))
        upper_ <- (y_est+(t_c)*se)
        lower_ <- (y_est-(t_c)*se)
        cat('Range : [ ',lower_,', ',upper_,' ]')
        cat(ne)
      }else{if(choise3 == 'n'){
        xnode = as.numeric(readline('Enter X value : '))
        y_est = ((B1_hat*xnode)+B0_hat)
        
        significance_level =as.numeric(readline("enter significance level (Alpha):   "))
        t_c2 = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
        
        se2=sqrt((MSE*((1)+(1/n)+((xnode-x_bar)^2/Sxx))))
        upper_2 <- (y_est+(t_c2)*se2)
        lower_2 <- (y_est-(t_c2)*se2)
        cat('Range : [ ',lower_2,', ',upper_2,' ]')
        cat(ne)}}
    }
    
    #========HYPOTHESIS TEST CALCULATOR=======================================================================
    hyp_b1 <- function(){
      
      choise5 = 'm'
      while(TRUE){
        choise5 = readline("[1] Ha:Not Equal   [2] Ha: <    [3] >   [b] back [q] EXIT           ")
        if(choise5 == 1){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
          t_node = (B1_hat)/(sqrt((MSE/Sxx)))
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. B1 !=")}else{print("we accept H. B1 =")}
          hyp_t()
        }
        else{if(choise5 == 2 | 3){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c =qt(p= significance_level, df= n-2, lower.tail=FALSE)
          t_node = ((B1_hat-0)/(sqrt(MSE/Sxx)))
          op='>'
          if(choise5 == 2){op='<'}
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. ","B1",op)}else{print("we accept H.")}
          hyp_t()
        }
          
          else{if (choise5 == 'b'){hyp_t()}
            else {if(choise5 == 'q'){exit()}
            }
          }}
      }
    }
    
    
    
    hyp_b0 <- function(){
      
      choise6 = 'm'
      while(TRUE){
        choise6 = readline("[1] Ha:Not Equal   [2] Ha: <    [3] >   [b] back [q] EXIT           ")
        if(choise6 == 1){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
          t_node = (B0_hat-0)/(sqrt(MSE*(1/n+(x_bar^2/Sxx))))
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. B0 !=")}else{print("we accept H. B0 =")}
          hyp_t()
        }
        else{if(choise6 == 2 | 3){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c = qt(p= significance_level, df= n-2, lower.tail=FALSE)
          t_node = ((B0_hat-0)/(sqrt(MSE*(1/n+(x_bar^2/Sxx)))))
          op='>'
          if(choise6 == 2){op='<'}
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. ","B0",op)}else{print("we accept H.")}
          hyp_t()}
          else{if (choise6 == 'b'){hyp_t()}
            else {if(choise6 == 'q'){exit()}
            }
          }}
      }
    }
    
    
    
    
    hyp_rho <- function(){
      
      choise7 = 'm'
      while(choise7 != 5|6){
        choise7 = readline("[1] Ha:Not Equal   [2] Ha: <    [3] >   [b] back [q] EXIT           ")
        if(choise7 == 1){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
          t_node = r*(sqrt((n-2)/(1-r^2)))
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. rho !=")}
          else{print("we accept H.  rho =")}
          hyp_t()
        }
        else{
          if(choise7 == 2 | 3){
            significance_level =as.numeric(readline("enter significance level (Alpha):   "))
            cat(ne)
            t_c = qt(p= significance_level, df= n-2, lower.tail=FALSE)
            t_node = (r*(sqrt((n-2)/(1-r^2))))
            op='>'
            if(choise7 == 2){op='<'}
            if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. ","rho",op)}else{print("we accept H.")}
            hyp_t()}
          
          else{
            if (choise7 == 'b'){hyp_t()}
            else {
              if(choise7 == 'q'){exit()}}
          }
        }
      }
    }
    
    
    
    
    hyp_FB1 <- function(){
      significance_level =as.numeric(readline("enter significance level (Alpha):   "))
      f_c = qf(p= significance_level,df1=1, df2= n-2, lower.tail=FALSE)
      f_node = SSR/MSE
      if(f_node > f_c){cat(f_node,">",f_c,"   ","we reject H. ")}else{print("we accept H.")}
    }
    
    #=======ANOVA CACULATOR=======================================================================
    ANOVA <- function(){
      
      SSR = B1_hat*Sxy
      SSE = Syy-SSR
      MSE = SSE/(n-2)
      df_res4 = data.frame(matrix( ncol = 5,nrow = 3))
      colnames(df_res4) <- c('Source','SS','D.f','MSS','F-value')
      
      df_res4['Source'] <- c('Reg','Res','Total')
      df_res4['SS'] <- c(SSR,SSE,Syy)
      df_res4['D.f'] <- c(1,n-2,n-1)
      df_res4['MSS'] <- c(SSR,MSE,'#')
      df_res4['F-value'] <- c(SSR/MSE,'#','#')
      
      print(df_res4,row.names = FALSE)
      cat(ne,ln,ne)
      ceo()
    }
    #===========INTERVAL CALC.CONSOL===================================================================
    interval <- function() {
      
      choise2 = 'm'
      choise3 = 'm'
      while(choise2 != 'q'){
        choise2 = readline("[1] (B1)   [2] (B0)   [3] (Y' at X)  [b] go back  [q] EXIT    ")
        if(choise2 == 1){conf_b1()}
        else{if(choise2 == 2){conf_b0()}
          else{if(choise2 == 3){
            
            ypredict()}
            else{if (choise2 == 'b'){ceo()}
              else {if(choise2 == 'q'){print("THANKS...")   
                invokeRestart("abort")}}}  }
        }
      }
    }
    
    #===========HYPOTHESIS CALC.CONSOL===================================================================
    hyp_t <- function() {
      
      choise4 = 'm'
      while(TRUE){
        choise4 = readline("Hypothesis test for : [1] (B1)   [2] (B0)   [3] rho  [4] F-for B1     [b] go back  [q] EXIT    ")
        if(choise4 == 1){hyp_b1()}
        else{if(choise4 == 2){hyp_b0()}
          else{if(choise4 == 3){hyp_rho()}
            else{if (choise4 == 4){hyp_FB1()}
              else {if(choise4 == 'b'){ceo()}
                else{if(choise4 == 'q'){print("THANKS...")   
                  invokeRestart("abort")}}}}  }
        }
      }
    }
    #============MAIN MENU===========================================================
    
    ceo <- function() {
      
      swi = readline("[1] ANOVA table   [2] Calculate confidence interval    [3] Hyp-testing  [b] back   [q] EXIT           ")
      cat(ne)
      core = switch(  
        swi,  
        '1' = ANOVA(),
        '2' = interval(),
        '3' = hyp_t(),
        'b' = source(getSourceEditorContext()$path ),
        'q' = print("THANKS...")+invokeRestart("abort")
        
      )  
    }
    ceo()
    
    
    
    
    
    
    
    
    
  }
  SIMPLE()
}else
  #================MULTIPLE==============================
{reloadM <- function(){MULTIPLE()}




if(sim_or_mult=='2'){
  MULTIPLE <- function(){
    #=======DATA INPUT========================================================================
    choise1 = 'g'
    choise2 = 'g'
    choise3 = 'g'
    sp="  "
    ne="\n"
    ln="=========================================="
    
    k =as.numeric(readline(" Enter \" K \" number of independent (X)  :"))
    
    cat("Enter space separated data column by column. Enter empty column when finished.\n")
    nums <- scan(stdin())
    xz <- matrix(nums, ncol=k, byrow = FALSE)
    
    xz = cbind(rep(1,nrow(xz)),xz)
    cat(ne,"x  :",ne)
    print(xz)
    cat("Enter Y values seperated by spaces ' ' : ")
    nums <- scan(stdin())
    y <- matrix(nums, ncol=1, byrow = FALSE)
    cat(ne,ne,"y  :",ne)
    print(y)
    
    if (nrow(xz)!= length(y)) {
      print("!!!! len X is not equal to len Y !!!!")
      reloadM()
    }
    #========STATS=========
    
    xz_trans= t(xz)
    sigma_y = (xz_trans%*%y)[1,] 
    cat(ne,ne,"x\' : ",ne)
    print(xz_trans)
    zx_xz = xz_trans%*%xz
    cat(ne,ne,"x\'x : ",ne)
    print(zx_xz)
    cat(ne,ln,ne)
    fst =as.array(zx_xz[1,])
    for(i in 1:k) {
      
      
      cat("sigma_x",i,'|',sep = "")
      
    }
    cat(ne)
    cat(fst[2:length(fst)],fill=TRUE)
    cat(ne)
    
    cat("n|")
    for(i in 1:k) {
      
      
      cat("sigma_x",i,"^2",'|',sep = "")
      
    }
    cat(ne) 
    di=as.array(diag(zx_xz,names=FALSE))
    cat(di,fill=TRUE)
    n=di[1]
    cat(ne)
    for(i in 1:(k-1)) {
      
      cat("sigma_x",i+1,"_x",1,"|",sep = "")
      
      
    }
    scnd = as.array(zx_xz[2,])
    cat(ne) 
    
    cat(scnd[3:length(scnd)],fill=TRUE)
    cat(ne,ln,ne)
    
    
    #===========B!=================================================================
    B_hat = inv(zx_xz)%*%t(xz)%*%y
    for(i in 0:k+1) {
      
      
      cat("B",i-1,'|',sp,sep = "")
      
      
    }
    cat(ne)
    cat(as.array(B_hat),fill=TRUE)
    cat(ne,ln,ne)
    #===========LINE========================================================================
    cat(" Regression line :   Y' = ")
    for(i in 0:k+1) {
      
      
      cat("B",i-1,sep = "")
      if(i>1){
        
      cat("X",i-1,sep = "")}
      if(i<k+1)
      cat(" + ")
    }
    cat(ne)
    cat(" Regression line :   Y' = ")
    for(i in 0:k+1) {
      
      
      cat(B_hat[i,],sep = "")
      if(i>1){
        
        cat(" X",i-1,sep = "")}
      if(i<k+1){
        cat(" + ")}
    }
    cat(ne,ln,ne)
    #=============Fitted_Values====================================================================
    cat("Fitted Values  :  ",fill = TRUE)
    for(i in 1:n){cat(i,'|')}
    y_fitted = xz%*%B_hat
    cat(ne)
    cat(y_fitted,fill=TRUE)
    cat(ne,ln,ne)
    #=============RESIDUALS====================================================================
    cat("Residuals  :  ",fill = TRUE)
    for(i in 1:n){cat(i,'|')}
    e = y - y_fitted
    cat(ne)
    cat(e,fill=TRUE)
    cat(ne,ln,ne)
    #=============ACCURACY====================================================================
    SST = (t(y)%*%y)-(n*(sigma_y/n)^2)
    SSE = t(e)%*%e
    SSR = SST-SSE
    MSE = SSE/(n-(k+1))
    Rsquared = SSR/SST
    r =sqrt(Rsquared)
    df_res5 = data.frame(matrix( ncol = 2,nrow = 1))
    colnames(df_res5) <- c('R^2','r')
    
    df_res5['R^2'] <- Rsquared
    df_res5['r'] <- r
    print(df_res5,row.names = FALSE)
    cat("  ",Rsquared*100,"% of the variation of Y is expressed by our model ")
    cat(ne,ln,ne)
    #========INTERVAL CALCULATOR=================================================================================
    
    conf_b1 <- function() {
      significance_level =as.numeric(readline("enter significance level (Alpha):   "))
      t_c = qt(p= significance_level/2, df= n-k, lower.tail=FALSE)
      upper_ <- (B_hat[2,]+((t_c)*(sqrt((MSE*as.array(diag(inv(zx_xz)))[2])))))
      lower_ <- (B_hat[2,]-((t_c)*(sqrt((MSE*as.array(diag(inv(zx_xz)))[2])))))
      cat('Range : [ ',lower_,', ',upper_,' ]')
      cat(ne)
    }
    conf_b0 <- function() {
      significance_level =as.numeric(readline("enter significance level (Alpha):   "))
      t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
      upper_ <- (B_hat[1,]+((t_c)*(sqrt((MSE*as.array(diag(inv(zx_xz)))[1])))))
      lower_ <- (B_hat[1,]-((t_c)*(sqrt((MSE*as.array(diag(inv(zx_xz)))[1])))))
      cat('Range : [ ',lower_,', ',upper_,' ]')
      cat(ne)
    }
    ypredict <- function() {
      choise3 = readline('mean response ?? : y/n ')
      
      
      if(choise3 == 'y'){
        cat("Enter x-node values seperated by spaces ' ' : ")
        nums <- scan(stdin())
        xnode <- matrix(nums, ncol=1, byrow = FALSE)
        xnode = rbind(1,xnode)
        cat(ne,"x-node : ",ne)
        print(xnode)
        y_est_node = t(xnode)%*%B_hat
        significance_level =as.numeric(readline("enter significance level (Alpha):   "))
        t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
        
        se=sqrt(MSE*(t(xnode)%*%inv(xz_trans%*%xz)%*%xnode))
        upper_ <- (y_est_node+(t_c)*se)
        lower_ <- (y_est_node-(t_c)*se)
        cat('Range : [ ',lower_,', ',upper_,' ]')
        cat(ne)
      }else{if(choise3 == 'n'){
        cat("Enter x-node values seperated by spaces ' ' : ")
        nums <- scan(stdin())
        xnode <- matrix(nums, ncol=1, byrow = FALSE)
        xnode = rbind(1,xnode)
        cat(ne,"x-node : ",ne)
        print(xnode)
        y_est_node = t(xnode)%*%B_hat
        
        significance_level =as.numeric(readline("enter significance level (Alpha):   "))
        t_c2 = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
        
        se2=sqrt(MSE*(1+(t(xnode)%*%inv(xz_trans%*%xz)%*%xnode)))
        upper_2 <- (y_est_node+(t_c2)*se2)
        lower_2 <- (y_est_node-(t_c2)*se2)
        cat('Range : [ ',lower_2,', ',upper_2,' ]')
        cat(ne)}}
    }
    
    #========HYPOTHESIS TEST CALCULATOR=======================================================================
    hyp_b1 <- function(){
      
      choise5 = 'm'
      while(TRUE){
        choise5 = readline("[1] Ha:Not Equal   [2] Ha: <    [3] >   [b] back [q] EXIT           ")
        if(choise5 == 1){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
          t_node = (B_hat[2,])/((sqrt((MSE*as.array(diag(inv(zx_xz)))[2]))))
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. B1 !=")}else{print("we accept H. B1 =")}
          hyp_t()
        }
        else{if(choise5 == 2 | 3){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c =qt(p= significance_level, df= n-2, lower.tail=FALSE)
          t_node = ((B_hat[2,]-0)/(sqrt((MSE*as.array(diag(inv(zx_xz)))[2]))))
          op='>'
          if(choise5 == 2){op='<'}
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. ","B1",op)}else{print("we accept H.")}
          hyp_t()
        }
          
          else{if (choise5 == 'b'){hyp_t()}
            else {if(choise5 == 'q'){exit()}
            }
          }}
      }
    }
    
    
    
    hyp_b0 <- function(){
      
      choise6 = 'm'
      while(TRUE){
        choise6 = readline("[1] Ha:Not Equal   [2] Ha: <    [3] >   [b] back [q] EXIT           ")
        if(choise6 == 1){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
          t_node = (B_hat[1,]-0)/(sqrt((MSE*as.array(diag(inv(zx_xz)))[1])))
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. B0 !=")}else{print("we accept H. B0 =")}
          hyp_t()
        }
        else{if(choise6 == 2 | 3){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c = qt(p= significance_level, df= n-2, lower.tail=FALSE)
          t_node = (B_hat[1,]-0)/(sqrt((MSE*as.array(diag(inv(zx_xz)))[1])))
          op='>'
          if(choise6 == 2){op='<'}
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. ","B0",op)}else{print("we accept H.")}
          hyp_t()}
          else{if (choise6 == 'b'){hyp_t()}
            else {if(choise6 == 'q'){exit()}
            }
          }}
      }
    }
    
    
    
    
    hyp_rho <- function(){
      
      choise7 = 'm'
      while(choise7 != 5|6){
        choise7 = readline("[1] Ha:Not Equal   [2] Ha: <    [3] >   [b] back [q] EXIT           ")
        if(choise7 == 1){
          significance_level =as.numeric(readline("enter significance level (Alpha):   "))
          cat(ne)
          t_c = qt(p= significance_level/2, df= n-2, lower.tail=FALSE)
          t_node = r*(sqrt((n-(k+1))/(1-r^2)))
          if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. rho !=")}
          else{print("we accept H.  rho =")}
          hyp_t()
        }
        else{
          if(choise7 == 2 | 3){
            significance_level =as.numeric(readline("enter significance level (Alpha):   "))
            cat(ne)
            t_c = qt(p= significance_level, df= n-2, lower.tail=FALSE)
            t_node = (r*(sqrt((n-(k+1))/(1-r^2))))
            op='>'
            if(choise7 == 2){op='<'}
            if(t_node > t_c){cat(t_node,">",t_c,"   ","we reject H. ","rho",op)}else{print("we accept H.")}
            hyp_t()}
          
          else{
            if (choise7 == 'b'){hyp_t()}
            else {
              if(choise7 == 'q'){exit()}}
          }
        }
      }
    }
    
    
    
    
    hyp_FB1 <- function(){
      significance_level =as.numeric(readline("enter significance level (Alpha):   "))
      f_c = qf(p= significance_level,df1=k, df2= n-(k+1), lower.tail=FALSE)
      f_node = (SSR/((k+1)-1))/MSE
      if(f_node > f_c){cat(f_node,">",f_c,"   ","we reject H. at least one slop equal constant nott zero")}else{print("we accept H. all slops equal 0")}
    }
    
    #=======ANOVA CACULATOR=======================================================================
    ANOVA <- function(){
      
      
      df_res4 = data.frame(matrix( ncol = 5,nrow = 3))
      colnames(df_res4) <- c('Source','SS','D.f','MSS','F-value')
      
      df_res4['Source'] <- c('Reg','Res','Total')
      df_res4['SS'] <- c(SSR,SSE,SST)
      df_res4['D.f'] <- c((k+1)-1,n-(k+1),n-1)
      df_res4['MSS'] <- c(SSR/(k+1)-1,MSE,'#')
      df_res4['F-value'] <- c(((SSR/((k+1)-1))/MSE),'#','#')
      
      print(df_res4,row.names = FALSE)
      cat(ne,ln,ne)
      ceo()
    }
    #===========INTERVAL CALC.CONSOL===================================================================
    interval <- function() {
      
      choise2 = 'm'
      choise3 = 'm'
      while(choise2 != 'q'){
        choise2 = readline("[1] (B1)   [2] (B0)   [3] (Y' at X)  [b] go back  [q] EXIT    ")
        if(choise2 == 1){conf_b1()}
        else{if(choise2 == 2){conf_b0()}
          else{if(choise2 == 3){
            
            ypredict()}
            else{if (choise2 == 'b'){ceo()}
              else {if(choise2 == 'q'){print("THANKS...")   
                invokeRestart("abort")}}}  }
        }
      }
    }
    
    #===========HYPOTHESIS CALC.CONSOL===================================================================
    hyp_t <- function() {
      
      choise4 = 'm'
      while(TRUE){
        choise4 = readline("Hypothesis test for : [1] (B1)   [2] (B0)   [3] rho  [4] F-for B1     [b] go back  [q] EXIT    ")
        if(choise4 == 1){hyp_b1()}
        else{if(choise4 == 2){hyp_b0()}
          else{if(choise4 == 3){hyp_rho()}
            else{if (choise4 == 4){hyp_FB1()}
              else {if(choise4 == 'b'){ceo()}
                else{if(choise4 == 'q'){print("THANKS...")   
                  invokeRestart("abort")}}}}  }
        }
      }
    }
    #============MAIN MENU===========================================================
    
    ceo <- function() {
      
      swi = readline("[1] ANOVA table   [2] Calculate confidence interval    [3] Hyp-testing  [q] EXIT           ")
      cat(ne)
      core = switch(  
        swi,  
        '1' = ANOVA(),
        '2' = interval(),
        '3' = hyp_t(),
        'b' = source(getSourceEditorContext()$path ),
        'q' = print("THANKS...")+invokeRestart("abort")
        
      )  
    }
    ceo()
    
  }
  MULTIPLE()
  
}}

