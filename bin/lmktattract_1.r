#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.2.3
#criado:     28.06.2017
#modificado: 09.12.2019

#1. FUNCTIONS
source("misc_v2.3.r")

#2. READ DATA
f1 <- "../data/lmktattract.csv"
x_all <- read.table(file=f1,header=TRUE,sep=",",dec=".",row.names=1)
dim(x_all)

# 3. ANALYSE DATA
set.seed(12345)

main1 <- "Labour market attractiveness"  #title for plots
col1 <- c(rbind(hsv(h=seq(0,1,len=8),s=1/3,v=5/5)[1:7],  #very light
                hsv(h=seq(0,1,len=8),s=2/3,v=5/5)[1:7],  #light
                hsv(h=seq(0,1,len=8),s=3/3,v=5/5)[1:7],  #normal
                hsv(h=seq(0,1,len=8),s=3/3,v=4/5)[1:7])) #dark
names(col1) <- rownames(x_all)           #colours for EU28

#normalize and scale data
n_all <- c(1,  #pop_Total
           5,  #pop_Y[]
           1,  #ARPR
           1,  #low_work
           1,  #rooms_pp
           1,  #mat_depriv
           1,  #AROPE
           6,  #emp_Y[]_ED[]
           2,  #emp_T[]
           3,  #emp_Y[]
           20, #emp_Y[]_Nace[]
           2,  #unemp_Y[]
           1,  #GDP
           1,  #GVAgr
           1,  #disp_income
           1,  #training
           27, #earn_OC[]_Nace[]
           1)  #expend_ED5-8
w_all <- rep(1/n_all,times=n_all)
x_norm <- t(apply(scale(x_all),1,function(x){x*w_all}))

#calculate distances
#from help(dist): "Missing values are allowed, and are excluded from all 
#  computations involving the rows within which they occur. If some columns are
#  excluded in calculating a [...] distance, the sum is scaled up proportionally
#  to the number of columns used."
x_dist <- dist(x_norm,method="euclidean")

#perform Social Network Analysis
f1 <- "../results/lmktattract_1/net"
plot_sna_v2(x_dist,col1=col1,edge_thr=0.65,f1=f1,main=main1)
analyse_net(x_dist,f1=f1)

#perform Partition Around Medoids analysis
f1 <- "../results/lmktattract_1/pam"
plot_pam_v2(x_dist,col1=col1,f1=f1,main=main1)
kbest <- 10
f1 <- paste("../results/lmktattract_1/pam_grps",kbest,sep="")
plot_pam_v2(x_dist,kbest=kbest,col1=col1,f1=f1,main=main1)
