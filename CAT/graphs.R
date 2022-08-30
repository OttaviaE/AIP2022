rm(list = ls())
set.seed(999)
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}

i_info <- function(b, a=1,c=0, theta = seq(-5,5,length.out=1000)){
  
  
  P <- NULL 
  Q <- NULL
  Ii <- NULL
  
  for(i in 1:1000){
    P[i] <- 1/(1+ exp (-a*(theta[i] - b)))
    Q[i]= 1-P[i]
    Ii[i] =(a*Q[i]*(P[i]-c)^2)/(P[i]*((1-c)^2)) # (3PL)
  }
  return(Ii)
}


# Function to get all item information
item_info <- function(b,a=1){
  item <- NULL
  for(i in 1:length(b)){
    item[[i]] <- i_info(b[i],a[i])
  }
  return(item)
}

# definizione dei parametri ---- 
item = data.frame(item = paste("item", 1:10), 
                  b = c(runif(10, -3,3)),
                  a = c(runif(10, .20, 2)))

true_theta = seq(-4, 4, length.ou = 1000)
true_theta = true_theta[order(true_theta)]

# stratgeia normale per ottenere forme brevi

p_all = data.frame(matrix(nrow = nrow(item),
                          ncol = length(true_theta)))
q_all = data.frame(matrix(nrow = nrow(item),
                          ncol = length(true_theta)))
ii_all = data.frame(matrix(nrow = nrow(item),
                           ncol = length(true_theta)))
colnames(ii_all) = true_theta
true_theta = true_theta[order(true_theta)]
for (i in 1:nrow(item)) {
  temp = item[i, ]
  for (j in 1:length(true_theta)) {
    p_all[i, j] =  exp(temp$a *(true_theta[j] - temp$b))/(1 + exp(temp$a *(true_theta[j] - temp$b)))
    q_all[i, j] = 1-(exp(temp$a *(true_theta[j] - temp$b))/(1 + exp(temp$a *(true_theta[j] - temp$b))))
    
    ii_all[i,j] = temp$a^2 * p_all[i,j] *q_all[i,j]
  }
}

info_total = mean(colSums(ii_all))
item$IIF = rowMeans(ii_all)
for(i in 2:ncol(item)) {
  item[, i] = round(item[,i], 3)
}

rownames(item) = item$item
small = item[order(item$IIF, decreasing = T), ]
b <- small[1:5, "b"]
a <- small[1:5, "a"]

c <- item_info(b,a)


#Theta <-matrix(seq(-4,4, length.out=1000))
check <- data.frame(true_theta,
                    item_info = c[[1]],
                    item_info2 = c[[2]],
                    item_info3 = c[[3]], 
                    item_info4 = c[[4]], 
                    item_info5 = c[[5]])
check$sum = rowSums(check[,-1])



d1 <- do.call('cbind',c)
sum_info2 <- rowSums(d1)
plot(check$true_theta, check$sum , ylim= c(0, 1), cex.lab= 2,
     cex.axis =1.5,
     xlab = expression(theta), ylab = bquote(.("I", expression(theta))),
     type = "l", lwd =2,
     col = "royalblue")

# stessa cosa ma con item selezionati con la "nuova strategia!
theta_target = 3
theta_target = stats::kmeans(matrix(true_theta, ncol = 1),
                   centers = theta_target)
theta_target = (theta_target$centers)
# theta_target = seq(min(true_theta),max(true_theta),
#                    length.out = theta_target)
# tt = cut_borders(cut(theta_target, 5))
# tt$theta_target = rowMeans(tt)
# compute information for each item for each theta target (cluster and guided strategies)
p = data.frame(matrix(nrow = nrow(item),
                      ncol = length(theta_target)))
q = data.frame(matrix(nrow = nrow(item),
                      ncol = length(theta_target)))
ii = data.frame(matrix(nrow = nrow(item),
                       ncol = length(theta_target)))

colnames(ii) = theta_target[order(theta_target)]
rownames(ii) = paste0("item", 1:nrow(item))
temp = NULL
for (i in 1:nrow(item)) {
  temp = item[i, ]
  for (j in 1:length(theta_target)) {
    p[i, j] =  exp(temp$a *(theta_target[j] - temp$b))/(1 + exp(temp$a *(theta_target[j] - temp$b)))
    q[i, j] = 1-(exp(temp$a *(theta_target[j] - temp$b))/(1 + exp(temp$a *(theta_target[j] - temp$b))))
    
    ii[i,j] = temp$a^2 * p[i,j] * q[i,j]
  }
}
theta_target_k = theta_target[order(theta_target)]
ii$item =rownames(ii)
ii = ii[, c(ncol(ii), 1:(ncol(ii)-1))]
show_item = ii 
colnames(show_item)[2:4] = round(theta_target_k, 2)
for (i in 2:ncol(show_item)) {
  show_item[, i] = round(show_item[,i], 2)
}

ii_long = stats::reshape(ii,
                         idvar = "item",
                         direction = "long",
                         varying = list(2:ncol(ii)),
                         v.names = "info",
                         timevar = "theta_target",
                         times = names(ii)[-1])


max_data = stats::aggregate(info ~ item + theta_target,
                            data = ii_long, max)

max_data =  max_data[order(as.numeric(max_data$theta_target)), ]

temp = NULL
max_info = NULL

for(i in 1:length(unique(max_data$theta_target))) {
  temp1 = max_data[which(max_data$info == max(max_data$info)), ]
  max_data = max_data[which(max_data$item != temp1$item & max_data$theta_target != temp1$theta_target), ]
  max_info = rbind(max_info, temp1)
}
max_info$theta_target = as.numeric(max_info$theta_target)
for (i in 2:ncol(max_info)) {
  max_info[, i] = round(max_info[, i], 3)
}


#max_info = max_info[order(max_info$theta_target), ]


colnames(max_info)[2:3] = c("theta target", "IIF")
# prendere item
item$item = rownames(item)
max_info$item = gsub("item", "item ", max_info$item)
new = item[item$item %in% max_info$item, ]

b1 <- new[, "b"]
a1 <- new[, "a"]

c1 <- item_info(b1,a1)

check1 <- data.frame(true_theta,
                    item_info = c1[[1]],
                    item_info2 = c1[[2]],
                    item_info3 = c1[[3]], 
                    item_info4 = c1[[4]], 
                    item_info5 = c1[[5]])
check1$sum = rowSums(check1[,-1])

# nuova strategia
lines(check1$true_theta, check1$sum, lwd =2,
      col = "magenta", lty = 4)
