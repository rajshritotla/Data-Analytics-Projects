library('ROSE') #Sampling Package
setwd("") # Set working directory
data<- read.csv("creditcard.csv",header = TRUE) #read original file

# to work in weka change class variable to categorical
data$Class[which(data$Class==1)] <- "yes"
data$Class[which(data$Class==0)] <- "no"
table(data$Class) # check the class distibution

# perfor test-train split
smp_siz = floor(0.70*nrow(data)) # sample size creation
set.seed(3)   # set seed to ensure you always have same random numbers generated
# train data index
train_ind = sample(seq_len(nrow(data)),size = smp_siz, replace = FALSE)  # Randomly identifies the rows equal to sample size ( defined in previous instruction) from  all the rows of data dataset and stores the row number in train_ind
train = data[train_ind,] #creates the training dataset with row numbers stored in train_ind
table(train$Class) # check the class distibution
write.csv(train, "traincreditcard.csv") # save data

test=data[-train_ind,]  # creates test data
table(test$Class) # check the class distibution
write.csv(test, "testcreditcard.csv") # save data

# sampling performed on train data
data <- as.matrix(data)
train1 <- as.data.frame(train)
balanced_data <- ovun.sample(Class ~ ., data = train1, method = "both",N = nrow(train1)) # performed hybrid sampling
table(balanced_data$data$Class)
write.csv(balanced_data$data, "balancedtraincreditcard.csv") # save data