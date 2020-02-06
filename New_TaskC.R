library(readr) 
DiamondDataComplete <- read.csv(file.choose(), header=TRUE)
View(DiamondDataComplete)
attach(DiamondDataComplete)
summary(DiamondDataComplete)
head(DiamondDataComplete)
table(DiamondDataComplete$cut)


DiamondDataComplete$cut = factor(DiamondDataComplete$cut, levels = c('Fair','Good','Ideal', 'Premium','Very Good'), labels = c(0,1,2,3,4))
DiamondDataComplete$color = factor(DiamondDataComplete$color, levels = c('D','E','F','G','H','I','J'), labels = c(0,1,2,3,4,5,6))
DiamondDataComplete$clarity = factor(DiamondDataComplete$clarity, levels = c('I1','IF','SI1','SI2','VS1','VS2','VVS1','VVS2'), labels = c(0,1,2,3,4,5,6,7))



head(DiamondDataComplete)

#Data is Divided into Training and testing Data 
data_split <- sort(sample(nrow(DiamondDataComplete), nrow(DiamondDataComplete)*.80))



train_Data <- DiamondDataComplete[data_split, ]
test_data <- DiamondDataComplete[-data_split, ]

View(train_Data)
View(test_data)


#KNN to classify diamond cuts into appropriate types based on their features

table(DiamondDataComplete$cut)

#Now Creating seperate dataframe for 'cut' feauture which is the target

train_labels <- DiamondDataComplete[data_split, 2] 
test_labels <- DiamondDataComplete[-data_split, 2]


#install.packages("class")
library(class)
NROW(train_labels) 
NROW(test_labels)

table(train_labels)
train_labels
#  0     1     2     3     4 
#1228  3662 16004 10234  8872

table(test_labels)
test_labels
# 0    1    2    3    4 
#272  877 4007 2536 2308


#Knn model using K = 1
knn_1 <- knn(train = train_Data, test = test_data, cl = train_labels, k = 1)
summary(knn_1)

# 0    1    2    3    4 
#116  587 4365 3024 1908 

#Accuracy of knn_model k =1
ACC.1 <- 100 * sum(test_labels == knn_1)/NROW(test_labels) 
ACC.1     #65.96 - Accuracy of K = 1

#Now, buidlding model on K = 2
knn_2 <- knn(train = train_Data, test = test_data, cl = train_labels, k = 2)
summary(knn_2)

ACC.2 <- 100 * sum(test_labels == knn_2)/NROW(test_labels) 
ACC.2   #61.43 - Accuracy of K = 2


#For loop 

i = 1
k.optm = 1
for(i in 1:28) {
  knn.mod <- knn(train = train_Data, test = test_data, cl = train_labels, k = i)
  k.optm[i] <- 100 *sum(test_labels == knn.mod) / NROW(test_labels)
  k = i
  cat(k,'=',k.optm[i],'\n')
}


#By seeing the output of the above code, we can k = 1 gives the best accuracy. 
#1 = 65.96 
#2 = 61.22 
#3 = 61.96 
#4 = 60.94 
#5 = 60.38 
#6 = 59.59 
#7 = 59.04 
#8 = 58.3 
#9 = 57.33 
#10 = 56.63 
#11 = 56.53 
#12 = 56.26 
#13 = 55.75 
#14 = 55.63 
#15 = 54.82 
#16 = 54.75 
#17 = 54.64 
#18 = 54.05 
#19 = 53.8 
#20 = 53.66 
#21 = 53.39 
#22 = 53.17 
#23 = 53.04 
#24 = 52.63 
#25 = 52.39 
#26 = 52.17 
#27 = 51.92 
#28 = 51.52



#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}



s <- sample(nrow(DiamondDataComplete), size = 10000, replace = FALSE, prob = NULL)
diamonds.subset <- DiamondDataComplete[s, ]
View(diamonds.subset)



#install.packages("C50")
library(C50)



#diamond_data <- c("cut", "color", "clarity")
#newdata <- diamonds.subset[diamond_data]
#View(newdata)
#c50_model <- C5.0(newdata, newdata$cut) 
#c50_model

#diamonds.subset$carat <- factor(diamonds.subset$carat)
#diamonds.subset$depth <- factor(diamonds.subset$depth)
#diamonds.subset$x <- factor(diamonds.subset$x)
#diamonds.subset$y <- factor(diamonds.subset$y)
#diamonds.subset$z <- factor(diamonds.subset$z)
#diamonds.subset$table <- factor(diamonds.subset$table)
#diamonds.subset$price <- factor(diamonds.subset$price)
#str(diamonds.subset)

str(diamonds.subset)


myTree <- C5.0(diamonds.subset$cut ~., data = diamonds.subset)
summary(myTree)

plot(myTree)

Call:
  C5.0.formula(formula = diamonds.subset$cut ~ ., data
               = diamonds.subset)


#C5.0 [Release 2.07 GPL Edition]  	Mon Dec 09 13:15:05 2019
-------------------------------
  
#  Class specified by attribute `outcome'

#Read 10000 cases (10 attributes) from undefined.data

#Decision tree:

#depth > 63:
#:...depth > 64.3: 0 (269/42)
#:   depth <= 64.3:
#:   :...depth > 63.5: 1 (452/80)
#:       depth <= 63.5:
#:       :...price <= 665:
#:           :...y > 4.28: 1 (67/9)
#:           :   y <= 4.28:
#:           :   :...table <= 58.5: 4 (25/5)
#:           :       table > 58.5: 1 (5/1)
#:           price > 665:
#:           :...y <= 4.29: 4 (27)
#:               y > 4.29:
#:               :...y <= 4.72:
#:                   :...x <= 4.31: 1 (4)
#:                   :   x > 4.31:
#:                   :   :...x > 4.69: 4 (10)
#:                   :       x <= 4.69:
#:                   :       :...y <= 4.69: 4 (33/4)
#:                   :           y > 4.69: 1 (5/1)
#:                   y > 4.72:
#:                   :...price <= 940: 1 (12/3)
#:                       price > 940:
#:                       :...table <= 58.5: 4 (296/66)
#:                           table > 58.5:
#:                           :...carat <= 0.85: 4 (36/6)
#:                               carat > 0.85:
#:                               :...x <= 6.31:
#:                                   :...z > 3.95: 1 (19)
#:                                   :   z <= 3.95:
#:                                   :   :...x <= 6.04: 1 (4)
#:                                   :       x > 6.04: 4 (14/3)
#:                                   x > 6.31:
#:                                   :...y <= 6.37: 4 (17)
#:                                       y > 6.37:
#:                                       :...color in {0,6}: 1 (9/2)
#:                                           color in {1,2,4,5}: 4 (19/4)
#:                                           color = 3:
#:                                           :...clarity in {0,1,2,3,4,6,
#:                                               :           7}: 4 (7/1)
#:                                               clarity = 5: 1 (4)
#depth <= 63:
#:...table <= 57:
#    :...depth > 62.7:
#    :   :...price <= 774: 4 (57/13)
#    :   :   price > 774:
#    :   :   :...depth > 62.9:
#    :   :       :...color = 0: 2 (8/2)
#    :   :       :   color in {1,6}: 4 (16/5)
#    :   :       :   color = 2:
#    :   :       :   :...y <= 5.47: 3 (5)
#    :   :       :   :   y > 5.47: 4 (5/2)
#    :   :       :   color = 3:
#    :   :       :   :...table <= 56.5: 4 (8/4)
#    :   :       :   :   table > 56.5: 2 (4/1)
#    :   :       :   color = 5:
#    :   :       :   :...table <= 55.5: 2 (5/1)
#    :   :       :   :   table > 55.5: 4 (5/2)
#    :   :       :   color = 4:
#    :   :       :   :...table <= 54.5: 3 (2/1)
#    :   :       :       table > 54.5:
#    :   :       :       :...price <= 3231: 4 (6/1)
#    :   :       :           price > 3231: 2 (3)
#    :   :       depth <= 62.9:
#    :   :       :...table <= 53.2: 3 (5/2)
#    :   :           table > 53.2:
#    :   :           :...color in {0,1}: 4 (50/20)
#    :   :               color in {2,4,6}: 2 (72/31)
#    :   :               color = 3:
#    :   :               :...table <= 54.5: 4 (3)
#    :   :               :   table > 54.5: 2 (34/19)
#    :   :               color = 5:
#    :   :               :...depth <= 62.8: 2 (4)
#    :   :                   depth > 62.8:
#    :   :                   :...carat <= 0.86: 2 (2)
#    :   :                       carat > 0.86: 4 (3)
#    :   depth <= 62.7:
#    :   :...depth > 60: 2 (4298/728)
#    :       depth <= 60:
#    :       :...depth <= 58.6:
#    :           :...table > 56.5: 3 (5/2)
#    :           :   table <= 56.5:
#    :           :   :...table <= 55.5: 1 (3/1)
#    :           :       table > 55.5: 0 (4/1)
#    :           depth > 58.6:
#    :           :...z <= 2.98:
#    :               :...depth <= 58.9: 3 (2/1)
#    :               :   depth > 58.9:
#    :               :   :...carat <= 0.29: 4 (4)
#    :               :       carat > 0.29: 2 (39/8)
#    :               z > 2.98:
#    :               :...clarity = 0: 2 (1)
#    :                   clarity in {1,6,7}: 4 (9/3)
#   :                   clarity = 2:
#    :                   :...color in {1,5}: 4 (6/1)
#    :                   :   color in {2,4}: 2 (9/3)
    :                   :   color = 6: 3 (2/1)
    :                   :   color = 0:
    :                   :   :...depth <= 59.9: 3 (5/1)
    :                   :   :   depth > 59.9: 2 (2)
    :                   :   color = 3:
    :                   :   :...table <= 56.5: 3 (2)
    :                   :       table > 56.5: 4 (3/1)
    :                   clarity = 4:
    :                   :...y > 6.9: 2 (6/3)
    :                   :   y <= 6.9:
    :                   :   :...z <= 3.53: 4 (5/1)
    :                   :       z > 3.53: 3 (3)
    :                   clarity = 3:
    :                   :...x > 6.66: 2 (11/1)
    :                   :   x <= 6.66:
    :                   :   :...y > 6.55: 4 (5)
    :                   :       y <= 6.55:
    :                   :       :...x <= 5.95: 2 (9/3)
    :                   :           x > 5.95: 3 (10/4)
    :                   clarity = 5:
    :                   :...carat <= 0.56:
    :                       :...table <= 56.5: 2 (2)
    :                       :   table > 56.5:
    :                       :   :...color in {1,3,4,5,6}: 2 (4/1)
    :                       :       color in {0,2}:
    :                       :       :...y <= 5.23: 3 (2)
    :                       :           y > 5.23: 4 (2)
    :                       carat > 0.56:
    :                       :...table <= 56.5: 3 (9/4)
    :                           table > 56.5:
    :                           :...z <= 3.45: 1 (2)
    :                               z > 3.45:
    :                               :...x <= 6.79: 4 (6/1)
    :                                   x > 6.79: 2 (2)
    table > 57:
    :...depth <= 57.9:
        :...table > 63.5:
        :   :...table > 65: 0 (15)
        :   :   table <= 65:
        :   :   :...depth <= 56.6: 0 (8)
        :   :       depth > 56.6: 1 (4)
        :   table <= 63.5:
        :   :...depth > 57.4: 1 (38/1)
        :       depth <= 57.4:
        :       :...depth <= 56.1: 0 (5)
        :           depth > 56.1:
        :           :...price <= 3516: 1 (21/4)
        :               price > 3516:
        :               :...carat <= 1.01: 0 (6)
        :                   carat > 1.01: 1 (7/2)
        depth > 57.9:
        :...table > 60:
            :...table > 62:
            :   :...table <= 63:
            :   :   :...table <= 62.6: 1 (3)
            :   :   :   table > 62.6: 4 (81/16)
            :   :   table > 63:
            :   :   :...table <= 65: 1 (49/3)
            :   :       table > 65: 0 (16/2)
            :   table <= 62:
            :   :...price <= 742:
            :       :...carat > 0.36: 4 (16/3)
            :       :   carat <= 0.36:
            :       :   :...clarity in {0,1,4,6,7}: 4 (31/9)
            :       :       clarity = 3: 3 (5/2)
            :       :       clarity = 2:
            :       :       :...price <= 515: 4 (6)
            :       :       :   price > 515: 3 (11/3)
            :       :       clarity = 5:
            :       :       :...price <= 596: 4 (9)
            :       :           price > 596: 3 (7/2)
            :       price > 742:
            :       :...carat > 1.24: 3 (104/37)
            :           carat <= 1.24:
            :           :...clarity in {0,3,7}: 3 (108/42)
            :               clarity = 1: 4 (9/4)
            :               clarity = 4:
            :               :...color = 0: 4 (1)
            :               :   color in {1,2}: 1 (25/14)
            :               :   color in {3,4,5,6}: 3 (26/11)
            :               clarity = 6:
            :               :...depth <= 59.6: 1 (3)
            :               :   depth > 59.6: 3 (22/9)
            :               clarity = 2:
            :               :...table > 61.5:
            :               :   :...carat <= 1.01: 3 (27/3)
            :               :   :   carat > 1.01: 4 (7/2)
            :               :   table <= 61.5:
            :               :   :...color in {0,6}: 3 (10/2)
            :               :       color in {2,5}: 4 (18/10)
            :               :       color = 4: 1 (4/2)
            :               :       color = 1:
            :               :       :...y <= 5.28: 3 (5)
            :               :       :   y > 5.28: 4 (9/3)
            :               :       color = 3:
            :               :       :...depth > 61.5: 4 (5/1)
            :               :           depth <= 61.5:
            :               :           :...x <= 6.48: 1 (4/1)
            :               :               x > 6.48: 3 (3/1)
            :               clarity = 5:
            :               :...carat <= 0.38: 3 (8)
            :                   carat > 0.38:
            :                   :...table <= 60.8: 1 (2)
            :                       table > 60.8:
            :                       :...color in {0,2,4,5,6}: 3 (45/18)
            :                           color = 1:
            :                           :...price <= 1669: 4 (3)
            :                           :   price > 1669: 1 (13/6)
            :                           color = 3:
            :                           :...carat <= 1: 4 (11/5)
            :                               carat > 1: 1 (6/3)
            table <= 60:
            :...x <= 4.26:
                :...table <= 58.5: 4 (50/13)
                :   table > 58.5:
                :   :...color in {0,3,5,6}: 4 (19/4)
                :       color = 4: 3 (7/2)
                :       color = 2:
                :       :...depth <= 61.1: 4 (5)
                :       :   depth > 61.1: 3 (6/2)
                :       color = 1:
                :       :...price <= 575: 4 (18/2)
                :           price > 575:
                :           :...x <= 4.23: 3 (6)
                :               x > 4.23: 4 (2)
                x > 4.26:
                :...depth > 62.5:
                    :...clarity = 0: 0 (2/1)
                    :   clarity in {2,4}: 3 (186/65)
                    :   clarity = 1:
                    :   :...depth <= 62.7: 3 (2)
                    :   :   depth > 62.7: 4 (4)
                    :   clarity = 6:
                    :   :...x <= 4.65: 4 (4/1)
                    :   :   x > 4.65: 3 (10/2)
                    :   clarity = 7:
                    :   :...table <= 58.5: 4 (15/6)
                    :   :   table > 58.5:
                    :   :   :...y <= 7.27: 3 (18/5)
                    :   :       y > 7.27: 4 (3)
                    :   clarity = 3:
                    :   :...x > 7.48: 3 (10/1)
                    :   :   x <= 7.48:
                    :   :   :...color in {2,6}: 4 (25/10)
                    :   :       color = 5: 3 (4/1)
                    :   :       color = 0:
                    :   :       :...price <= 2792: 3 (6)
                    :   :       :   price > 2792: 4 (5)
                    :   :       color = 1:
                    :   :       :...depth <= 62.7: 3 (4/1)
                    :   :       :   depth > 62.7: 4 (6)
                    :   :       color = 3:
                    :   :       :...depth <= 62.6: 4 (3)
                    :   :       :   depth > 62.6: 3 (7/2)
                    :   :       color = 4:
                    :   :       :...x <= 6.7: 4 (5/1)
                    :   :           x > 6.7: 3 (4/1)
                    :   clarity = 5:
                    :   :...z <= 2.99: 3 (24/2)
                    :       z > 2.99:
                    :       :...color in {0,1}: 3 (28/11)
                    :           color in {3,6}: 4 (19/6)
                    :           color = 4:
                    :           :...carat <= 0.8: 4 (3)
                    :           :   carat > 0.8: 3 (6)
                    :           color = 5:
                    :           :...y <= 5.72: 3 (2)
                    :           :   y > 5.72: 4 (4)
                    :           color = 2:
                    :           :...carat <= 0.95: 4 (5/2)
                    :               carat > 0.95:
                    :               :...y <= 6.4: 3 (4)
                    :                   y > 6.4: 4 (6/2)
                    depth <= 62.5:
                    :...depth <= 59.5:
                        :...x <= 4.39:
                        :   :...price <= 731: 4 (6/3)
                        :   :   price > 731: 2 (3)
                        :   x > 4.39:
                        :   :...y <= 4.68: 3 (29/1)
                        :       y > 4.68:
                        :       :...depth <= 58.4: 3 (47/18)
                        :           depth > 58.4:
                        :           :...clarity in {0,3,4,7}: 3 (92/38)
                        :               clarity = 1:
                        :               :...carat <= 1.06: 3 (3/1)
                        :               :   carat > 1.06: 4 (2)
                        :               clarity = 2:
                        :               :...color in {0,1,2,3,4,
                        :               :   :         5}: 3 (53/21)
                        :               :   color = 6: 4 (4/1)
                        :               clarity = 6:
                        :               :...carat <= 1.12: 4 (6/1)
                        :               :   carat > 1.12: 3 (2)
                        :               clarity = 5:
                        :               :...depth <= 59.2:
                        :                   :...price <= 14581: 4 (25/7)
                        :                   :   price > 14581: 3 (3)
                        :                   depth > 59.2:
                        :                   :...depth <= 59.4: 3 (15/2)
                        :                       depth > 59.4: 4 (5/1)
                        depth > 59.5:
                        :...z <= 2.95:
                            :...carat > 0.3:
                            :   :...price > 681: 3 (315/45)
                            :   :   price <= 681:
                            :   :   :...carat <= 0.37: 3 (121/31)
                            :   :       carat > 0.37: 4 (10/3)
                            :   carat <= 0.3:
                            :   :...y <= 4.28: 3 (24)
                            :       y > 4.28:
                            :       :...table > 58.5:
                            :           :...clarity in {0,2,4,5,7}: 3 (24/3)
                            :           :   clarity in {1,3,6}:
                            :           :   :...depth > 61.1: 4 (2)
                            :           :       depth <= 61.1:
                            :           :       :...color in {0,1,2,4,
                            :           :           :         6}: 2 (4)
                            :           :           color in {3,5}: 3 (2)
                            :           table <= 58.5:
                            :           :...x > 4.35: 3 (5)
                            :               x <= 4.35:
                            :               :...y > 4.34: 2 (6/3)
                            :                   y <= 4.34:
                            :                   :...color in {0,1,3,
                            :                       :         6}: 2 (16/6)
                            :                       color in {2,4}: 3 (9/1)
                            :                       color = 5:
                            :                       :...depth <= 61.8: 3 (2)
                            :                           depth > 61.8: 2 (2)
                            z > 2.95:
                            :...x > 6.39:
                                :...y <= 6.44: 3 (96/7)
                                :   y > 6.44:
                                :   :...x > 6.44: 3 (837/277)
                                :       x <= 6.44:
                                :       :...z <= 3.93: 4 (9)
                                :           z > 3.93:
                                :           :...y <= 6.46: 3 (8/1)
                                :               y > 6.46:
                                :               :...y <= 6.5: 4 (13/4)
                                :                   y > 6.5: 3 (2)
                                x <= 6.39:
                                :...clarity = 0: 3 (7/1)
                                    clarity = 7: 4 (53/28)
                                    clarity = 1:
                                    :...color in {0,2}: 2 (3/1)
                                    :   color in {1,3,4,5,6}: 3 (11/5)
                                    clarity = 6:
                                    :...z <= 3.52: 3 (22/9)
                                    :   z > 3.52: 4 (6/2)
                                    clarity = 2:
                                    :...color in {0,2}: 3 (92/29)
                                    :   color = 6: 4 (7/3)
                                    :   color = 1:
                                    :   :...depth <= 62.2: 3 (36/13)
                                    :   :   depth > 62.2: 4 (5/2)
                                    :   color = 3:
                                    :   :...price <= 4283: 3 (21/5)
                                    :   :   price > 4283: 4 (3/1)
                                    :   color = 5:
                                    :   :...depth <= 60.2: 3 (2)
                                    :   :   depth > 60.2: 4 (10/2)
                                    :   color = 4:
                                    :   :...x <= 5.68: 4 (10/5)
                                    :       x > 5.68:
                                    :       :...price <= 4381: 3 (10/1)
                                    :           price > 4381: 2 (2)
                                    clarity = 4:
                                    :...color = 5: 3 (10/3)
                                    :   color = 6: 2 (3/2)
                                    :   color in {0,1,2}: 4 (65/26)
                                    :   color = 4:
                                    :   :...z <= 3.58: 3 (4/1)
                                    :   :   z > 3.58: 4 (6/1)
                                    :   color = 3:
                                    :   :...price <= 2774: 3 (25/8)
                                    :       price > 2774:
                                    :       :...depth <= 61.5: 2 (3)
                                    :           depth > 61.5: 3 (2)
                                    clarity in {3,5}:
                                    :...carat > 0.84:
                                        :...table > 59.4:
                                        :   :...carat <= 0.9: 4 (5/1)
                                        :   :   carat > 0.9: 3 (19/2)
                                        :   table <= 59.4:
                                        :   :...z <= 3.77:
                                        :       :...color in {0,1,2,3,4,
                                        :       :   :         6}: 3 (6)
                                        :       :   color = 5: 1 (2)
                                        :       z > 3.77:
                                        :       :...color = 3: 3 (14/7)
                                        :           color = 1:
                                        :           :...y <= 6.17: 3 (4/1)
                                        :           :   y > 6.17: 4 (8)
                                        :           color = 4:
                                        :           :...depth <= 62: 3 (6)
                                        :           :   depth > 62: 4 (5/1)
                                        :           color = 5:
                                        :           :...table <= 58.5: 4 (2)
                                        :           :   table > 58.5: 3 (3)
                                        :           color = 6:
                                        :           :...depth <= 61.7: 3 (2)
                                        :           :   depth > 61.7: 4 (2)
                                        :           color = 0: [S1]
                                        :           color = 2:
                                        :           :...x <= 6.31: 4 (7/2)
                                        :               x > 6.31:
                                        :               :...z <= 3.95: 3 (4)
                                        :                   z > 3.95: 4 (3)
                                        carat <= 0.84:
                                        :...x > 5.95: 3 (16/3)
                                            x <= 5.95:
                                            :...carat <= 0.48:
                                                :...price <= 1104: 3 (13/4)
                                                :   price > 1104: 4 (5/1)
                                                carat > 0.48:
                                                :...color = 0: 3 (31/12)
                                                    color = 1:
                                                    :...y <= 5.87: 3 (57/18)
                                                    :   y > 5.87: 4 (3)
                                                    color = 3: [S2]
                                                    color = 4:
                                                    :...z <= 3.52: 3 (9/3)
                                                    :   z > 3.52: 4 (4/1)
                                                    color = 5:
                                                    :...table <= 59.4: 3 (5/1)
                                                    :   table > 59.4: 4 (4/1)
                                                    color = 6:
                                                    :...price <= 1234: 2 (4/1)
                                                    :   price > 1234: 4 (7/2)
                                                    color = 2:
                                                    :...price <= 2594: 3 (34/6)
                                                        price > 2594: [S3]

SubTree [S1]

clarity = 5: 1 (1)
clarity = 3:
:...z <= 3.9: 3 (3)
    z > 3.9: 4 (2)

SubTree [S2]

clarity = 3: 3 (11/3)
clarity = 5: 4 (10/4)

SubTree [S3]

clarity = 3: 2 (3/1)
clarity = 5:
:...x <= 5.73: 2 (4/1)
    x > 5.73: 3 (6/1)


#Evaluation on training data (10000 cases):

#	    Decision Tree   
	  ----------------  
#	  Size      Errors  

#	   251 2164(21.6%)   <<


#	   (a)   (b)   (c)   (d)   (e)    <-classified as
	  ----  ----  ----  ----  ----
#	   279     9     3     8     3    (a): class 0
#	    39   635    15    80   135    (b): class 1
#	     3     7  3752   158    67    (c): class 2
#	     1    13   281  2118   141    (d): class 3
#	     3   104   517   577  1052    (e): class 4


#	Attribute usage:

#	100.00%	depth
#	 91.21%	table
#	 32.76%	x
#	 25.49%	z
#	 21.69%	price
#	 21.35%	y
#	 19.79%	clarity
#	 16.85%	carat
#	 13.96%	color


#Time: 0.1 secs

























