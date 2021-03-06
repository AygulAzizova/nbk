setwd("C://Users/DNS/Desktop/StatAd")
getwd()
# ��������� ���� � ������� � ���������� winedata
winedata <- read.csv("datawine.txt", sep=",")

# ������ ����� ������� � ����� ���������
dims = dim(winedata) # n x m
n = dims[1] # ����� ������� # 178
m = dims[2] # ���������� �������� # 14 (13+1 ���������� �������) 

# ����� �������, ��������, 120 �������, ����� ������������ ��� ��������� �������
n_train = 120



# ���������� ������� ������������� �������
# � �������� �������
# (������� ��� ������� � %, �������� �� �������)
round(prop.table(table(winedata[1]))*100, digits = 1) # ��������� ����������� ������� (�������) (p(Hk))


# ����� ���������� ������������������ �������,���������� �
set.seed(12345) # ��������������� �����
winedata_mixed=winedata[order(runif(n)),]


# ��������� ������ � ��������� �������
# ������� ��������� �������
train_data = winedata_mixed[1:n_train,]



# �������� ������ ������� ��� ����� ��������� ������� � ���������� train_data_labels
train_data_labels = train_data[,1]


# ���������� ������������ ������� factor ��� ������������ ����� ��������������� ������� �������� ���������� �������
train_data_labels<-factor(train_data_labels)
# ���������� ������� ����������� ������� ������ �
# ��������� �������
# � ������� � ���������������� ��������� �
# �������� ������



#������� ������ ��� �������� ������������
  
new_predict<-winedata[c(1:2),]
winedata[-c(1,2),]
  
round(prop.table(table(train_data[1]))*100,
        digits = 1)

# ���������� ����� "������������ �������" ����� ������������ ��� �������� �������
test_data = winedata_mixed[(n_train+1):n, ]
test_data_labels = test_data[,1]

  
# ����� ������� �������� ������������ �������������� ������ ������ ������� �� ��������� � �������� �������
train_data = train_data[-1] # "�����" ������� �� ��������� �������������� ��������
test_data = test_data[-1] # "�����" ������� �������� ������� ���������� �������������� �� �����!
    

# ��������� (��� �������������) � ��������� ����� e1071:
    
#install.packages("e1071")

library(e1071)

# ������� ������� ����������� �������������:
my_classifier <- naiveBayes(train_data,
                              train_data_labels)
# ������� ��������� �������: ������� (train_data) � ����� ������� (train_data_labels)
# ��������� ("�������������") ����� ��� ��������
# ������  ������� ������������� � �������� �������
data_test_pred <- predict(my_classifier,
                            test_data) 

  #������� ������������� � �������� ������� 
  # (������ �������, ��� �����)
  # ��� ������ �������� ��������
  # ��������� (���� �� ���������� �����) ���������� gmodels:

   # install.packages("gmodels")
    library("gmodels")
  # � �������� �����-������������� �������:
CrossTable(x = test_data_labels,
             y = data_test_pred,
             prop.chisq=TRUE)
  
new_predict
data_test_pred<-predict(my_classifier,new_predict)
data_test_pred
  