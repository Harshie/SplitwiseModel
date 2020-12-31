# Example 1 of ALDI Dataset
#Price and Category in ASCENDING ORDER
aldi1sept <-c(.39,"TY",3.5,'TY',1.39,'C',.36*2,'TY',3,'TY',.89,'TY',1.99*3,'TY',2,'TY',.95,'E',2.99,'C',.97+.34+.34+2.94+2.94-1.88+.39+5,'TY',1.35,'C',1.08,'Y',1.99,'T',2.26,'Y',.89*2,'N',1.49,'C',1.99,'T',2+1.19,'TY',.99,'C',1.31,'Y',.79*4,'C')

#Matrix Format
YASHALDI <-matrix(aldi1sept,ncol=2,byrow=T)

#To data frame
d1<-data.frame(YASHALDI)

#Price from character to numeric format
d1$X1<-as.numeric(d1$X1)

#validating the total sum
sum(d1$X1)

#count of category
table(d1$X2)

# calculating the share
d2<-aggregate(d1$X1, by=list(Category=d1$X2), FUN=sum)

# verifying the total bill cost
sum(d2$x)
##############
#Example 2 of Asian store dataset

#Price and Category in ASCENDING ORDER
aldi1sept <-c(2.49,'EHMN',2.89*4,'HMNTY',3.49,'E',3.49,'HM',3.49,'N',3.49,'T',3.49,'Y',3.25,'C',1.25,'N',1.25,'TY',30.99,'C')

#Matrix Format
YASHALDI <-matrix(aldi1sept,ncol=2,byrow=T)

#To data frame
d1<-data.frame(YASHALDI)

#Price from character to numeric format
d1$X1<-as.numeric(d1$X1)

#validating the total sum
sum(d1$X1)

#count of category
table(d1$X2)

# calculating the share
d2<-aggregate(d1$X1, by=list(Category=d1$X2), FUN=sum)
print(d2)
# verifying the total bill cost
sum(d2$x)

#############

#Example 3 of TESCO store dataset

#Price and Category in ASCENDING ORDER
aldi1sept <-c(1.08,'H',0.69,'H',0.9,'EMT',2,'EHMNT',1.29,'C',1,'H',1,'H',0.99,'C',0.59,'H',0.44,'C')

#Matrix Format
YASHALDI <-matrix(aldi1sept,ncol=2,byrow=T)

#To data frame
d1<-data.frame(YASHALDI)

#Price from character to numeric format
d1$X1<-as.numeric(d1$X1)

#validating the total sum
sum(d1$X1)

#count of category
table(d1$X2)

# calculating the share
d2<-aggregate(d1$X1, by=list(Category=d1$X2), FUN=sum)
print(d2)
# verifying the total bill cost
sum(d2$x)

#############

#Example 4 of TESCO store dataset

#Price and Category in ASCENDING ORDER
aldi1sept <-c(1.79,'TY',2,'EHMNT',0.89,'TY',0.69,'C',1.29,'TY',3,'EHNTY',0.49,'HT',0.49,'HT',0.99,'C',1,'T')

#Matrix Format
YASHALDI <-matrix(aldi1sept,ncol=2,byrow=T)

#To data frame
d1<-data.frame(YASHALDI)

#Price from character to numeric format
d1$X1<-as.numeric(d1$X1)

#validating the total sum
sum(d1$X1)

#count of category
table(d1$X2)

# calculating the share
d2<-aggregate(d1$X1, by=list(Category=d1$X2), FUN=sum)
print(d2)
# verifying the total bill cost
sum(d2$x)

#############


#Example 5 of TESCO store dataset

#Price and Category in ASCENDING ORDER
aldi1sept <-c(2,'EHMTY',.99*2,'C',.49,'C',1.29*2,'C',.89*2,'C',.59*2,'C',.97,'N')

#Matrix Format
YASHALDI <-matrix(aldi1sept,ncol=2,byrow=T)

#To data frame
d1<-data.frame(YASHALDI)

#Price from character to numeric format
d1$X1<-as.numeric(d1$X1)

#validating the total sum
sum(d1$X1)

#count of category
table(d1$X2)

# calculating the share
d2<-aggregate(d1$X1, by=list(Category=d1$X2), FUN=sum)
print(d2)
# verifying the total bill cost
sum(d2$x)

#############

#Example 5 of TESCO store dataset

#Function for splitting the bill
split <- function(billList){
  #Matrix Format
  listtomatrix <-matrix(billList,ncol=2,byrow=T)
  
  #To data frame
  d1<-data.frame(listtomatrix)
  
  #Price from character to numeric format
  d1$X1<-as.numeric(d1$X1)
  
  #validating the total sum
  cat("Printing total expense for verification",sum(d1$X1))
  
  #count of category
  print(table(d1$X2))
  
  # calculating the share
  d2<-aggregate(d1$X1, by=list(Category=d1$X2), FUN=sum)
  print("The split of the bill is: ")
  print(d2)
  
  # verifying the total bill cost
  cat("Printing total expense for verification",sum(d2$x))
  
}

#Price and Category in ASCENDING ORDER
billList <-c(1.08+.89,'H',.89+.89+.99+.59+.59+.88+.99+.99+.49+.49+.81+.99+.99+4.58+1.99+4+1.35+1.35+1.76+.69+2.5+.69+.69+.61+.46,'C',.85+.85+1.49,'EHT',1.99,'E',.89,'EH',5,'N',.28*2,'EH',.9,'E',1.89+.69,'H',6,'EHMNY',.46,'HN',2.5*2,'E',2.5,'EHNTY')
split(billList)

#nUMBER OF TENENTS
n=6
tenentsInitial <- c('E','H','M','N','T','Y')


#############



# Development phase
y1 <-d2
y2 <-d2

y3<-data.frame()
temp1<-c(y1$Category,y2$Category)
temp2<-unique(temp1)

temp3<-0

for(i in 1:length(temp2)){
  temp3<-0
  if(temp2[i] == y1$Category)
    temp3<-temp3 + y1$x[i]
  if(temp2[i] == y2$Category)
    temp3<-temp3 + y2$x[i]
  print(temp2[i])
  print(temp3)
}
