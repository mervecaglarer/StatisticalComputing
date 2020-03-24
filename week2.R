#vectors
gender<-c("M","M","F","M","M","F","F","M")
height<-c(63,67,58,78,65,62,69,75) #1inch=2.54 cm
weight<-c(127,165,121,164,221,136,171,317)#1pound=0.45 kg
ls()

#answers
#a. comvert h in cm and w in kg
height<-height*2.54
weight<-weight*0.45
#b. calculate mean and sd
hmean<- sum(height)/length(height)
hmean
wmean<- sum(weight)/length(weight)
wmean
hsd<-sqrt(sum((height-hmean)^2)/(length(height)-1)) #st.h<-sqrt(sum((height-mean.height)^2)/(lenght(height)-1))
hsd
wsd<-sqrt(sum((weight-wmean)^2)/(length(weight)-1)) #st.w<-sqrt(sum((weight-mean.weight)^2)/(lenght(weight)-1))
wsd
#c. find any students that are obese in terms of BMI>20
bmi<- weight/height^2*10000
bmi[bmi>20]

students<-data.frame(no=c(1:8),gender,height,weight,bmi)
students[students$bmi>20,1]
students[students$bmi>20,-c(5)]
students
