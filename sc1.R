colors <- factor(c("Red", "Green", "Blue", "Yellow", "Black"))
grades<- factor(c("A", "B", "C", "D", "F"))
temperatures<- c(15, 20, 25, 30, 35)
distances<- c(0, 5, 10, 15, 20)
temp <- 24
if( temp > 25){
  print("It's hot!")
  }else{
   print("It's not hot!")
}

grade<- "A"
if(grade== "A"){
  print("Exellent!")
}else if(grade =="B"){
  print("Good job!")
}else{
  print("Keep working hard!")
}

distance2 <- 15
if(distance2 > 10){
  print("That's far!")
}else{
  print("Not too far!")
}

for(i in 1:5){
  print(colors[i])
}

for(i in 1:5){
  print(grades[i])
}


for(i in temperatures){
  if(i>25){print("above")
    print(temperatures[i])}
    else if(i){print("below")
    print(temperatures[i])}
}
