#Write an if-else-if structure that explores and prints the quadrant in which an angle belongs. For example, if you input 45 degree it should print ‘First quadrant’
angle=45
if (angle>=0 && angle<90){
  print('First quadrant')
}else if(angle>=90 && angle<180){
  print('Second quadrant')
}else if(angle>=180 && angle<270){
  print('Third quadrant')
}else if(angle>=270 && angle<360){
  print('Fourth quadrant')
} else{
  print('Invalid angle')
} 

#Write an if-else-if structure that takes three numeric inputs and uses this structure alone to put the 3 numbers in decreasing order. Do not use any built in function for sorting purposes
# Input three numbers
num1 <- 12
num2 <- 7
num3 <- 15
# Sorting numbers in decreasing order using if-else-if
if (num1 >= num2 & num1 >= num3) {
  if (num2 >= num3) {
    print(paste("The numbers in decreasing order: ", num1, num2, num3))
  } else {
    print(paste("The numbers in decreasing order: ", num1, num3, num2))
  }
} else if (num2 >= num1 & num2 >= num3) {
  if (num1 >= num3) {
    print(paste("The numbers in decreasing order: ", num2, num1, num3))
  } else {
    print(paste("The numbers in decreasing order: ", num2, num3, num1))
  }
} else {
  if (num1 >= num2) {
    print(paste("The numbers in decreasing order: ", num3, num1, num2))
  } else {
    print(paste("The numbers in decreasing order: ", num3, num2, num1))
  }
}
#Let’s say the cost of a journey ticket depends not only on the distance travelled but also on the details of the traveller. Distance-wise, the cost is a minimum of Rs.
#100 for the first 100km, Rs. 1.50 for every extra km until 1000km and Rs.2 per km thereafter. On top of that, senior citizens (> 60 years ) get a 25% concession
#and children under 6 years of age get 50% concession. Write a code that takes the journey distance and the traveller’s age as inputs, and prints out the ticket cost.
# Input: Journey distance and traveller's age
distance <- as.numeric(readline(prompt = "Enter the journey distance (in km): "))
age <- as.numeric(readline(prompt = "Enter the traveller's age: "))

# Initialize the ticket cost
ticket_cost <- 0

# Calculate the base cost based on the distance
if (distance <= 100) {
  ticket_cost <- 100  # Fixed cost for the first 100 km
} else if (distance <= 1000) {
  ticket_cost <- 100 + (distance - 100) * 1.50  # Rs. 1.50 per km after 100 km
} else {
  ticket_cost <- 100 + (1000 - 100) * 1.50 + (distance - 1000) * 2  # Rs. 2 per km after 1000 km
}

# Apply concessions based on age
if (age > 60) {
  ticket_cost <- ticket_cost * 0.75  # 25% concession for senior citizens
} else if (age < 6) {
  ticket_cost <- ticket_cost * 0.50  # 50% concession for children under 6 years
}

# Print the final ticket cost
print(paste("The total ticket cost is: Rs.", round(ticket_cost, 2)))
