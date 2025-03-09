#Ex5: Write a script to recreate the plot in the slide with the plot title ’This is a graph’.

# Define the data
Time <- 1:10
Performance <- c(2, 6, 7, 5, 6, 9, 10, 8, 9, 10)

# Create an empty plot
plot(Time, Performance, type = "n", main = "This is a graph", 
     xlab = "Time", ylab = "Performance", 
     col.main = "blue", font.main = 2, cex.main = 1.5, 
     col.lab = "black", cex.lab = 1.2, 
     xaxt = "n", yaxt = "n")

# Add custom axes
axis(1, at = seq(2, 10, by = 2))  # X-axis ticks
axis(2, at = seq(2, 10, by = 2))  # Y-axis ticks

# Add the dashed line
lines(Time, Performance, lty = 2, col = "pink", lwd = 2)

# Add the points
points(Time, Performance, col = "pink", pch = 16, cex = 2)

# Add text labels at points
text(Time, Performance, labels = Performance, col = "red", pos = 3, cex = 1.2)

# Add a legend
legend("topleft", legend = "Per curve", col = "pink", lty = 2, pch = 16, box.lty = 1)
