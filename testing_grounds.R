x = 0
while(x<=3) {
  print("x is less than 4")
  x = x+1}

s = 0
for (i in 1:10) {
  s = s + i
print(s) # u can cheat the indents lol r doesnt care
}

x = c(1:100); s = 0
for (i in 1:length(x)){
  print("starting loop")
  if (x[i]%%2 == 0) {
    s = s + x[i]
    print(s)
  } else {
    s = s
  }
}
