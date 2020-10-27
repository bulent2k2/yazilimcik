clear
invisible

val t1 = newTurtle
val t2 = newTurtle(0, 50)

t1.penUp
t1.moveTo(-100, -50)
t1.setHeading(90)

t2.write("    Hey t1, where'r you goin?\n"
    + s"    We are already ${round(t2.distanceTo(t1), 2)} apart.\n"
    + "    Aren't we? Come back!")
    
val input = readln("Enter some text when ready for t1 to speak up..")

t1.penDown
t1.write("    I don't know. I guess so.\n"
    + "    I was listening to Baray who tells me that\n"
    + s"    '$input.'")
