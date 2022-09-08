# Haskell Vacuum Agent

Introduction:

I set out to make a vacuum AI agent that would make smart decisions. This program behaves more like a basic Roomba with limited dirt sensors: it will clean the dirt where it is and then either move towards an adjacent dirt location that it can see or move in a random direction. The user chooses the number of moves during execution, and the vacuum robot always starts in the center square. The program is built with ‘cabal build’ and executed with ‘cabal exec vacuum.’

Explanation of Code:

All IO functionality is contained in the Main.hs file. The rest of the functionality is in the Vacuum.hs file. 

In Main.hs, main gets a number of moves ‘n’ from the user during execution, and then calls robotGo with n, an initial position, and an initial world state. RobotGo will either have the robot clean the dirt at the current position, or move the robot to another position. Each time it recurses, it decreases n. If the move option is being executed, it checks for the case where robotMove returns the same position the robot is already in. This means that the robot has not sensed dirt on any adjacent squares. If this is the case, it will choose a random move. At the start of each execution of robotGo, the world state is printed. 
Once n has decreased to zero, the program will end.

Reflection:

I didn’t get as far as I’d hoped with this project. I haven’t fully internalized the way that Haskell works, and found my usual imperative programming instincts determining how I sought to create this program. The next step for this program is to refactor from less naive, imperative-inspired functions into more ‘Haskellonic’ code and take advantage of higher-order functions. 
