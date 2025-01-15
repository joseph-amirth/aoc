# 2023 - Day 20

Visualizing the graph immediately revealed what needed to be done: find the period of each
independent lasso and find where they coincide. The lassos are all cycles, so it is just a matter of
finding the LCM of the periods of the cycles. [`Main.kt`](Main.kt) just finds the period of each
cycle, and I found the LCM manually by plugging the numbers into https://www.wolframalpha.com/.
