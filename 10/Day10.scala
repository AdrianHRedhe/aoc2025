package day10

import util.readInput
import com.microsoft.z3

object Solution:

  case class Machine(configuration: List[Boolean], buttons: List[List[Int]], joltage: List[Int])

  def parse(input: List[String]): List[Machine] =
    def parseSingleInstruction(s: String): List[Int] =
      val inside = s.tail.init
      s.head match
        case '[' => inside.map {
          case '.' => -1
          case '#' => 1
        }.toList
        case '(' | '{' => inside.split(',').map(_.toInt).toList
          
    def parseMachine(s: String): Machine = 
      val parts = s.split(" ").map(parseSingleInstruction).toList
      val configuration = parts.head.map {
        case 1 => true 
        case -1 => false
      }
      val joltage = parts.last
      val buttons = parts.tail.init        
      Machine(configuration, buttons, joltage)

    input.map(parseMachine)

  def toBit(int: Int): Int = 
    1 << int

  // Each button should be pressed either zero or once as twice is the same as doing it zero times
  def allButtonCombinations(nButtons: Int): List[List[Boolean]] =
    // Bit-masking to get a list of all combinations of binary key presses
    def getCombination(nButtons: Int)(mask: Int): List[Boolean] =
      (0 until nButtons).map(i => (mask & toBit(i)) != 0).toList

    val nCombinations = math.pow(2, nButtons).toInt
    (0 until nCombinations).toList.map(getCombination(nButtons))

  def pushButtons(buttons: List[List[Int]])(buttonCombination: List[Boolean]): (Int, Int) =
    val activatedButtons = buttons.zip(buttonCombination).collect { case (button, true) => button }
    val lightIndicesToFlip = activatedButtons.flatten
    val finalState = lightIndicesToFlip.foldLeft(0)((bitmask, lightIndex) => bitmask ^ toBit(lightIndex))
    (finalState , activatedButtons.length)

  def stepsToConfigure(machine: Machine): Int =
    val nButtons = machine.buttons.length
    // Convert configuration list to bitmask
    val configurationBitMask = machine.configuration.zipWithIndex
      .filter(_._1 == true)
      .map(_._2)
      .foldLeft(0)((acc, idx) => acc | toBit(idx))

    val buttonCombinations = allButtonCombinations(nButtons)
    val outcomes = buttonCombinations.map(pushButtons(machine.buttons))
    outcomes.collect { case (finalState, steps) if finalState==configurationBitMask => steps}.min

  def part1(machines: List[Machine]): Int =
    machines.map(stepsToConfigure).sum

  // Create one integer variable per button
  def createButtonVariables(ctx: z3.Context, numButtons: Int): Array[z3.IntExpr] =
    (0 until numButtons).map(i => ctx.mkIntConst(s"button_$i")).toArray

  // Each button must be pressed 0 or more times
  def addNonNegativeConstraints(ctx: z3.Context, optimizer: z3.Optimize, buttonVars: Array[z3.IntExpr]): z3.Optimize =
    for (buttonVariable <- buttonVars)
      val constraint = ctx.mkGe(buttonVariable, ctx.mkInt(0))
      optimizer.Add(constraint)
    optimizer

  // For each joltage indicator, sum of button effects must equal target
  def addJoltageConstraints(
    ctx: z3.Context, 
    optimizer: z3.Optimize, 
    buttonVars: Array[z3.IntExpr],
    machine: Machine
  ): z3.Optimize =
    for (indicatorIdx <- machine.joltage.indices)
      val constraint = buildJoltageConstraint(ctx, machine, buttonVars, indicatorIdx)
      optimizer.Add(constraint)
    optimizer

  // Sum of presses of all button vars affecting this indicator == target joltage
  def buildJoltageConstraint(
    ctx: z3.Context,
    machine: Machine,
    buttonVars: Array[z3.IntExpr],
    indicatorIdx: Int
  ): z3.BoolExpr =
    val targetJoltage = machine.joltage(indicatorIdx)
    
    val affectingButtonVars = machine.buttons.zipWithIndex.collect {
      case (buttonEffects, btnIdx) if buttonEffects.contains(indicatorIdx) =>
        buttonVars(btnIdx)
    }
    ctx.mkEq(sumButtonVariables(ctx, affectingButtonVars), ctx.mkInt(targetJoltage))

  def sumButtonVariables(ctx: z3.Context, buttons: Seq[z3.IntExpr]): z3.ArithExpr[?] =
    buttons.reduceLeft((acc, b) => ctx.mkAdd(acc, b)) 

  def addMinimizationObjective(ctx: z3.Context, optimizer: z3.Optimize, buttonVars: Array[z3.IntExpr]): z3.Optimize =
      val totalPresses = sumButtonVariables(ctx, buttonVars)
      optimizer.MkMinimize(totalPresses)
      optimizer

  def extractSolutionFromOptimizer(optimizer: z3.Optimize, buttonVars: Array[z3.IntExpr]): Long =
    if (optimizer.Check() == z3.Status.SATISFIABLE) {
      val model = optimizer.getModel()
      buttonVars.map { v => model.eval(v, false).toString.toLong }.sum
    } else {
      throw new Exception("No solution found!")
    }

  def solveWithZ3(machine: Machine): Long =
    val ctx = new z3.Context()
    try
      var optimizer = ctx.mkOptimize()
      val buttonVars = createButtonVariables(ctx, machine.buttons.length)
    
      // Unfortunately this modifies the optimizer in place but at least it is returned so that it is not hidden
      optimizer = addNonNegativeConstraints(ctx, optimizer, buttonVars)
      optimizer = addJoltageConstraints(ctx, optimizer, buttonVars, machine)
      optimizer = addMinimizationObjective(ctx, optimizer, buttonVars)
      extractSolutionFromOptimizer(optimizer, buttonVars)
    finally
      ctx.close()

  def part2(machines: List[Machine]): Long =
    machines.map(solveWithZ3).sum

@main def main(): Unit =
  val input = readInput(10)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
