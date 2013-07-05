// RUN: %swift -i %s | FileCheck %s

protocol RollCallable {
  func rollCall() -> String
}

protocol Snarker {
  func snark() -> String
}

struct Cambot : RollCallable {
  func rollCall() -> String { return "Cambot!" }
}
struct Gypsy : RollCallable {
  func rollCall() -> String { return "Gypsy!" }
}
struct TomServo : RollCallable {
  func rollCall() -> String { return "Tom Servo!" }
}
struct Crow : RollCallable, Snarker {
  func rollCall() -> String { return "Croooow!" }
  func snark() -> String { return "That's one O!" }
}

func printRollCall(x:RollCallable) {
  println(x.rollCall())
}

func printRollCallWithSnark(x:protocol<RollCallable, Snarker>) {
  printRollCall(x)
  println("(\(x.snark()))")
}

// CHECK: Cambot!
printRollCall(Cambot())
// CHECK: Gypsy!
printRollCall(Gypsy())
// CHECK: Tom Servo!
printRollCall(TomServo())
// CHECK: Croooow!
// CHECK: (That's one O!)
printRollCallWithSnark(Crow())
