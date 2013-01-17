// RUN: %swift -sil-i %s | FileCheck %s
// requires irgen support for SIL protocol_method instruction
// XFAIL: *

protocol RollCallable {
  func rollCall() -> String
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
struct Crow : RollCallable {
  func rollCall() -> String { return "Croooow!" }
}

// FIXME: ProtocolMethodInst not irgenned yet
func printRollCall(x:RollCallable) -> () /*{
  println(x.rollCall())
}*/

// CHECK: Cambot!
printRollCall(Cambot())
// CHECK: Gypsy!
printRollCall(Gypsy())
// CHECK: Tom Servo!
printRollCall(TomServo())
// CHECK: Croooow!
printRollCall(Crow())
