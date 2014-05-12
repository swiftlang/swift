// RUN: %swift -emit-ir -target x86_64-apple-darwin10 %s | FileCheck %s
protocol Runcer {
  typealias Runcee
}

protocol Runcible {
  typealias RuncerType : Runcer
  typealias AltRuncerType : Runcer
}

struct Mince {}

struct Quince : Runcer {
  typealias Runcee = Mince
}

struct Spoon : Runcible {
  typealias RuncerType = Quince
  typealias AltRuncerType = Quince
}

struct Owl<T : Runcible, U> {
  // CHECK: define void @_TFV16associated_types3Owl3eat{{.*}}(%swift.opaque*
  func eat(what: T.RuncerType.Runcee, and: T.RuncerType, with: T) { }
}

class Pussycat<T : Runcible, U> {
  init() {} 

  // CHECK: define void @_TFC16associated_types8Pussycat3eat{{.*}}(%swift.opaque* noalias, %swift.opaque* noalias, %swift.opaque* noalias, %C16associated_types8Pussycat*)
  func eat(what: T.RuncerType.Runcee, and: T.RuncerType, with: T) { }
}

func owl() -> Owl<Spoon, Int> {
  return Owl()
}

func owl2() {
  Owl<Spoon, Int>().eat(Mince(), and: Quince(), with: Spoon())
}


func pussycat() -> Pussycat<Spoon, Int> {
  return Pussycat()
}

func pussycat2() {
  Pussycat<Spoon, Int>().eat(Mince(), and: Quince(), with: Spoon())
}
