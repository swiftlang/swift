// RUN: %target-swift-frontend -emit-ir -primary-file %s | FileCheck %s

// REQUIRES: CPU=i386_or_x86_64

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
  // CHECK: define hidden void @_TFV16associated_types3Owl3eat{{.*}}(%swift.opaque*
  func eat(what: T.RuncerType.Runcee, and: T.RuncerType, with: T) { }
}

class Pussycat<T : Runcible, U> {
  init() {} 

  // CHECK: define hidden void @_TFC16associated_types8Pussycat3eat{{.*}}(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %C16associated_types8Pussycat*)
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
