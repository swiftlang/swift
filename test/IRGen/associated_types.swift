// RUN: %target-swift-frontend -emit-ir -primary-file %s | %FileCheck %s

// REQUIRES: CPU=i386_or_x86_64

protocol Runcer {
  associatedtype Runcee
}

protocol Runcible {
  associatedtype RuncerType : Runcer
  associatedtype AltRuncerType : Runcer
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
  func eat(_ what: T.RuncerType.Runcee, and: T.RuncerType, with: T) { }
}

class Pussycat<T : Runcible, U> {
  init() {} 

  // CHECK: define hidden void @_TFC16associated_types8Pussycat3eat{{.*}}(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %C16associated_types8Pussycat*)
  func eat(_ what: T.RuncerType.Runcee, and: T.RuncerType, with: T) { }
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

protocol Speedy {
  static func accelerate()
}
protocol FastRuncer {
  associatedtype Runcee : Speedy
}

protocol FastRuncible {
  associatedtype RuncerType : FastRuncer
}

// This is a complex example demonstrating the need to pull conformance
// information for archetypes from all paths to that archetype, not just
// its parent relationships.

func testFastRuncible<T: Runcible, U: FastRuncible where T.RuncerType == U.RuncerType>(_ t: T, u: U) {
  U.RuncerType.Runcee.accelerate()
}
// CHECK: define hidden void @_TF16associated_types16testFastRuncibleu0_RxS_8Runcible_S_12FastRunciblewx10RuncerTypezw_10RuncerTyperFTx1uq__T_(%swift.opaque* noalias nocapture, %swift.opaque* noalias nocapture, %swift.type* %T, %swift.type* %U, i8** %T.Runcible, i8** %U.FastRuncible)
//   1. Get the type metadata for U.RuncerType.Runcee.
//     1a. Get the type metadata for U.RuncerType.
//         Note that we actually look things up in T, which is going to prove unfortunate.
// CHECK:      [[T0:%.*]] = load i8*, i8** %T.Runcible,
// CHECK-NEXT: [[T1:%.*]] = bitcast i8* [[T0]] to %swift.type* (%swift.type*, i8**)*
// CHECK-NEXT: %T.RuncerType = call %swift.type* [[T1]](%swift.type* %T, i8** %T.Runcible)
//     1b. Get the protocol witness table for U.RuncerType : Runcer.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8*, i8** %T.Runcible, i32 1
// CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]]
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to i8** (%swift.type*, %swift.type*, i8**)*
// CHECK-NEXT: %T.RuncerType.Runcer = call i8** [[T2]](%swift.type* %T.RuncerType, %swift.type* %T, i8** %T.Runcible)
//     1c. Get the type metadata for U.RuncerType.Runcee.
// CHECK-NEXT: [[T0:%.*]] = load i8*, i8** %T.RuncerType.Runcer,
// CHECK-NEXT: [[T1:%.*]] = bitcast i8* [[T0]] to %swift.type* (%swift.type*, i8**)*
// CHECK-NEXT: %T.RuncerType.Runcee = call %swift.type* [[T1]](%swift.type* %T.RuncerType, i8** %T.RuncerType.Runcer)
//   2. Get the witness table for U.RuncerType.Runcee : Speedy
//     2a. Get the protocol witness table for U.RuncerType : FastRuncer.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8*, i8** %U.FastRuncible, i32 1
// CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]],
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to i8** (%swift.type*, %swift.type*, i8**)*
// CHECK-NEXT: %T.RuncerType.FastRuncer = call i8** [[T2]](%swift.type* %T.RuncerType, %swift.type* %U, i8** %U.FastRuncible)
//     2b. Get the witness table for U.RuncerType.Runcee : Speedy.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds i8*, i8** %T.RuncerType.FastRuncer, i32 1
// CHECK-NEXT: [[T1:%.*]] = load i8*, i8** [[T0]],
// CHECK-NEXT: [[T2:%.*]] = bitcast i8* [[T1]] to i8** (%swift.type*, %swift.type*, i8**)*
// CHECK-NEXT: %T.RuncerType.Runcee.Speedy = call i8** [[T2]](%swift.type* %T.RuncerType.Runcee, %swift.type* %T.RuncerType, i8** %T.RuncerType.FastRuncer)
//   3. Perform the actual call.
// CHECK-NEXT: [[T0:%.*]] = load i8*, i8** %T.RuncerType.Runcee.Speedy,
// CHECK-NEXT: [[T1:%.*]] = bitcast i8* [[T0]] to void (%swift.type*, %swift.type*, i8**)*
// CHECK-NEXT: call void [[T1]](%swift.type* %T.RuncerType.Runcee, %swift.type* %T.RuncerType.Runcee, i8** %T.RuncerType.Runcee.Speedy)

