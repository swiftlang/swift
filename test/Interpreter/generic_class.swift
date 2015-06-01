// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

protocol MyPrintable {
  func myPrint()
}

extension Int : MyPrintable {
  func myPrint() {
    print(self.description, appendNewline: false)
  }
}

extension Double : MyPrintable {
  func myPrint() {
    print(self.description, appendNewline: false)
  }
}

extension String : MyPrintable {
  func myPrint() {
    print(self.debugDescription, appendNewline: false)
  }
}

class BufferedPair<T, U> {
  var front: UInt8
  var first: T
  var second: U
  var back: UInt8

  init(_ front: UInt8, _ first: T, _ second: U, _ back: UInt8) {
    self.front = front
    self.first = first
    self.second = second
    self.back = back
  }
}

enum State : MyPrintable {
  case CA, OR, WA

  func myPrint() {
    switch self {
    case .CA:
      print("California", appendNewline: false)
    case .OR:
      print("Oregon", appendNewline: false)
    case .WA:
      print("Washington", appendNewline: false)
    }
  }
}

func printPair<A: MyPrintable, B: MyPrintable>(p: BufferedPair<A,B>) {
  print("\(p.front) ", appendNewline: false)
  p.first.myPrint()
  print(" ", appendNewline: false)
  p.second.myPrint()
  print(" \(p.back)")
}

var p = BufferedPair(99, State.OR, "Washington's Mexico", 84)
// CHECK: 99 Oregon "Washington\'s Mexico" 84
printPair(p)

class AwkwardTriple<V, W, X> : BufferedPair<V, W> {
  var third: X

  init(_ front: UInt8, _ first: V, _ second: W, _ back: UInt8, _ third: X) {
    self.third = third
    super.init(front, first, second, back)
    self.third = third
  }
}

func printTriple
  <D: MyPrintable, E: MyPrintable, F: MyPrintable>
  (p: AwkwardTriple<D, E, F>)
{
  print("\(p.front) ", appendNewline: false)
  p.first.myPrint()
  print(" ", appendNewline: false)
  p.second.myPrint()
  print(" \(p.back) ", appendNewline: false)
  p.third.myPrint()
  print("")
}

var q = AwkwardTriple(123, State.CA, "Foo", 234, State.WA)
// CHECK: 123 California "Foo" 234
printPair(q)
// CHECK: 123 California "Foo" 234 Washington
printTriple(q)

class FourthWheel<P, Q, R, S> : AwkwardTriple<P, Q, R> {
  var fourth: S

  init(_ front: UInt8, _ first: P, _ second: Q, _ back: UInt8, _ third: R, 
       _ fourth: S) {
    self.fourth = fourth
    super.init(front, first, second, back, third)
    self.fourth = fourth
  }
}

func printQuad
  <G: MyPrintable, H: MyPrintable, I: MyPrintable, J: MyPrintable>
  (p: FourthWheel<G, H, I, J>)
{
  print("\(p.front) ", appendNewline: false)
  p.first.myPrint()
  print(" ", appendNewline: false)
  p.second.myPrint()
  print(" \(p.back) ", appendNewline: false)
  p.third.myPrint()
  print(" ", appendNewline: false)
  p.fourth.myPrint()
  print("")
}

var r = FourthWheel(21, State.WA, "Bar", 31, State.OR, 3.125)
// CHECK: 21 Washington "Bar" 31
printPair(r)
// CHECK: 21 Washington "Bar" 31 Oregon
printTriple(r)
var rAsPair: BufferedPair<State, String> = r
// CHECK: 21 Washington "Bar" 31 Oregon
printTriple(rAsPair as! AwkwardTriple<State, String, State>)
// CHECK: 21 Washington "Bar" 31 Oregon 3.125
printQuad(r)
// CHECK: 21 Washington "Bar" 31 Oregon 3.125
printQuad(rAsPair as! FourthWheel<State, String, State, Double>)

class ConcretePair {
  var first, second: UInt8

  init(_ first: UInt8, _ second: UInt8) {
    self.first = first
    self.second = second
  }
}

class SemiConcreteTriple<O> : ConcretePair {
  var third: O

  init(_ first: UInt8, _ second: UInt8, _ third: O) {
    self.third = third
    super.init(first, second)
    self.third = third
  }
}

func printConcretePair(p: ConcretePair) {
  print("\(p.first) \(p.second)")
}

func printSemiTriple<O : MyPrintable>(p: SemiConcreteTriple<O>) {
  print("\(p.first) \(p.second) ", appendNewline: false)
  p.third.myPrint()
  print("")
}

var s = SemiConcreteTriple(120, 230, State.CA)
// CHECK: 120 230
printConcretePair(s)
// CHECK: 120 230 California
printSemiTriple(s)
var t = SemiConcreteTriple(121, 231, "California's Canada")
// CHECK: 121 231
printConcretePair(t)
// CHECK: 121 231 "California\'s Canada"
printSemiTriple(t)

class MoreConcreteQuadruple : SemiConcreteTriple<State> {
  var fourth: String

  init(_ first: UInt8, _ second: UInt8, _ third: State, _ fourth: String) {
    self.fourth = fourth
    super.init(first, second, third)
  }
}

var u = MoreConcreteQuadruple(10, 17, State.CA, "Hella")

// CHECK: 10 17
printConcretePair(u)
