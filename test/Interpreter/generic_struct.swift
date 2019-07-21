// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

protocol MyPrintable {
  func myPrint()
}

extension Int : MyPrintable {
  func myPrint() {
    print(self.description, terminator: "")
  }
}

extension String : MyPrintable {
  func myPrint() {
    print(self.debugDescription, terminator: "")
  }
}

struct BufferedPair<T, U> {
  var front: UInt8
  var first: T
  var second: U
  var back: UInt8
}

enum State : MyPrintable {
  case CA, OR, WA

  func myPrint() {
    switch self {
    case .CA:
      print("California", terminator: "")
    case .OR:
      print("Oregon", terminator: "")
    case .WA:
      print("Washington", terminator: "")
    }
  }
}

func printPair<A : MyPrintable, B : MyPrintable>(_ p: BufferedPair<A,B>) {
  print("\(p.front) ", terminator: "")
  p.first.myPrint()
  print(" ", terminator: "")
  p.second.myPrint()
  print(" \(p.back)")
}

var p = BufferedPair(front: 219, first: State.OR, second: "Idaho's Portugal",
                     back: 17)
// CHECK: 219 Oregon "Idaho\'s Portugal" 17
printPair(p)
