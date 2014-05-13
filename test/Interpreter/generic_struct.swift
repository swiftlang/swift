// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift -Xfrontend -enable-dynamic-value-type-layout %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

protocol MyPrintable {
  func myPrint()
}

extension Int : MyPrintable {
  func myPrint() {
    print(self.description)
  }
}

extension String : MyPrintable {
  func myPrint() {
    print(self.debugDescription)
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
      print("California")
    case .OR:
      print("Oregon")
    case .WA:
      print("Washington")
    }
  }
}

func printPair<A : MyPrintable, B : MyPrintable>(p: BufferedPair<A,B>) {
  print("\(p.front) ")
  p.first.myPrint()
  print(" ")
  p.second.myPrint()
  println(" \(p.back)")
}

var p = BufferedPair(front: 219, first: State.OR, second: "Idaho's Portugal",
                     back: 17)
// CHECK: 219 Oregon "Idaho\'s Portugal" 17
printPair(p)
