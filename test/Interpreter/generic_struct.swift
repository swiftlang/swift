// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift -Xfrontend -enable-dynamic-value-type-layout %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

struct BufferedPair<T, U> {
  var front: UInt8
  var first: T
  var second: U
  var back: UInt8
}

enum State : ReplPrintable {
  case CA, OR, WA

  func replPrint() {
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

func printPair<A : ReplPrintable, B : ReplPrintable>(p: BufferedPair<A,B>) {
  print("\(p.front) ")
  p.first.replPrint()
  print(" ")
  p.second.replPrint()
  println(" \(p.back)")
}

var p = BufferedPair(front: 219, first: State.OR, second: "Idaho's Portugal", 
                     back: 17)
// CHECK: 219 Oregon "Idaho\'s Portugal" 17
printPair(p)
