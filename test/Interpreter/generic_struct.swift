// RUN: %swift -enable-dynamic-value-type-layout -i %s | FileCheck %s

struct BufferedPair<T, U> {
  var front : UInt8
  var first : T
  var second : U
  var back : UInt8
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

func printPair<A:ReplPrintable, B:ReplPrintable>(p:BufferedPair<A,B>) {
  print("\(p.front) ")
  p.first.replPrint()
  print(" ")
  p.second.replPrint()
  println(" \(p.back)")
}

var p = BufferedPair(219, State.OR, "Idaho's Portugal", 17)
// CHECK: 219 Oregon "Idaho\'s Portugal" 17
printPair(p)
