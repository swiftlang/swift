// RUN: %swift -enable-dynamic-value-type-layout -i %s | FileCheck %s

// TODO: Nongeneric subclasses of generic classes
// TODO: Generic subclasses of generic classes
// TODO: Generic subclasses of nongeneric Swift classes
// TODO: Generic subclasses of ObjC classes

class BufferedPair<T, U> {
  var front: UInt8
  var first: T
  var second: U
  var back: UInt8

  init(front: UInt8, first: T, second: U, back: UInt8) {
    self.front = front
    self.first = first
    self.second = second
    self.back = back
  }
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

func printPair<A: ReplPrintable, B: ReplPrintable>(p:BufferedPair<A,B>) {
  print("\(p.front) ")
  p.first.replPrint()
  print(" ")
  p.second.replPrint()
  println(" \(p.back)")
}

var p = BufferedPair(99, State.OR, "Washington's Mexico", 84)
// CHECK: 99 Oregon "Washington\'s Mexico" 84
printPair(p)
