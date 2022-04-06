// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

public struct SpareBits {
    var o: UInt64 = 0
    var x: UInt8 = 0
    var y: UInt64 = 0
    var x_2: UInt8 = 0
    var y_2: UInt64 = 0
    var x_3: UInt8 = 0
    var y_3: UInt64 = 0
    var x_4: UInt8 = 0
    var y_4: UInt64 = 0
    var x_5: UInt8 = 0
    var y_5: UInt64 = 0
    var x_6: UInt8 = 0
    var y_6: UInt64 = 0
}

public class MyClass {}

public enum Multipayload {
    case a
    case b(MyClass)
    case c(SpareBits)
    case e
    case f
    case g
}

public func testIt(_ e : Multipayload) {
    switch e {
        case .a:
          print(".a (no payload)")
        case .e:
          print(".e (no payload)")
        case .f:
          print(".f (no payload)")
        case .g:
          print(".g (no payload)")
        case .b(let s):
          print(".b(\(s))")
        case .c(let x):
          print(".c(\(x))")
    }
}

func doit() {
  testIt(Multipayload.a)
  testIt(Multipayload.e)
  testIt(Multipayload.f)
  testIt(Multipayload.g)
  testIt(Multipayload.b(MyClass()))
  testIt(Multipayload.c(SpareBits()))
}
doit()

// CHECK: .a (no payload)
// CHECK: .e (no payload)
// CHECK: .f (no payload)
// CHECK: .g (no payload)
// CHECK: .b(main.MyClass)
// CHECK: .c(SpareBits(o: 0, x: 0, y: 0, x_2: 0, y_2: 0, x_3: 0, y_3: 0, x_4: 0, y_4: 0, x_5: 0, y_5: 0, x_6: 0, y_6: 0))
