// RUN: not %swift -parse -target %target-triple %s -emit-fixits-path %t.remap
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

class Base {}
class Derived : Base {}

var b : Base
b as Derived
b as Derived

b as! Base

var opti : Int?
// Add bang.
var i : Int = opti
// But remove unnecessary bang.
var i2 : Int = i!

struct MyMask : OptionSet {
  init(_ rawValue: UInt) {}
  init(rawValue: UInt) {}
  init(nilLiteral: ()) {}

  var rawValue: UInt { return 0 }

  static var allZeros: MyMask { return MyMask(0) }
  static var Bingo: MyMask { return MyMask(1) }
}

func supported() -> MyMask {
  return Int(MyMask.Bingo.rawValue)
}

func goo(var e : ErrorProtocol) {
}
func goo2(var e: ErrorProtocol) {}
func goo3(var e: Int) { e = 3 }
protocol A {
  func bar(var s: Int)
}
extension A {
  func bar(var s: Int) {
    s += 5
  }
}

func baz(var x: Int) {
  x += 10
}
func foo(let y: String, inout x: Int) {
  
}

struct Test1 : OptionSet {
  init(rawValue: Int) {}
  var rawValue: Int { return 0 }
}

print("", false)

func ftest1() {
  // Don't replace the variable name with '_'
  let myvar = 0
}

func ftest2(x x: Int -> Int) {}
