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

struct MyMask : OptionSetType {
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

struct MyEventMask2 : OptionSetType {
  init(rawValue: UInt64) {}
  var rawValue: UInt64 { return 0 }
}
func sendIt(_: MyEventMask2) {}
func testMask1(a: Int) {
  sendIt(a)
}
func testMask2(a: UInt64) {
  sendIt(a)
}

func foo() -> Int {
  do {
  } catch var err {
    goo(err)
  }
}

func goo(var e : ErrorType) {}

struct Test1 : RawOptionSetType {
  init(rawValue: Int) {}
  var rawValue: Int { return 0 }
}

print("", false)

func ftest1() {
  // Don't replace the variable name with '_'
  let myvar = 0
}

func testPlusPlus() {
  var x = 1
  x++
  --x
  var y = "".startIndex
  y++
  --y
}

@available(*, deprecated, renamed="braveNewWorld")
func ancientPast() {
}
func deprecated() {
  _ = ancientPast()
}
