// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/Foundation_bridge.h -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

// CHECK: 17 bridges to 17
do {
  var i = 17
  let obj = _bridgeAnythingToObjectiveC(i)
  print("\(i) bridges to \(obj.description!)")
}

// CHECK: 3.14159 bridges to 3.14159
do {
  var d = 3.14159
  let obj = _bridgeAnythingToObjectiveC(d)
  print("\(d) bridges to \(obj.description!)")
}

// CHECK: Hello, world! bridges to Hello, world!
do {
  var s = "Hello, world!"
  let obj = _bridgeAnythingToObjectiveC(s)
  print("\(s) bridges to \(obj.description!)")
}

  // CHECK: int array bridges to (
  // CHECK:     1
  // CHECK:     2
  // CHECK:     3
  // CHECK: )
do {
  var a = [1, 2, 3]
  let obj = _bridgeAnythingToObjectiveC(a)
  print("int array bridges to \(obj.description!)")
}

  // CHECK: uint array bridges to (
  // CHECK:     1
  // CHECK:     2
  // CHECK:     3
  // CHECK: )
do {
  var aui: [UInt] = [1, 2, 3]
  let obj = _bridgeAnythingToObjectiveC(aui)
  print("uint array bridges to \(obj.description!)")
}

// CHECK: float array bridges to (
// CHECK:     1.5
// CHECK:     2.5
// CHECK:     3.5
// CHECK: )
do {
  var af: [Float] = [1.5, 2.5, 3.5]
  let obj = _bridgeAnythingToObjectiveC(af)
  print("float array bridges to \(obj.description!)")
}

// CHECK: double array bridges to (
// CHECK:     1.5
// CHECK:     2.5
// CHECK:     3.5
// CHECK: )
do {
  var ad = [1.5, 2.5, 3.5]
  let obj = _bridgeAnythingToObjectiveC(ad)
  print("double array bridges to \(obj.description!)")
}

// CHECK: string array bridges to (
// CHECK:     Hello
// CHECK:     Swift
// CHECK:     World
// CHECK: )
do {
  var a2 = ["Hello", "Swift", "World"]
  let obj = _bridgeAnythingToObjectiveC(a2)
  print("string array bridges to \(obj.description!)")
}

// CHECK: bool array bridges to (
// CHECK:     0
// CHECK:     1
// CHECK:     0
// CHECK: )
do {
  var ab = [false, true, false]
  let obj = _bridgeAnythingToObjectiveC(ab)
  print("bool array bridges to \(obj.description!)")
}

// CHECK: tuple array bridges to (
// CHECK:   (1, 1)
// CHECK:   (1, 1)
// CHECK:   (1, 2)
// CHECK: )
do {
  var a3 = [(1, 1), (1, 1), (1, 2)]
  let obj = _bridgeAnythingToObjectiveC(a3)
  print("tuple array bridges to \(obj.description!)")
}

// CHECK:      dictionary bridges to {
// CHECK-DAG:   1 = Hello;
// CHECK-DAG:   2 = World;
// CHECK: }
do {
  var dict: Dictionary<NSNumber, NSString> = [1: "Hello", 2: "World"]
  let obj = _bridgeAnythingToObjectiveC(dict)
  print("dictionary bridges to \(obj.description!)")
}

// CHECK:      dictionary bridges to {
// CHECK-DAG:   1 = Hello;
// CHECK-DAG:   2 = World;
// CHECK: }
do {
  var dict2 = [1: "Hello", 2: "World"]
  let obj = _bridgeAnythingToObjectiveC(dict2)
  print("dictionary bridges to \(obj.description!)")
}

// CHECK: dictionary bridges to {
// CHECK-DAG:   1 = "(\"Hello\", 1)";
// CHECK-DAG:   2 = "(\"World\", 2)";
// CHECK: }
do {
  var dict3 = [1: ("Hello", 1), 2: ("World", 2)]
  let obj = _bridgeAnythingToObjectiveC(dict3)
  print("dictionary bridges to \(obj)")
}

// Check dictionary bridging.
var propListStr: NSString = "\"Hello\" = 1;\n\n\"World\" = 2;"
var dict4 = propListStr.propertyListFromStringsFileFormat()!
var hello: NSString = "Hello"
var world: NSString = "World"

// Print out the keys.
// CHECK-DAG: Bridged key: Hello
// CHECK-DAG: Bridged key: World
for key in dict4.keys {
  print("Bridged key: \(key.description)")
}

// CHECK: Hello: 1
print("Hello: \(dict4[hello]!)")
// CHECK: World: 2
print("World: \(dict4[world]!)")

// <rdar://problem/17035548> bridging array of blocks.
class Foo: NSObject {
    func foo() { print("Foo.foo()") }
    lazy var closures: [(@convention(block) () -> Void)] = [self.foo]
    func invoke() {
        closures[0]()
    }
}

// CHECK: Foo.foo()
Foo().invoke()

// <rdar://problem/19734621> Dealing with APIs that have been updated not to return nil in newer SDKs
// CHECK: getNullable: nil
print("getNullable: \(getNullable())")
// CHECK: getNonnull: []
print("getNonnull: \(getNonnull())")

// CHECK: final
print("final")

