// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/Foundation_bridge.h -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK: 17 bridges to 17
var i = 17
if let obj = _bridgeToObjectiveC(i) {
  print("\(i) bridges to \(obj.description!)")
} else {
  print("\(i) is not bridged to Objective-C")
}

// CHECK: 3.14159 bridges to 3.14159
var d = 3.14159
if let obj = _bridgeToObjectiveC(d) {
  print("\(d) bridges to \(obj.description!)")
} else {
  print("\(d) is not bridged to Objective-C")
}

// CHECK: Hello, world! bridges to Hello, world!
var s = "Hello, world!"
if let obj = _bridgeToObjectiveC(s) {
  print("\(s) bridges to \(obj.description!)")
} else {
  print("\(s) is not bridged to Objective-C")
}

// CHECK: int array bridges to (
// CHECK:     1
// CHECK:     2
// CHECK:     3
// CHECK: )
var a = [1, 2, 3]
if let obj = _bridgeToObjectiveC(a) {
  print("int array bridges to \(obj.description!)")
} else {
  print("int array is not bridged to Objective-C")
}

// CHECK: uint array bridges to (
// CHECK:     1
// CHECK:     2
// CHECK:     3
// CHECK: )
var aui: [UInt] = [1, 2, 3]
if let obj = _bridgeToObjectiveC(aui) {
  print("uint array bridges to \(obj.description!)")
} else {
  print("uint array is not bridged to Objective-C")
}

// CHECK: float array bridges to (
// CHECK:     1.5
// CHECK:     2.5
// CHECK:     3.5
// CHECK: )
var af: [Float] = [1.5, 2.5, 3.5]
if let obj = _bridgeToObjectiveC(af) {
  print("float array bridges to \(obj.description!)")
} else {
  print("float array is not bridged to Objective-C")
}

// CHECK: double array bridges to (
// CHECK:     1.5
// CHECK:     2.5
// CHECK:     3.5
// CHECK: )
var ad = [1.5, 2.5, 3.5]
if let obj = _bridgeToObjectiveC(ad) {
  print("double array bridges to \(obj.description!)")
} else {
  print("double array is not bridged to Objective-C")
}

// CHECK: string array bridges to (
// CHECK:     Hello
// CHECK:     Swift
// CHECK:     World
// CHECK: )
var a2 = ["Hello", "Swift", "World"]
if let obj = _bridgeToObjectiveC(a2) {
  print("string array bridges to \(obj.description!)")
} else {
  print("string array is not bridged to Objective-C")
}

// CHECK: bool array bridges to (
// CHECK:     0
// CHECK:     1
// CHECK:     0
// CHECK: )
var ab = [false, true, false]
if let obj = _bridgeToObjectiveC(ab) {
  print("bool array bridges to \(obj.description!)")
} else {
  print("bool array is not bridged to Objective-C")
}

// CHECK: tuple array is not bridged to Objective-C
var a3 = [(1, 1), (1, 1), (1, 2)]
if let obj = _bridgeToObjectiveC(a3) {
  print("tuple array bridges to \(obj.description!)")
} else {
  print("tuple array is not bridged to Objective-C")
}

// CHECK:      dictionary bridges to {
// CHECK-NEXT:   2 = World;
// CHECK-NEXT:   1 = Hello;
// CHECK-NEXT: }
var dict: Dictionary<NSNumber, NSString> = [1: "Hello", 2: "World"]
if let obj = _bridgeToObjectiveC(dict) {
  print("dictionary bridges to \(obj.description!)")
} else {
  print("dictionary is not bridged to Objective-C")
}

// CHECK:      dictionary bridges to {
// CHECK-NEXT:   2 = World;
// CHECK-NEXT:   1 = Hello;
// CHECK-NEXT: }
var dict2 = [1: "Hello", 2: "World"]
if let obj = _bridgeToObjectiveC(dict2) {
  print("dictionary bridges to \(obj.description!)")
} else {
  print("dictionary is not bridged to Objective-C")
}

// CHECK: dictionary is not bridged to Objective-C
var dict3 = [1: ("Hello", 1), 2: ("World", 2)]
if let obj = _bridgeToObjectiveC(dict3) {
  print("dictionary bridges to \(obj.description!)")
} else {
  print("dictionary is not bridged to Objective-C")
}

// Check dictionary bridging.
var propListStr: NSString = "\"Hello\" = 1;\n\n\"World\" = 2;"
var dict4 = propListStr.propertyListFromStringsFileFormat()!
var hello: NSString = "Hello"
var world: NSString = "World"

// Print out the keys. We only check one of these because the order is
// nondeterministic.
// CHECK: Hello
for key in dict4.keys {
  print(key.description)
}

// CHECK: Hello: 1
print("Hello: \(dict4[hello]!.description!)")
// CHECK: World: 2
print("World: \(dict4[world]!.description!)")

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

