// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift -Xfrontend -objc-bridge-dictionary %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

import Foundation

// CHECK: 17 bridges to 17
var i = 17
if let obj: AnyObject = bridgeToObjectiveC(i) {
#if os(OSX)
  println("\(i) bridges to \(obj.description!)")
#else
  println("\(i) bridges to \(obj.description!())")
#endif
} else {
  println("\(i) is not bridged to Objective-C")
}

// CHECK: 3.14159 bridges to 3.14159
var d = 3.14159
if let obj: AnyObject = bridgeToObjectiveC(d) {
#if os(OSX)
  println("\(d) bridges to \(obj.description!)")
#else
  println("\(d) bridges to \(obj.description!())")
#endif
} else {
  println("\(d) is not bridged to Objective-C")
}

// CHECK: Hello, world! bridges to Hello, world!
var s = "Hello, world!"
if let obj: AnyObject = bridgeToObjectiveC(s) {
#if os(OSX)
  println("\(s) bridges to \(obj.description!)")
#else
  println("\(s) bridges to \(obj.description!())")
#endif
} else {
  println("\(s) is not bridged to Objective-C")
}

// CHECK: int array bridges to (
// CHECK:     1
// CHECK:     2
// CHECK:     3
// CHECK: )
var a = [1, 2, 3]
if let obj: AnyObject = bridgeToObjectiveC(a) {
#if os(OSX)
  println("int array bridges to \(obj.description!)")
#else
  println("int array bridges to \(obj.description!())")
#endif
} else {
  println("int array is not bridged to Objective-C")
}

// CHECK: uint array bridges to (
// CHECK:     1
// CHECK:     2
// CHECK:     3
// CHECK: )
var aui: UInt[] = [1, 2, 3]
if let obj: AnyObject = bridgeToObjectiveC(aui) {
#if os(OSX)
  println("uint array bridges to \(obj.description!)")
#else
  println("uint array bridges to \(obj.description!())")
#endif
} else {
  println("uint array is not bridged to Objective-C")
}

// CHECK: float array bridges to (
// CHECK:     1.5
// CHECK:     2.5
// CHECK:     3.5
// CHECK: )
var af: Float[] = [1.5, 2.5, 3.5]
if let obj: AnyObject = bridgeToObjectiveC(af) {
#if os(OSX)
  println("float array bridges to \(obj.description!)")
#else
  println("float array bridges to \(obj.description!())")
#endif
} else {
  println("float array is not bridged to Objective-C")
}

// CHECK: double array bridges to (
// CHECK:     1.5
// CHECK:     2.5
// CHECK:     3.5
// CHECK: )
var ad = [1.5, 2.5, 3.5]
if let obj: AnyObject = bridgeToObjectiveC(ad) {
#if os(OSX)
  println("double array bridges to \(obj.description!)")
#else
  println("double array bridges to \(obj.description!())")
#endif
} else {
  println("double array is not bridged to Objective-C")
}

// CHECK: string array bridges to (
// CHECK:     Hello
// CHECK:     Swift
// CHECK:     World
// CHECK: )
var a2 = ["Hello", "Swift", "World"]
if let obj: AnyObject = bridgeToObjectiveC(a2) {
#if os(OSX)
  println("string array bridges to \(obj.description!)")
#else
  println("string array bridges to \(obj.description!())")
#endif
} else {
  println("string array is not bridged to Objective-C")
}

// CHECK: bool array bridges to (
// CHECK:     0
// CHECK:     1
// CHECK:     0
// CHECK: )
var ab = [false, true, false]
if let obj: AnyObject = bridgeToObjectiveC(ab) {
#if os(OSX)
  println("bool array bridges to \(obj.description!)")
#else
  println("bool array bridges to \(obj.description!())")
#endif
} else {
  println("bool array is not bridged to Objective-C")
}

// CHECK: tuple array is not bridged to Objective-C
var a3 = [(1, 1), (1, 1), (1, 2)]
if let obj: AnyObject = bridgeToObjectiveC(a3) {
#if os(OSX)
  println("tuple array bridges to \(obj.description!)")
#else
  println("tuple array bridges to \(obj.description!())")
#endif
} else {
  println("tuple array is not bridged to Objective-C")
}

// CHECK:      dictionary bridges to {
// CHECK-NEXT:   1 = Hello;
// CHECK-NEXT:   2 = World;
// CHECK-NEXT: }
var dict: Dictionary<NSNumber, NSString> = [1: "Hello", 2: "World"]
if let obj: AnyObject = bridgeToObjectiveC(dict) {
#if os(OSX)
  println("dictionary bridges to \(obj.description!)")
#else
  println("dictionary bridges to \(obj.description!())")
#endif
} else {
  println("dictionary is not bridged to Objective-C")
}

// CHECK: dictionary is not bridged to Objective-C
var dict2 = [1: "Hello", 2: "World"]
if let obj: AnyObject = bridgeToObjectiveC(dict2) {
  // FIXME: Printing \(obj.description!()) here 
#if os(OSX)
  println("dictionary bridges to \(obj.description!)")
#else
  println("dictionary bridges to \(obj.description!())")
#endif
} else {
  println("dictionary is not bridged to Objective-C")
}

// Check dictionary bridging.
var propListStr: NSString = "\"Hello\" = 1;\n\n\"World\" = 2;"
var dict3 = propListStr.propertyListFromStringsFileFormat()
var hello = "Hello"
var world = "World"

// Print out the keys. We only check one of these because the order is
// nondeterministic.
// CHECK: Hello
for key in dict3.keys {
  println(key.description)
}

// CHECK: Hello: 1
println("Hello: \(dict3[hello]!.description!)")
// CHECK: World: 2
println("World: \(dict3[world]!.description!)")



// CHECK: final
println("final")

