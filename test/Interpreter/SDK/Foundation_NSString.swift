// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -i %s | FileCheck %s
// REQUIRES: sdk

import Foundation

var hello : NSString = "Hello, world!"
// CHECK: Hello, world!
println(hello)

var upperHello = hello.uppercaseString()
// CHECK: HELLO, WORLD!
println(upperHello)

// Note: easy way to create an NSDictionary
var strings : NSString = "\"A\" = \"Foo\";\n\"B\" = \"Bar\";\n"
var dict  = strings.propertyListFromStringsFileFormat()

// Subscripting an NSDictionary. FIXME: The inner NSString casts are annoying.
// CHECK: A -> Foo
println("A -> " + (dict[NSString("A")] as! NSString))
// CHECK: B -> Bar
println("B -> " + (dict[NSString("B")] as! NSString))

// Creating and subscripting an NSMutableArray
var array = new NSMutableArray(2)
hello = "Hello"
array[0] = hello
array[1] = NSString("world")

// FIXME: NSString string interpolation doesn't work due to lack of
// overload resolution.
// CHECK: Hello, world!
print(array[0] as! NSString)
print(", ");
print(array[1] as! NSString)
println("!")

// Selectors
assert(NSString.instancesRespondToSelector("init"))
assert(!NSString.instancesRespondToSelector("wobble"))
