// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-build-swift -module-name MangleTest %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

/* FIXME: SwiftObject doesn't support -description
class Foo { }
var anyFoo: AnyObject = Foo()
print(anyFoo.description())

@objc class Bar { }
var anyBar: AnyObject = Bar()
print(anyBar.description())
*/

func checkClassName(cls: AnyClass, _ name: String, _ mangled: String)
{
  // Class's name should appear unmangled.
  assert(NSStringFromClass(cls) == name)
  assert(NSStringFromClass(object_getClass(cls)) == name)

  // Look up by unmangled name should work.
  // Look up by mangled name should also work.
  for query in [name, mangled] {
    let cls2 = NSClassFromString(query)
    assert(cls === cls2)
    assert(object_getClass(cls) === object_getClass(cls2))
  }
}

func checkProtocolName(proto: Protocol, _ name: String, _ mangled: String)
{
  // Protocol's name should appear unmangled.
  assert(NSStringFromProtocol(proto) == name)

  // Look up by unmangled name should work.
  // Look up by mangled name should also work.
  for query in [name, mangled] {
    let proto2 = NSProtocolFromString(query)
    assert(protocol_isEqual(proto, proto2))
  }
}

func checkIvarName(cls: AnyClass, _ name: String)
{
  assert(name == String.fromCString(ivar_getName(class_getInstanceVariable(cls, name))))
}


@objc class Wibble : NSObject { }
checkClassName(Wibble.self, "MangleTest.Wibble", "_TtC10MangleTest6Wibble")

// Check whether the class name comes out properly in the instance description
var anyWibble: AnyObject = Wibble()
print(anyWibble.description)
// CHECK: MangleTest.Wibble


// ObjC metadata is not punycoded.

@objc protocol RadicalHeart⺖ { }
checkProtocolName(RadicalHeart⺖.self, "MangleTest.RadicalHeart⺖", "_TtP10MangleTest15RadicalHeart⺖_")

@objc class RadicalSheep⽺ : NSObject, RadicalHeart⺖ {
  var ⽺x: Int = 0
}
checkClassName(RadicalSheep⽺.self, 
  "MangleTest.RadicalSheep⽺", "_TtC10MangleTest15RadicalSheep⽺")
checkIvarName(RadicalSheep⽺.self, "⽺x")
