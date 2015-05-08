// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

var activeXObjects: Int = 0

class X { 
  var value: Int

  init(value: Int) { 
    self.value = value 

    ++activeXObjects
  }

  deinit {
    --activeXObjects
  }
}

func getIUO<T>(x: T?) -> T! { return x }

// Bridge an array of an implicitly unwrapped class type.
func testConvertArrayOfImplicitUnwrappedClass() {
  print("Converting array of IOU of class type to NSArray...")
  let classArr1 = [ getIUO(X(value: 1)), getIUO(X(value: 2)) ]
  let classNSArr1 = classArr1 as NSArray
  // CHECK: Class array count = 2
  print("Class array count = \(classNSArr1.count)")
 
  // CHECK: Element 0 has value 1
  // CHECK: Element 1 has value 2
  for (index, obj) in classNSArr1.enumerate() {
    if let x = obj as? X {
      print("Element \(index) has value \(x.value)")
    } else {
      print("Element \(index) is not an X")
    }
  }
}

autoreleasepool {
  testConvertArrayOfImplicitUnwrappedClass()
}
// CHECK: Active X objects = 0
print("Active X objects = \(activeXObjects)")

// Bridge an array of an implicitly unwrapped bridged value type.
func testConvertArrayOfImplicitUnwrappedValue() {
  print("Converting array of IOU of String to NSArray...")
  var stringArr1: [String!] = ["Hello", "World"]

  let stringNSArr1 = stringArr1 as NSArray

  // CHECK: String array count = 2
  print("String array count = \(stringNSArr1.count)")

  // CHECK: Element 0 has value Hello
  // CHECK: Element 1 has value World
  for (index, obj) in stringNSArr1.enumerate() {
    if let str = obj as? String {
      print("Element \(index) has value \(str)")
    } else {
      print("Element \(index) is not a String")
    }
  }

  // FIXME: "stringNSArray1[0] as String" also fails
}

testConvertArrayOfImplicitUnwrappedValue()

// Bridge an array of an implicitly unwrapped array value type.
func testConvertArrayOfImplicitUnwrappedArray() {
  print("Converting array of IUO of Arrays of String to NSArray...")
  var stringArr1: [String!] = ["Hello", "World"]
  var stringArr2 = [getIUO("Welcome"), getIUO("Swift")]

  var stringArrArr: [[String!]!] = []
  stringArrArr.append(getIUO(stringArr1))
  stringArrArr.append(getIUO(stringArr2))

  let nsarr = stringArrArr as NSArray

  // CHECK: String array array count = 2
  print("String array array count = \(nsarr.count)")

  // CHECK: Element 0 has value (
  // CHECK:   Hello,
  // CHECK:   World
  // CHECK: )
  // CHECK: Element 1 has value (
  // CHECK:   Welcome,
  // CHECK:   Swift
  // CHECK: )
  for (index, obj) in nsarr.enumerate() {
    if let innerNSArr = obj as? NSArray {
      print("Element \(index) has value \(innerNSArr.description)")
    } else {
      print("Element \(index) is not an NSArray")
    }

    // FIXME: Downcast to [String!]
  }
}

testConvertArrayOfImplicitUnwrappedArray()

// Bridge an NSArray to an array of implicitly unwrapped class type.
func testConvertToArrayOfImplicitUnwrappedClass() {
  print("Converting an NSArray to an array of X!")
  var nsarr = NSMutableArray()
  nsarr.addObject(X(value: 1))
  nsarr.addObject(X(value: 2))

  var arr: [X!] = _convertNSArrayToArray(nsarr)
  
  // CHECK: Class array count = 2
  // CHECK: Element 0 has value X(1)
  // CHECK: Element 1 has value X(2)
  print("Class array count = \(arr.count)")
  for (index, opt) in arr.enumerate() {
    if let x = opt {
      print("Element \(index) has value X(\(x.value))")
    } else {
      print("Element \(index) is empty")
    }
  }
}

testConvertToArrayOfImplicitUnwrappedClass()

// Bridge an NSArray to an array of implicitly unwrapped string type.
func testConvertToArrayOfImplicitUnwrappedString() {
  print("Converting an NSArray to an array of String!")
  var nsarr = NSMutableArray()
  nsarr.addObject(NSString(string: "Hello"))
  nsarr.addObject(NSString(string: "World"))

  var arr: [String!] = _convertNSArrayToArray(nsarr)
  
  // CHECK: String array count = 2
  // CHECK: Element 0 has value Hello
  // CHECK: Element 1 has value World
  print("String array count = \(arr.count)")
  for (index, opt) in arr.enumerate() {
    if let str = opt {
      print("Element \(index) has value \(str)")
    } else {
      print("Element \(index) is empty")
    }
  }
}

testConvertToArrayOfImplicitUnwrappedString()

// FIXME: Negative tests will need their own path.

// CHECK: DONE
print("DONE")
