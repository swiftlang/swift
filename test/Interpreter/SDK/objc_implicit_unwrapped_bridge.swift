// RUN: %target-run-simple-swift | FileCheck %s

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
  println("Converting array of IOU of class type to NSArray...")
  let classArr1 = [ getIUO(X(value: 1)), getIUO(X(value: 2)) ]
  let classNSArr1: NSArray = classArr1
  // CHECK: Class array count = 2
  println("Class array count = \(classNSArr1.count)")
 
  // CHECK: Element 0 has value 1
  // CHECK: Element 1 has value 2
  for (index, obj) in enumerate(classNSArr1) {
    if let x = obj as X {
      println("Element \(index) has value \(x.value)")
    } else {
      println("Element \(index) is not an X")
    }
  }
}

testConvertArrayOfImplicitUnwrappedClass()

// CHECK: Active X objects = 0
println("Active X objects = \(activeXObjects)")

// Bridge an array of an implicitly unwrapped bridged value type.
func testConvertArrayOfImplicitUnwrappedValue() {
  println("Converting array of IOU of String to NSArray...")
  // FIXME: Array literal of these explodes
  var stringArr1: (String!)[] = []
  stringArr1.append(getIUO("Hello"))
  stringArr1.append(getIUO("World"))

  let stringNSArr1: NSArray = stringArr1

  // CHECK: String array count = 2
  println("String array count = \(stringNSArr1.count)")

  // CHECK: Element 0 has value Hello
  // CHECK: Element 1 has value World
  for (index, obj) in enumerate(stringNSArr1) {
    if let str = obj as String {
      println("Element \(index) has value \(str)")
    } else {
      println("Element \(index) is not a String")
    }
  }

  // FIXME: "stringNSArray1[0] as String" also fails
}

testConvertArrayOfImplicitUnwrappedValue()

// Bridge an array of an implicitly unwrapped array value type.
func testConvertArrayOfImplicitUnwrappedArray() {
  println("Converting array of IOU of Arrays of String to NSArray...")
  // FIXME: Array literal of these explodes
  var stringArr1: (String!)[] = []
  stringArr1.append(getIUO("Hello"))
  stringArr1.append(getIUO("World"))

  var stringArr2: (String!)[] = []
  stringArr2.append(getIUO("Welcome"))
  stringArr2.append(getIUO("Swift"))

  var stringArrArr: (((String!)[])!)[] = []
  stringArrArr.append(getIUO(stringArr1))
  stringArrArr.append(getIUO(stringArr2))

  let nsarr: NSArray = stringArrArr

  // CHECK: String array array count = 2
  println("String array array count = \(nsarr.count)")

  // CHECK: Element 0 has value (
  // CHECK:   Hello,
  // CHECK:   World
  // CHECK: )
  // CHECK: Element 1 has value (
  // CHECK:   Welcome,
  // CHECK:   Swift
  // CHECK: )
  for (index, obj) in enumerate(nsarr) {
    if let innerNSArr = obj as NSArray {
      println("Element \(index) has value \(innerNSArr.description!)")
    } else {
      println("Element \(index) is not an NSArray")
    }

    // FIXME: Downcast to (String!)[]
  }
}

testConvertArrayOfImplicitUnwrappedArray()


// FIXME: Negative tests will need their own path.

// CHECK: DONE
println("DONE")
