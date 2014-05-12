// RUN: %target-run-simple-swift | FileCheck %s

//===----------------------------------------------------------------------===//
// String Splits
//===----------------------------------------------------------------------===//
if true {
  var (before, after, found) = "foo.bar".splitFirst(".")
  assert(found)
  assert(before == "foo")
  assert(after == "bar")
}

// CHECK: OKAY
println("OKAY")

//===----------------------------------------------------------------------===//
// String uppercase/lowercase
//===----------------------------------------------------------------------===//
if true {
  // CHECK-NEXT: {{^}}FOOBAR.WIBBLE{{$}}
  println("FooBar.Wibble".uppercase)
  // CHECK-NEXT: {{^}}foobar.wibble{{$}}
  println("FooBar.Wibble".lowercase)
}
