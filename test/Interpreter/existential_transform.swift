// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -wmo %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

protocol Foo {
  var myName: String { get }
}

struct MyURL {
}

extension MyURL : Foo {
  var myName : String { return "MyURL" }
}

struct MyStruct : Foo {
  var myName : String { return "MyStruct" }
}

@inline(never) func getName(_ f: Foo) -> String {
  return f.myName
}

@inline(never) func getName_wrapper() {
  let u = MyURL()
  // CHECK: MyURL
  print(getName(u))
}
getName_wrapper()

