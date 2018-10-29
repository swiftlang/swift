// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

struct Foo<T> {
  static var foo: T { return (0 as Int) as! T }
}

extension Foo where T == Int {
  // CHECK: sil_global private [[X_TOKEN:@.*]] : $Builtin.Word
  // CHECK: sil_global hidden [let] @$s4main3FooVAASiRszlE1xSivpZ : $Int
  static let x = foo

  // CHECK: sil_global private [[Y_TOKEN:@.*]] : $Builtin.Word
  // CHECK: sil_global hidden @$s4main3FooVAASiRszlE1ySivpZ : $Int
  static var y = foo
}

print(Foo<Int>.x)
Foo<Int>.y = 2
Foo<Int>.y += 3
print(Foo<Int>.y)

