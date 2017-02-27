// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

struct Foo<T> {
  static var foo: T { return (0 as Int) as! T }
}

extension Foo where T == Int {
  // CHECK: sil_global private [[X_TOKEN:@.*]] : $Builtin.Word
  // CHECK: sil_global hidden [let] @_TZve4mainRxzSirVS_3Foo1xSi : $Int
  static let x = foo

  // CHECK: sil_global private [[Y_TOKEN:@.*]] : $Builtin.Word
  // CHECK: sil_global hidden @_TZve4mainRxzSirVS_3Foo1ySi : $Int
  static var y = foo
}

print(Foo<Int>.x)
Foo<Int>.y = 2
Foo<Int>.y += 3
print(Foo<Int>.y)

