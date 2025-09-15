// RUN: %sourcekitd-test -req=expand-placeholder %s | %FileCheck %s

// FIXME: Make it accept '-debug-forbid-typecheck-prefix' and ensure no typecheck happens.'

protocol MyProto {}

enum MyEnum: Hashable, MyProto {
  case foo
  case bar
}

func test(array: [MyEnum]) -> [NyEnum] {
  array.filter(<#T##isIncluded: (MyEnum) throws -> Bool##(MyEnum) throws -> Bool#>)
// CHECK: array.filter { <#MyEnum#> in 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
}
