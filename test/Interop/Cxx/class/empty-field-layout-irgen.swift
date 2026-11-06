// RUN: %target-swiftxx-frontend -emit-ir -I %S/Inputs -module-name main %s | %FileCheck %s

import EmptyFieldLayout

public func readEmptyThenInt() -> Int32 {
  let v = makeEmptyThenInt(2026)
  return v.i
}

// CHECK-LABEL: define {{.*}} @"$s4main16readEmptyThenInts5Int32VyF"
// CHECK: %[[BUF:clang\.record\.return\.coerced]] = alloca
// CHECK: %[[CALL:[0-9]+]] = {{(invoke|call)}} i64 @{{.*}}makeEmptyThenInt
// CHECK: store i64 %[[CALL]], ptr %[[BUF]]
// CHECK: getelementptr inbounds {{.*}} %[[BUF]],
// CHECK: load i32
// CHECK-NOT: load i32, ptr %{{[^,]*}}.coercion.coerced

public func readNonTrivial() -> Int32 {
  let v = makeEmptyAndIntNonTrivial(2026)
  return v.i
}

// A struct with a non-trivial destructor is address-only, 
// so the C ABI returns it via `sret` — no `clang.record.return.coerced` alloca.

// CHECK-LABEL: define {{.*}} @"$s4main14readNonTrivials5Int32VyF"
// CHECK: alloca %TSo21EmptyAndIntNonTrivialV
// CHECK: {{(invoke|call)}} void @{{.*}}makeEmptyAndIntNonTrivial{{.*}}(ptr {{.*}}sret(%TSo21EmptyAndIntNonTrivialV)
// CHECK-NOT: clang.record.return.coerced = alloca
