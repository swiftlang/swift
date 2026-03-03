// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/existential-bitwise-borrowability.swift
// RUN: %target-swift-frontend -enable-experimental-feature RawLayout -emit-ir -disable-availability-checking -I %S/Inputs -cxx-interoperability-mode=upcoming-swift %t/existential-bitwise-borrowability.swift | %FileCheck %t/existential-bitwise-borrowability.swift --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: swift_feature_RawLayout

// Copyable existentials are bitwise-borrowable (because copyable types are
// always bitwise-borrowable if they're bitwise-takable, and only bitwise-takable
// values are stored inline in existentials). Noncopyable existentials are
// not (since types like Atomic and Mutex can be stored inline in their
// buffers).

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}3FooVWV" = {{.*}} %swift.vwtable
// size
// CHECK-64-SAME:  , {{i64|i32}} 32
// CHECK-32-SAME:  , {{i64|i32}} 16
// stride
// CHECK-64-SAME:  , {{i64|i32}} 32
// CHECK-32-SAME:  , {{i64|i32}} 16
// flags: word alignment, noncopyable, non-POD, non-inline-storage
// CHECK-64-SAME:  , <i32 0x830007>
// CHECK-32-SAME:  , <i32 0x830003>
struct Foo: ~Copyable {
    var x: Any
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}3BarVWV" = {{.*}} %swift.vwtable
// size
// CHECK-64-SAME:  , {{i64|i32}} 32
// CHECK-32-SAME:  , {{i64|i32}} 16
// stride
// CHECK-64-SAME:  , {{i64|i32}} 32
// CHECK-32-SAME:  , {{i64|i32}} 16
// flags: word alignment, non-bitwise-borrowable, noncopyable, non-POD, non-inline-storage
// CHECK-64-SAME:  , <i32 0x1830007>
// CHECK-32-SAME:  , <i32 0x1830003>
struct Bar: ~Copyable {
    var x: any ~Copyable
}

