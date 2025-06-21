// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/raw_layout_multifile.swift
// RUN: %target-swift-frontend -enable-experimental-feature BuiltinModule -enable-experimental-feature RawLayout -emit-ir -O -Xllvm -sil-disable-pass=deinit-devirtualizer -disable-availability-checking %s %S/Inputs/raw_layout_multifile_b.swift | %FileCheck %t/raw_layout_multifile.swift --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_RawLayout

@_rawLayout(like: Int32)
public struct Foo<T>: ~Copyable {}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}5MyIntVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 4
// stride
// CHECK-SAME:  , {{i64|i32}} 4
// flags: alignment 3, noncopyable, non-bitwise-borrowable, addressable for dependencies
// CHECK-SAME:  , <i32 0x380_0003>
struct MyInt: ~Copyable {
  let x: Int32Fake
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}9BadBufferVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 48
// stride
// CHECK-SAME:  , {{i64|i32}} 48
// flags: alignment 7, noncopyable, non-bitwise-borrowable, addressable for dependencies, is not inline
// CHECK-SAME:  , <i32 0x382_0007>
struct BadBuffer: ~Copyable {
  let buf = SmallVectorOf3<Int64?>()
}

// CHECK-LABEL: @"$s{{[A-Za-z0-9_]*}}5WeirdVWV" = {{.*}} %swift.vwtable
// size
// CHECK-SAME:  , {{i64|i32}} 8
// stride
// CHECK-SAME:  , {{i64|i32}} 8
// flags-32: alignment 7, noncopyable, non-bitwise-borrowable, addressable for dependencies, is not inline
// CHECK-SAME-32:  , <i32 0x3820007>
// flags-64: alignment 7, noncopyable, non-bitwise-borrowable, addressable for dependencies
// CHECK-SAME-64:  , <i32 0x3800007>
struct Weird: ~Copyable {
  let value = UnsafeCell<Int64>()
}

// Force emission of Weird's descriptor to be lazy...
public func something() -> Int64 {
  let x = Weird()
  return x.value.address.pointee
}

// Force emission of BadBuffer's descriptor to be lazy...
public func something2() -> Int64? {
  let buf = BadBuffer()
  return buf.buf.address[1]
}

// Force emission of MyInt's descriptor to be lazy...
public func something3() -> Int32 {
  let x = MyInt(x: Int32Fake())
  return x.x.address.pointee
}
