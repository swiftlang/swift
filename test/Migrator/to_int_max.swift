// REQUIRES: objc_interop
// RUN: %target-swift-frontend -typecheck -swift-version 3 %s
// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -update-code -primary-file %s -emit-migrated-file-path %t/to_int_max.result -emit-remap-file-path %t/to_int_max.remap -o /dev/null
// RUN: diff -u %S/to_int_max.swift.expected %t/to_int_max.result
// RUN: %target-swift-frontend -typecheck -swift-version 4 %t/to_int_max.result

let u: UInt = 0
let u8: UInt8 = 0
let u16: UInt16 = 0
let u32: UInt32 = 0
let u64: UInt64 = 0

let i: Int = 0
let i8: Int8 = 0
let i16: Int16 = 0
let i32: Int32 = 0
let i64: Int64 = 0

_ = u.toUIntMax()
_ = u8.toUIntMax()
_ = u16.toUIntMax()
_ = u32.toUIntMax()
_ = u64.toUIntMax()

_ = i.toIntMax()
_ = i8.toIntMax()
_ = i16.toIntMax()
_ = i32.toIntMax()
_ = i64.toIntMax()

func foo<T: UnsignedInteger>(x: T) {
  _ = x.toUIntMax()
}

func foo<T: SignedInteger>(x: T) {
  _ = x.toIntMax()
}
