// RUN: %target-swift-frontend -disable-objc-interop -emit-ir %s | %FileCheck %s

// @c enums use the same representation and value witnesses as @objc enums:
// the builtin integer witness table, with no extra tag witnesses.

@c public enum CEnum: Int8 { case a, b, c }

// CHECK: @"$s6c_enum5CEnumOMf" = {{.*}} ptr @"$sBi8_WV"
// CHECK-NOT: swift.enum_vwtable
public func metatype() -> Any.Type { CEnum.self }

// CHECK-LABEL: define {{.*}}swiftcc i64 @"$s6c_enum6switchySiAA5CEnumOF"(i8 %0)
// CHECK: _diagnoseUnexpectedEnumCaseValue
public func `switch`(_ e: CEnum) -> Int {
  switch e {
  case .a: return 0
  case .b: return 1
  case .c: return 2
  }
}
