// RUN: %target-swift-frontend -emit-silgen-ossa -o /dev/null -sil-verify-all -enable-sil-opaque-values %S/Inputs/enum_debuginfo_other.swift -primary-file %s -module-name enum_debuginfo -g -Xllvm -sil-print-debuginfo
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/enum_debuginfo_other.swift -primary-file %s -module-name enum_debuginfo -g -Xllvm -sil-print-debuginfo | %FileCheck %s

public func makeEnum() -> MyEnum {
  return .hasPayload(argument: 123)
}

// CHECK-NOT: enum_debuginfo_other.swift":{{[0-9]}}
