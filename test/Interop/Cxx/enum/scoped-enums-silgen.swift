// RUN: %target-swift-emit-sil -Xllvm -sil-print-types %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import ScopedEnums

// CHECK-LABEL: sil @$s4main24returnsScopedEnumDefinedSo0cdE0VyF : $@convention(thin) () -> ScopedEnumDefined
// CHECK: [[OUT:%.*]] = enum $ScopedEnumDefined, #ScopedEnumDefined.x!enumelt
// CHECK: return [[OUT]] : $ScopedEnumDefined
// CHECK-LABEL: end sil function '$s4main24returnsScopedEnumDefinedSo0cdE0VyF'
public func returnsScopedEnumDefined() -> ScopedEnumDefined {
  return .x
}

// CHECK-LABEL: sil @$s4main22returnsScopedEnumBasicSo0cdE0VyF : $@convention(thin) () -> ScopedEnumBasic
// CHECK: [[OUT:%.*]] = enum $ScopedEnumBasic, #ScopedEnumBasic.x!enumelt
// CHECK: return [[OUT]] : $ScopedEnumBasic
// CHECK-LABEL: end sil function '$s4main22returnsScopedEnumBasicSo0cdE0VyF'
public func returnsScopedEnumBasic() -> ScopedEnumBasic {
  return .x
}

// CHECK-LABEL: sil @$s4main28returnsScopedEnumCharDefinedSo0cdeF0VyF : $@convention(thin) () -> ScopedEnumCharDefined
// CHECK: [[OUT:%.*]] = enum $ScopedEnumCharDefined, #ScopedEnumCharDefined.x!enumelt
// CHECK: return [[OUT]] : $ScopedEnumCharDefined
// CHECK-LABEL: end sil function '$s4main28returnsScopedEnumCharDefinedSo0cdeF0VyF'
public func returnsScopedEnumCharDefined() -> ScopedEnumCharDefined {
  return .x
}

