// RUN: %target-swift-frontend %s -O -Xllvm -sil-print-types -emit-sil -parse-as-library | %FileCheck %s

// is_same_metatype builtin is no longer used due to rdar://145707064 (Builtin.is_same_metatype should support noncopyable/nonescapable types)
// XFAIL: rdar145707064

protocol SomeP {}

public enum SpecialEnum : SomeP {}

// CHECK-LABEL: sil shared [noinline] @$s20existential_metatype17checkProtocolType0aE0Sbxm_tAA5SomePRzlFAA11SpecialEnumO_Ttg5 : $@convention(thin) () -> Bool {
// CHECK:       bb0:
// CHECK-NEXT:   debug_value undef : $@thick SpecialEnum.Type, let, name "existentialType", argno 1
// CHECK-NEXT:   %1 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT:   %2 = struct $Bool (%1 : $Builtin.Int1)
// CHECK-NEXT:   return %2 : $Bool
// CHECK-LABEL: } // end sil function '$s20existential_metatype17checkProtocolType0aE0Sbxm_tAA5SomePRzlFAA11SpecialEnumO_Ttg5'
@inline(never)
func checkProtocolType<P : SomeP>(existentialType: P.Type) -> Bool {
  return existentialType == SpecialEnum.self
}

public func testProtocolType() -> Bool {
  return checkProtocolType(existentialType: SpecialEnum.self)
}
