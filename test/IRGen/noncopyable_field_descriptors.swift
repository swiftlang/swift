// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o - %s -module-name test \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -parse-as-library \
// RUN:   -enable-library-evolution \
// RUN:   > %t/test.irgen

// RUN: %FileCheck %s < %t/test.irgen

// rdar://124401253
// REQUIRES: OS=macosx || OS=linux || OS=windows-msvc
// UNSUPPORTED: CPU=arm64e

@frozen
public enum ConditionallyCopyable<Wrapped: ~Copyable>: ~Copyable {
  case none
  case some(Wrapped)
}

extension ConditionallyCopyable: Copyable where Wrapped: Copyable { }

@frozen
public enum NeverCopyable<Wrapped: ~Copyable>: ~Copyable {
  case none
  case some(Wrapped)
}

@frozen
public struct NonCopyable: ~Copyable { }

// CHECK: @"$s4test1CCMF" = 
// CHECK-SAME: @"symbolic _____yxG 4test21ConditionallyCopyableOAARi_zrlE"
// CHECK-SAME: @"get_type_metadata Ri_zl4test21ConditionallyCopyableOyAA03NonC0VG.3"
// CHECK-SAME: @"symbolic _____yxG 4test21ConditionallyCopyableOAARi_zrlE"
// CHECK-SAME: @"get_type_metadata Ri_zl4test21ConditionallyCopyableOyAA03NonC0VG.3"
public class C<T: ~Copyable> {
  var ccT: ConditionallyCopyable<T> = .none
  var ccNC: ConditionallyCopyable<NonCopyable> = .none
  var ncT: ConditionallyCopyable<T> = .none
  var ncNC: ConditionallyCopyable<NonCopyable> = .none
}
