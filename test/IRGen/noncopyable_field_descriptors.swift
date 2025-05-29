// RUN: %empty-directory(%t)
// RUN: %swift-frontend -emit-ir -o - %s -module-name test \
// RUN:   -parse-as-library \
// RUN:   -enable-library-evolution \
// RUN:   -target %target-cpu-apple-macosx15 \
// RUN:   > %t/test_new.irgen

// RUN: %swift-frontend -emit-ir -o - %s -module-name test \
// RUN:   -parse-as-library \
// RUN:   -enable-library-evolution \
// RUN:   -target %target-cpu-apple-macosx14 \
// RUN:   > %t/test_old.irgen

// RUN: %FileCheck --check-prefix=NEW %s < %t/test_new.irgen
// RUN: %FileCheck --check-prefix=OLD %s < %t/test_old.irgen

// rdar://124401253
// REQUIRES: OS=macosx
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

// HINT: when debugging this test, you can look for an `i32 2` field in the
// 'MF' constant as a separator that precedes each field descriptor.

// NEW: @"$s4test8CC_TestsCMF" =
// NEW-SAME: @"get_type_metadata Ri_zr0_l4test21ConditionallyCopyableOyxG.3"
// NEW-SAME: @"symbolic _____yq_G 4test21ConditionallyCopyableOAARi_zrlE"
// NEW-SAME: @"get_type_metadata Ri_zr0_l4test21ConditionallyCopyableOyAA03NonC0VG.4"
// NEW-SAME: @"symbolic _____ySSG 4test21ConditionallyCopyableOAARi_zrlE"

// OLD: @"$s4test8CC_TestsCMF" =
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test21ConditionallyCopyableOyxG.3"
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test21ConditionallyCopyableOyq_G.4"
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test21ConditionallyCopyableOyAA03NonC0VG.5"
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test21ConditionallyCopyableOySSG.6"
public class CC_Tests<NCG: ~Copyable, T> {
  var ccNCG: ConditionallyCopyable<NCG> = .none
  var ccT: ConditionallyCopyable<T> = .none
  var ccNC: ConditionallyCopyable<NonCopyable> = .none
  var ccC: ConditionallyCopyable<String> = .none
}


/// For the "never copyable" fields, we expect to always go through the
/// type metadata accessor strategy, which is designed to hide these
/// fields until a future runtime says they're safe to reflect.

// NEW: @"$s4test8NC_TestsCMF" =
// NEW-SAME: @"get_type_metadata Ri_zr0_l4test13NeverCopyableOyxG.5"
// NEW-SAME: @"get_type_metadata Ri_zr0_l4test13NeverCopyableOyq_G.6"
// NEW-SAME: @"get_type_metadata Ri_zr0_l4test13NeverCopyableOyAA03NonC0VG.7"
// NEW-SAME: @"get_type_metadata Ri_zr0_l4test13NeverCopyableOySSG.8"

// OLD: @"$s4test8NC_TestsCMF" =
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test13NeverCopyableOyxG.7"
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test13NeverCopyableOyq_G.8"
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test13NeverCopyableOyAA03NonC0VG.9"
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test13NeverCopyableOySSG.10"
public class NC_Tests<NCG: ~Copyable, T> {
  var ncNCG: NeverCopyable<NCG> = .none
  var ncT: NeverCopyable<T> = .none
  var ncNC: NeverCopyable<NonCopyable> = .none
  var ncC: NeverCopyable<String> = .none
}


// NEW: @"$s4test17StdlibTypes_TestsCMF" =
// NEW-SAME: @"symbolic xSg"
// NEW-SAME: @"symbolic q_Sg"
// NEW-SAME: @"get_type_metadata Ri_zr0_l4test11NonCopyableVSg.9"
// NEW-SAME: @"symbolic SSSg"
// NEW-SAME: @"symbolic SPyxG"
// NEW-SAME: @"symbolic SPyq_G"
// NEW-SAME: @"symbolic SPy_____G 4test11NonCopyableV"
// NEW-SAME: @"symbolic SPySSG"

// OLD: @"$s4test17StdlibTypes_TestsCMF" =
// OLD-SAME: @"symbolic xSg"
// OLD-SAME: @"symbolic q_Sg"
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test11NonCopyableVSg.11"
// OLD-SAME: @"symbolic SSSg"
// OLD-SAME: @"symbolic SPyxG"
// OLD-SAME: @"symbolic SPyq_G"
// OLD-SAME: @"symbolic SPy_____G 4test11NonCopyableV"
// OLD-SAME: @"symbolic SPySSG"
public class StdlibTypes_Tests<NCG: ~Copyable, T> {
  var optNCG: Optional<NCG> = .none
  var optT: Optional<T> = .none
  var optNC: Optional<NonCopyable> = .none
  var optC: Optional<String> = .none

  var upNCG: UnsafePointer<NCG> = .init(bitPattern: 16)!
  var upT: UnsafePointer<T> = .init(bitPattern: 32)!
  var upNC: UnsafePointer<NonCopyable> = .init(bitPattern: 64)!
  var upC: UnsafePointer<String> = .init(bitPattern: 128)!
}


// NEW: @"$s4test19PlainlyStored_TestsCMF" =
// NEW-SAME: @"symbolic x"
// NEW-SAME: @"symbolic q_"
// NEW-SAME: @"get_type_metadata Ri_zr0_l4test11NonCopyableV.10"
// NEW-SAME: @"symbolic SS"

// OLD: @"$s4test19PlainlyStored_TestsCMF" =
// OLD-SAME: @"symbolic x"
// OLD-SAME: @"symbolic q_"
// OLD-SAME: @"get_type_metadata Ri_zr0_l4test11NonCopyableV.12"
// OLD-SAME: @"symbolic SS"
public class PlainlyStored_Tests<NCG: ~Copyable, T> {
  var ncg: NCG
  var t: T
  var concreteNC: NonCopyable
  var str: String

  public init(_ ncg: consuming NCG, _ t: T) {
    self.ncg = ncg
    self.t = t
    self.concreteNC = NonCopyable()
    self.str = ""
  }
}
