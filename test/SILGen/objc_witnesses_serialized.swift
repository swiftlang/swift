// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -import-objc-header %S/Inputs/objc_witnesses_serialized.h | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

func useRawRepresentable<T : RawRepresentable>(_: T) {}

// FIXME: getEffectiveClangContext() is broken and checks
// for an explicit '@objc' attribute.

@objc public class Public : NSObject {
  func takesInner(_ value: Inner) {
    useRawRepresentable(value)
  }
}

@objc internal class Internal : NSObject {
  func takesInner(_ value: Inner) {
    useRawRepresentable(value)
  }
}

func takesTopLevel(_ value: TopLevel) {
  useRawRepresentable(value)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$sSo13InternalInnerVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW : $@convention(witness_method: RawRepresentable) (@in UInt32, @thick Public.Inner.Type) -> @out Optional<Public.Inner>
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$sSo13InternalInnerVSYSCSY8rawValue03RawD0QzvgTW : $@convention(witness_method: RawRepresentable) (@in_guaranteed Public.Inner) -> @out UInt32

// CHECK-LABEL: sil shared [transparent] [thunk] [ossa] @$sSo11PublicInnerVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW : $@convention(witness_method: RawRepresentable) (@in UInt32, @thick Internal.Inner.Type) -> @out Optional<Internal.Inner>
// CHECK-LABEL: sil shared [transparent] [thunk] [ossa] @$sSo11PublicInnerVSYSCSY8rawValue03RawD0QzvgTW : $@convention(witness_method: RawRepresentable) (@in_guaranteed Internal.Inner) -> @out UInt32

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$sSo8TopLevelVSYSCSY8rawValuexSg03RawD0Qz_tcfCTW : $@convention(witness_method: RawRepresentable) (@in UInt32, @thick TopLevel.Type) -> @out Optional<TopLevel>
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$sSo8TopLevelVSYSCSY8rawValue03RawD0QzvgTW : $@convention(witness_method: RawRepresentable) (@in_guaranteed TopLevel) -> @out UInt32
