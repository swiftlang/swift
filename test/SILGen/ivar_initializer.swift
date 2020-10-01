// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk-nosource -I %t) -primary-file %s %S/Inputs/ivar_initializer_other.swift | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

@requires_stored_property_inits public class MyBase : NSObject {}

public class MyMiddle : MyBase {}

public class C {}

public class MyDerived : MyMiddle {
    var c = C()
}

public class OtherDerived : OtherMiddle {
    var c = C()
}

// CHECK-LABEL: sil hidden [ossa] @$s16ivar_initializer9MyDerivedCfeTo : $@convention(objc_method) (@owned MyDerived) -> @owned MyDerived {

// CHECK-LABEL: sil hidden [ossa] @$s16ivar_initializer12OtherDerivedCfeTo : $@convention(objc_method) (@owned OtherDerived) -> @owned OtherDerived {
