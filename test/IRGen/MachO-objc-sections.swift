// RUN: %swift -target arm64-apple-ios8.0 -parse-stdlib -enable-objc-interop -disable-objc-attr-requires-foundation-module -I %S/Inputs/usr/include -emit-ir %s -o - | %FileCheck %s -check-prefix CHECK-MACHO

// REQUIRES: OS=ios

import ObjCInterop

@objc
class C {
}

extension C : P {
  public func method() {
    f(I())
  }
}

@_objc_non_lazy_realization
class D {
}

// CHECK-MACHO: @"$s4main1CCMf" = {{.*}}, section "__DATA,__objc_data, regular"
// CHECK-MACHO: @"\01l_OBJC_LABEL_PROTOCOL_$_P" = {{.*}}, section "__DATA,__objc_protolist,coalesced,no_dead_strip"
// CHECK-MACHO: @"\01l_OBJC_PROTOCOL_REFERENCE_$_P" = {{.*}}, section "__DATA,__objc_protorefs,coalesced,no_dead_strip"
// CHECK-MACHO: @"\01l_OBJC_CLASS_REF_$_I" = {{.*}}, section "__DATA,__objc_classrefs,regular,no_dead_strip"
// CHECK-MACHO: @"\01L_selector(init)" = {{.*}}, section "__DATA,__objc_selrefs,literal_pointers,no_dead_strip"
// CHECK-MACHO: @objc_classes = {{.*}}, section "__DATA,__objc_classlist,regular,no_dead_strip"
// CHECK-MACHO: @objc_categories = {{.*}}, section "__DATA,__objc_catlist,regular,no_dead_strip"
// CHECK-MACHO: @objc_non_lazy_classes = {{.*}}, section "__DATA,__objc_nlclslist,regular,no_dead_strip"

