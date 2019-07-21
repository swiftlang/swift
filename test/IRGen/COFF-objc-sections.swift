// RUN: %swift -target x86_64-unknown-windows-msvc -parse-stdlib -enable-objc-interop -disable-objc-attr-requires-foundation-module -I %S/Inputs/usr/include -emit-ir %s -o - | %FileCheck %s -check-prefix CHECK-COFF

// REQUIRES: OS=windows

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

// CHECK-COFF-NOT: @"$s4main1CCMf" = {{.*}}, section "__DATA,__objc_data, regular"
// CHECK-COFF: @"\01l_OBJC_LABEL_PROTOCOL_$_P" = {{.*}}, section ".objc_protolist$B"
// CHECK-COFF: @"\01l_OBJC_PROTOCOL_REFERENCE_$_P" = {{.*}}, section ".objc_protorefs$B"
// CHECK-COFF: @"OBJC_CLASS_REF_$_I" = {{.*}}, section ".objc_classrefs$B"
// CHECK-COFF: @"\01L_selector(init)" = {{.*}}, section ".objc_selrefs$B"
// CHECK-COFF: @objc_classes = {{.*}}, section ".objc_classlist$B"
// CHECK-COFF: @objc_categories = {{.*}}, section ".objc_catlist$B"
// CHECK-COFF: @objc_non_lazy_classes = {{.*}}, section ".objc_nlclslist$B"

