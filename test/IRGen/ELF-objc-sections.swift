// RUN: %target-swift-frontend -parse-stdlib -enable-objc-interop -disable-objc-attr-requires-foundation-module -I %S/Inputs/usr/include -Xcc --sysroot=/var/empty -emit-ir %s -o - | %FileCheck %s -check-prefix CHECK-ELF

// REQUIRES: OS=linux-gnu

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

// CHECK-ELF-NOT: @"$s4main1CCMf" = {{.*}}, section "__DATA,__objc_data, regular"
// CHECK-ELF: @"\01l_OBJC_LABEL_PROTOCOL_$_P" = {{.*}}, section "objc_protolist"
// CHECK-ELF: @"\01l_OBJC_PROTOCOL_REFERENCE_$_P" = {{.*}}, section "objc_protorefs", align 8
// CHECK-ELF: @"OBJC_CLASS_REF_$_I" = {{.*}}, section "objc_classrefs", align 8
// CHECK-ELF: @"\01L_selector(init)" = {{.*}}, section "objc_selrefs"
// CHECK-ELF: @objc_classes = {{.*}}, section "objc_classlist"
// CHECK-ELF: @objc_categories = {{.*}}, section "objc_catlist"
// CHECK-ELF: @objc_non_lazy_classes = {{.*}}, section "objc_nlclslist", align 8

