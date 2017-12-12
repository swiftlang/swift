// RUN: %swift -target x86_64-unknown-windows-msvc -parse-stdlib -enable-objc-interop -disable-objc-attr-requires-foundation-module -I %S/Inputs/usr/include -emit-ir %s -o - | %FileCheck %s -check-prefix CHECK-COFF
// RUN: %swift -target x86_64-unknown-linux-gnu -parse-stdlib -enable-objc-interop -disable-objc-attr-requires-foundation-module -I %S/Inputs/usr/include -Xcc --sysroot=/var/empty -emit-ir %s -o - | %FileCheck %s -check-prefix CHECK-ELF
// RUN: %swift -target x86_64-apple-ios8.0 -parse-stdlib -enable-objc-interop -disable-objc-attr-requires-foundation-module -I %S/Inputs/usr/include -emit-ir %s -o - | %FileCheck %s -check-prefix CHECK-MACHO

import ObjCInterop

@objc
class C {
}

extension C : P {
  public func method() {
    f(I())
  }
}

@objc_non_lazy_realization
class D {
}

// CHECK-COFF-NOT: @_T04main1CCMf = {{.*}}, section "__DATA,__objc_data, regular"
// CHECK-COFF: @"\01l_OBJC_LABEL_PROTOCOL_$_P" = {{.*}}, section ".objc_protolist$B"
// CHECK-COFF: @"\01l_OBJC_PROTOCOL_REFERENCE_$_P" = {{.*}}, section ".objc_protorefs$B"
// CHECK-COFF: @"OBJC_CLASS_REF_$_I" = {{.*}}, section ".objc_classrefs$B"
// CHECK-COFF: @"\01L_selector(init)" = {{.*}}, section ".objc_selrefs$B"
// CHECK-COFF: @objc_classes = {{.*}}, section ".objc_classlist$B"
// CHECK-COFF: @objc_categories = {{.*}}, section ".objc_catlist$B"
// CHECK-COFF: @objc_non_lazy_classes = {{.*}}, section ".objc_nlclslist$B"

// CHECK-ELF-NOT: @_T04main1CCMf = {{.*}}, section "__DATA,__objc_data, regular"
// CHECK-ELF: @"\01l_OBJC_LABEL_PROTOCOL_$_P" = {{.*}}, section "objc_protolist"
// CHECK-ELF: @"\01l_OBJC_PROTOCOL_REFERENCE_$_P" = {{.*}}, section "objc_protorefs", align 8
// CHECK-ELF: @"OBJC_CLASS_REF_$_I" = {{.*}}, section "objc_classrefs", align 8
// CHECK-ELF: @"\01L_selector(init)" = {{.*}}, section "objc_selrefs"
// CHECK-ELF: @objc_classes = {{.*}}, section "objc_classlist"
// CHECK-ELF: @objc_categories = {{.*}}, section "objc_catlist"
// CHECK-ELF: @objc_non_lazy_classes = {{.*}}, section "objc_nlclslist", align 8

// CHECK-MACHO: @_T04main1CCMf = {{.*}}, section "__DATA,__objc_data, regular"
// CHECK-MACHO: @"\01l_OBJC_LABEL_PROTOCOL_$_P" = {{.*}}, section "__DATA,__objc_protolist,coalesced,no_dead_strip"
// CHECK-MACHO: @"\01l_OBJC_PROTOCOL_REFERENCE_$_P" = {{.*}}, section "__DATA,__objc_protorefs,coalesced,no_dead_strip"
// CHECK-MACHO: @"OBJC_CLASS_REF_$_I" = {{.*}}, section "__DATA,__objc_classrefs,regular,no_dead_strip"
// CHECK-MACHO: @"\01L_selector(init)" = {{.*}}, section "__DATA,__objc_selrefs,literal_pointers,no_dead_strip"
// CHECK-MACHO: @objc_classes = {{.*}}, section "__DATA,__objc_classlist,regular,no_dead_strip"
// CHECK-MACHO: @objc_categories = {{.*}}, section "__DATA,__objc_catlist,regular,no_dead_strip"
// CHECK-MACHO: @objc_non_lazy_classes = {{.*}}, section "__DATA,__objc_nlclslist,regular,no_dead_strip"

