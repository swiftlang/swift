// REQUIRES: VENDOR=apple 
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -tbd-is-installapi -emit-tbd -emit-tbd-path %t/linker_directives.tbd
// RUN: %FileCheck %s --check-prefix CHECK-HAS --check-prefix CHECK-HAS-NOT < %t/linker_directives.tbd
// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/linker_directives.tbd
// RUN: %FileCheck %s --check-prefix CHECK-HAS --check-prefix CHECK-HAS-NOT < %t/linker_directives.tbd

@available(OSX 10.8, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 10.15)
public func toast() {}

// CHECK-HAS: $ld$hide$os10.14$_$s10ToasterKit5toastyyF
// CHECK-HAS: $ld$hide$os10.8$_$s10ToasterKit5toastyyF

// CHECK-HAS-NOT-NOT: $ld$hide$os10.15$_$s10ToasterKit5toastyyF
// CHECK-HAS-NOT-NOT: $ld$hide$os10.7$_$s10ToasterKit5toastyyF

// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/linker_directives.tbd -emit-ldadd-cfile-path %t/ldAdd.c -module-name AppKit
// RUN: %FileCheck %s --check-prefix CHECK-C-SYMBOL < %t/ldAdd.c

// CHECK-C-SYMBOL: $ld$add$os10.14$_$s10ToasterKit5toastyyF
// CHECK-C-SYMBOL: $ld$add$os10.8$_$s10ToasterKit5toastyyF