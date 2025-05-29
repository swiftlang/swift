// REQUIRES: VENDOR=apple 
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -tbd-install_name directive_use -tbd-is-installapi -emit-tbd -emit-tbd-path %t/linker_directives.tbd

// RUN: %llvm-nm %t/linker_directives.tbd | %FileCheck %s --check-prefix CHECK-HAS --check-prefix CHECK-HAS-NOT 
// RUN: %target-swift-frontend -typecheck %s -tbd-install_name directive_use -emit-tbd -emit-tbd-path %t/linker_directives.tbd
// RUN: %llvm-nm %t/linker_directives.tbd | %FileCheck %s --check-prefix CHECK-HAS --check-prefix CHECK-HAS-NOT 

@available(OSX 10.8, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 10.15)
public func toast() {}

// CHECK-HAS: D $ld$hide$os10.14$_$s10ToasterKit5toastyyF
// CHECK-HAS: D $ld$hide$os10.8$_$s10ToasterKit5toastyyF

// CHECK-HAS-NOT-NOT: $ld$hide$os10.15$_$s10ToasterKit5toastyyF
// CHECK-HAS-NOT-NOT: $ld$hide$os10.7$_$s10ToasterKit5toastyyF
