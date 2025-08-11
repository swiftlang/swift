// REQUIRES: VENDOR=apple 
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -tbd-install_name directive_use -tbd-is-installapi -emit-tbd -emit-tbd-path %t/linker_directives.tbd
// RUN: %llvm-nm %t/linker_directives.tbd | %FileCheck %s --check-prefix CHECK-HAS
// RUN: %llvm-nm %t/linker_directives.tbd | %FileCheck %s --check-prefix CHECK-HAS-NOT

// RUN: %target-swift-frontend -typecheck %s -tbd-install_name directive_use -emit-tbd -emit-tbd-path %t/linker_directives.tbd
// RUN: %llvm-nm %t/linker_directives.tbd | %FileCheck %s --check-prefix CHECK-HAS
// RUN: %llvm-nm %t/linker_directives.tbd | %FileCheck %s --check-prefix CHECK-HAS-NOT

@available(OSX 10.8, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 10.15)
public func toast() {}

// CHECK-HAS: D $ld$hide$os10.14$_$s10ToasterKit5toastyyF
// CHECK-HAS: D $ld$hide$os10.8$_$s10ToasterKit5toastyyF

// CHECK-HAS-NOT-NOT: $ld$hide$os10.15$_$s10ToasterKit5toastyyF
// CHECK-HAS-NOT-NOT: $ld$hide$os10.7$_$s10ToasterKit5toastyyF

@available(macOS 15, *)
@_originallyDefinedIn(module: "ToasterKit", macOS 16)
public func toastMovedInVersionMappingTo26() {}

// CHECK-HAS: D $ld$hide$os15.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os15.1$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os15.10$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// FIXME: Symbols shouldn't be generated for non-existent intermediate OS versions.
// CHECK-HAS: D $ld$hide$os16.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os17.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os18.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os19.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os20.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os21.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os22.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os23.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os24.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS: D $ld$hide$os25.0$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF

// CHECK-HAS-NOT-NOT: $ld$hide$os14.{{.*}}$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
// CHECK-HAS-NOT-NOT: $ld$hide$os26.{{.*}}$_$s10ToasterKit30toastMovedInVersionMappingTo26yyF
