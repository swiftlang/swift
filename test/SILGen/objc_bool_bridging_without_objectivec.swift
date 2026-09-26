// RUN: %target-swift-frontend -emit-silgen -parse-stdlib -module-name Swift -enable-objc-interop -target arm64-apple-macosx14 %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -parse-stdlib -module-name Swift -enable-objc-interop -target x86_64-apple-ios17-simulator %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -parse-stdlib -module-name Swift -enable-objc-interop -target arm64_32-apple-watchos10 %s | %FileCheck %s

// RUN: not %target-swift-frontend -emit-silgen -parse-stdlib -module-name Swift -enable-objc-interop -target x86_64-apple-macosx14 %s 2>&1 | %FileCheck %s --check-prefix=ERROR
// RUN: not %target-swift-frontend -emit-silgen -parse-stdlib -module-name Swift -enable-objc-interop -target x86_64-apple-watchos10-simulator %s 2>&1 | %FileCheck %s --check-prefix=ERROR

// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: CODEGENERATOR=X86

@frozen public struct Bool { var _value: Builtin.Int1 }
public typealias CBool = Bool

// CHECK-LABEL: sil hidden [ossa] @$ss1fyyyySbXEXBF : $@convention(thin) (@guaranteed @convention(block) @noescape (@convention(block) @noescape (Bool) -> ()) -> ()) -> ()
func f(_: @convention(block) ((Bool) -> ()) -> ()) {}

// CHECK-LABEL: sil hidden [ossa] @$ss1gyyyS2bXEXBF : $@convention(thin) (@guaranteed @convention(block) @noescape (@convention(block) @noescape (Bool) -> Bool) -> ()) -> ()
func g(_: @convention(block) ((Bool) -> Bool) -> ()) {}

// ERROR: error: could not find Objective-C bridge type for type 'Bool'; did you forget to import Foundation?
