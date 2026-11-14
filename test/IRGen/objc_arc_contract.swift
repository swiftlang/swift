// Make sure that we run objc arc contract when emitting ir or bc with optimization enabled.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir -Xllvm -disable-objc-arc-contract -parse-as-library -O | %FileCheck --check-prefix=CHECK-WITHOUT-PASS %s
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-bc -Xllvm -disable-objc-arc-contract -parse-as-library -O -o %t/test1.bc && %llvm-dis -o - %t/test1.bc | %FileCheck --check-prefix=CHECK-WITHOUT-PASS %s
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir -disable-llvm-optzns -parse-as-library -O | %FileCheck --check-prefix=CHECK-WITHOUT-PASS %s
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-bc -disable-llvm-optzns -parse-as-library -O -o %t/test1.bc && %llvm-dis -o - %t/test1.bc | %FileCheck --check-prefix=CHECK-WITHOUT-PASS %s

// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir -parse-as-library -O | %FileCheck --check-prefix=CHECK-WITH-PASS %s
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-bc -parse-as-library -O -o %t/test2.bc && %llvm-dis -o - %t/test2.bc | %FileCheck --check-prefix=CHECK-WITH-PASS %s

// Clang code generation at -O1 and above leaves clang.arc.use and
// clang.arc.noop.use, so we need the ObjCARCContract pass to remove them at
// -Onone too (rdar://192359751).
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir -Xllvm -disable-objc-arc-contract -parse-as-library -Onone -Xcc -O2 | %FileCheck --check-prefix=CHECK-WITHOUT-PASS %s
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/StaticInline.h %s -emit-ir -parse-as-library -Onone -Xcc -O2 | %FileCheck --check-prefix=CHECK-WITH-PASS %s

// On 32-bit ARM and x86, Clang leaves the assembly markers to the pass via a
// module flag. There's no stdlib for these targets, so compile the client as
// the Swift module itself, defining the Void that importing useIt() needs.
// RUN: echo 'id makeObject(void); static inline void useIt(void) { id o = makeObject(); (void)o; }' > %t/H.h
// RUN: echo 'public typealias Void = (); public func f() { useIt() }' > %t/client.swift
// RUN: %if CODEGENERATOR=ARM %{ %swift -target armv7k-apple-watchos9.0 -emit-ir -parse-stdlib -module-name Swift -parse-as-library -import-objc-header %t/H.h %t/client.swift -Onone -Xcc -O2 | %FileCheck --check-prefix=CHECK-MARKER-ARM %s %}
// RUN: %if CODEGENERATOR=X86 %{ %swift -target i386-apple-watchos9.0-simulator -emit-ir -parse-stdlib -module-name Swift -parse-as-library -import-objc-header %t/H.h %t/client.swift -Onone -Xcc -O2 | %FileCheck --check-prefix=CHECK-MARKER-X86 %s %}

// REQUIRES: objc_interop
// REQUIRES: asserts

// CHECK-WITHOUT-PASS: call void (...) @llvm.objc.clang.arc.use
// CHECK-WITH-PASS-NOT: call void (...) @llvm.objc.clang.arc.{{(noop\.)?}}use

// CHECK-MARKER-ARM: call void asm sideeffect "mov\09r7, r7{{.*}}marker for objc_retainAutoreleaseReturnValue"
// CHECK-MARKER-ARM-NEXT: call ptr @llvm.objc.retainAutoreleasedReturnValue

// CHECK-MARKER-X86: call void asm sideeffect "movl\09%ebp, %ebp{{.*}}marker for objc_retainAutoreleaseReturnValue"
// CHECK-MARKER-X86-NEXT: call ptr @llvm.objc.retainAutoreleasedReturnValue

import Foundation

@inline(never)
public func foo() throws {
  let x: FileManager! = nil
  let y = URL(string: "http://swift.org")
  let z: URL! = nil
  let w: String = "foo"
  var e: NSError? = nil
  test(x, y, z, w, .usingNewMetadataOnly, &e)
}

// Adds clang.arc.noop.use; foo() above produces clang.arc.use.
public func bar() -> String {
  return staticInlineFun()
}
