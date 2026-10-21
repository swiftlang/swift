// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// The link step is itself a check: Swift and Clang must agree on the symbol.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the Swift library and its generated header.
// RUN: %target-build-swift -enable-experimental-feature ObjCDirect \
// RUN:   -emit-library -emit-objc-header -emit-objc-header-path %t/Mod-Swift.h \
// RUN:   -module-name Mod %t/Mod.swift -o %t/libMod.dylib

// The header must carry the attribute, otherwise clang would emit an ordinary
// msgSend to a selector that is no longer in the class metadata.
// RUN: %FileCheck %s -check-prefix=HEADER < %t/Mod-Swift.h

// The public direct symbol is exported from the library.
// RUN: %llvm-nm -gU %t/libMod.dylib | %FileCheck %s -check-prefix=DYLIB

// The direct selector is gone from the binary's ObjC metadata, while a normal
// @objc selector is still there.
// RUN: %llvm-nm -a %t/libMod.dylib > %t/syms.txt
// RUN: strings %t/libMod.dylib | %FileCheck %s -check-prefix=SELECTORS

// Compile the ObjC client to assembly and check how the call is dispatched.
// RUN: %target-clang -fobjc-arc -fobjc-direct-precondition-thunk -I %t \
// RUN:   -S -o - %t/use.m | %FileCheck %s -check-prefix=ASM

// Compile and link the client. Undefined-symbol agreement is checked by ld.
// RUN: %target-clang -fobjc-arc -fobjc-direct-precondition-thunk -I %t \
// RUN:   -c -o %t/use.o %t/use.m
// RUN: %llvm-nm -u %t/use.o | %FileCheck %s -check-prefix=CLIENT
// RUN: %target-clang %t/use.o %t/libMod.dylib -lobjc -framework Foundation -o %t/a.out

// Not run, since that needs a test host; it prints 1, then 0 for nil.

// HEADER: - (NSInteger)returnOne SWIFT_WARN_UNUSED_RESULT SWIFT_OBJC_DIRECT;
// HEADER: - (void)normalMethod;

// DYLIB: T {{_?}}-[Direct returnOne]D

// SELECTORS-NOT: {{^}}returnOne{{$}}
// SELECTORS: {{^}}normalMethod{{$}}

// The client calls the exported symbol through a local thunk, not objc_msgSend.
// CLIENT: {{_?}}-[Direct returnOne]D

// The call site branches to the caller-side thunk rather than objc_msgSend.
// ASM: bl "_-[Direct returnOne]D_thunk"

// The thunk nil-checks self (cbz) and tail-calls the implementation (b).
// ASM-LABEL: "_-[Direct returnOne]D_thunk":
// ASM: cbz
// ASM: b "_-[Direct returnOne]D"

//--- Mod.swift
import Foundation

public class Direct: NSObject {
  @objcDirect public final func returnOne() -> Int { return 1 }
  @objc public func normalMethod() {}
}

//--- use.m
#import "Mod-Swift.h"
#import <stdio.h>

int main() {
  Direct *d = [[Direct alloc] init];
  printf("%ld\n", (long)[d returnOne]);

  // A nil receiver goes through the caller-side thunk, which returns zero.
  Direct *maybeNil = nil;
  printf("%ld\n", (long)[maybeNil returnOne]);
  return 0;
}
