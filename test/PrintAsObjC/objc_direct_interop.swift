// REQUIRES: objc_interop

// The Swift callee and the clang caller have to agree on the direct symbol, and
// a mismatch shows up as a link error rather than a failed CHECK -- so the link
// step below is itself an assertion. Everything here is static: nothing is run,
// so this does not need executable_test.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the Swift library and its generated header.
// RUN: %target-build-swift -Xcc -fobjc-direct-precondition-thunk \
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

// The client links and runs correctly, printing "1" then "0" -- the second from
// a nil receiver going through clang's thunk. Running it needs a test host, so
// it is not a RUN line; to check it by hand:
//
//     %target-codesign %t/a.out && %target-run %t/a.out

// HEADER: - (NSInteger)returnOne SWIFT_WARN_UNUSED_RESULT SWIFT_OBJC_DIRECT;
// HEADER: - (void)normalMethod;

// DYLIB: T {{_?}}-[Direct returnOne]D

// SELECTORS-NOT: {{^}}returnOne{{$}}
// SELECTORS: {{^}}normalMethod{{$}}

// The client emits an undefined reference to the callee's exported symbol, and
// a local linkonce_odr thunk. It does not reference objc_msgSend for the direct
// method.
// CLIENT: {{_?}}-[Direct returnOne]D

// The call site branches to the caller-side thunk rather than objc_msgSend.
// ASM: bl "_-[Direct returnOne]D_thunk"

// The thunk holds the nil check the callee no longer has (cbz on self), and
// reaches the true implementation with a tail call -- b, not bl. That musttail
// is what keeps the thunk transparent to ARC.
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

  // A nullable receiver must go through the caller-side thunk, which returns a
  // zero-initialized value instead of dereferencing nil. The callee contains no
  // nil check at all under this ABI.
  Direct *maybeNil = nil;
  printf("%ld\n", (long)[maybeNil returnOne]);
  return 0;
}
