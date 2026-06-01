// ---------------------
// Feature mixing: library = NEW, client = OLD
// RUN: %empty-directory(%t)

// RUN: %target-build-swift -target %target-cpu-apple-macosx13 -parse-as-library -emit-library -enable-experimental-feature SuppressedAssociatedTypesWithDefaults \
// RUN:     -emit-module-path %t/lib.swiftmodule -emit-module-interface-path %t/lib.swiftinterface \
// RUN:     -module-name lib -enable-library-evolution %S/associated_type_suppressed.swift -o %t/%target-library-name(lib)
// RUN: %target-codesign %t/%target-library-name(lib)

// RUN: %target-swift-frontend -DEXPECTED_ERROR -typecheck -verify -verify-ignore-unrelated -enable-experimental-feature SuppressedAssociatedTypes -I %t -L %t %s

// RUN: %target-build-swift -enable-experimental-feature SuppressedAssociatedTypes -target %target-cpu-apple-macosx13 -llib -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=EXEC %s

// Try again after deleting the swiftmodule to force rebuilding from its interface
// RUN: rm %t/lib.swiftmodule

// RUN: %target-build-swift -enable-experimental-feature SuppressedAssociatedTypes -target %target-cpu-apple-macosx13 -llib -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=EXEC %s

// Ensure NEW & NEW works too.
// RUN: %target-build-swift -enable-experimental-feature SuppressedAssociatedTypesWithDefaults -target %target-cpu-apple-macosx13 -llib -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=EXEC %s


// ---------------------
// Feature mixing: library = OLD, client = NEW
// RUN: %empty-directory(%t)

// RUN: %target-build-swift -target %target-cpu-apple-macosx13 -parse-as-library -emit-library -enable-experimental-feature SuppressedAssociatedTypes \
// RUN:     -emit-module-path %t/lib.swiftmodule -emit-module-interface-path %t/lib.swiftinterface \
// RUN:     -module-name lib -enable-library-evolution %S/associated_type_suppressed.swift -o %t/%target-library-name(lib)
// RUN: %target-codesign %t/%target-library-name(lib)

// RUN: %target-build-swift -DEXPECTED_ERROR -enable-experimental-feature SuppressedAssociatedTypesWithDefaults -target %target-cpu-apple-macosx13 -llib -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=EXEC %s


// REQUIRES: OS=macosx && (CPU=x86_64 || CPU=arm64)
// REQUIRES: executable_test
// UNSUPPORTED: remote_run || device_run

// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults
// REQUIRES: swift_feature_SuppressedAssociatedTypes

import lib

struct NC: ~Copyable {}
public struct S<Element, Iterator>: P where Element: ~Copyable, Iterator: ~Copyable {}

public protocol OldVersion<Element> {
  associatedtype Element: ~Copyable // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated; see SE-503 for the official version}}
}

// `ncIter2` is defined with a generic signature <T: P> where P has a suppressed primary associated type Element.
// Under a library built using the NEW feature, it should require S's first type to be Copyable, otherwise, it's not
// required to be Copyable.

ncIter2(S<Int, Int>())  // EXEC: hello S<Int, Int>()
ncIter2(S<Int, NC>())   // EXEC: hello S<Int, NC>()

// Ensure no linking error calling other kinds of public funcs
public func check<C, NC>(_ s: S<C, NC>) {
  ncIter(s)
  ncIter2(s)
  ncIter3(s)
  ncBoth(s)
  ncElement_pi(s)
  ncElement(s)
  bothCopyable(s)
}
check(S<String, String>())

#if EXPECTED_ERROR
ncIter2(S<NC, NC>()) // expected-error {{global function 'ncIter2' requires that 'NC' conform to 'Copyable'}}

func requireCopyable<T: Copyable>(_ t: T) {} // expected-note {{}}

func test<T: OldVersion>(_ t: borrowing T, _ e: borrowing T.Element) {
  requireCopyable(e) // expected-error {{global function 'requireCopyable' requires that 'T.Element' conform to 'Copyable'}}
}
#endif

