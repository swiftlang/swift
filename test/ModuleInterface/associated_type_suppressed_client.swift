// RUN: %empty-directory(%t)

// Check that you can use a library that has the new version of the feature, even if using the old feature locally.

// RUN: %target-build-swift -target %target-cpu-apple-macosx13 -parse-as-library -emit-library -enable-experimental-feature SuppressedAssociatedTypesWithDefaults \
// RUN:     -emit-module-path %t/lib.swiftmodule -module-name lib -enable-library-evolution %S/associated_type_suppressed.swift -o %t/%target-library-name(lib)
// RUN: %target-codesign %t/%target-library-name(lib)

// RUN: %target-swift-frontend -DEXPECTED_ERROR -typecheck -verify -verify-ignore-unrelated -enable-experimental-feature SuppressedAssociatedTypes -I %t -L %t %s

// RUN: %target-build-swift -enable-experimental-feature SuppressedAssociatedTypes -target %target-cpu-apple-macosx13 -llib -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefixes=EXEC %s

// RUN: %target-build-swift -enable-experimental-feature SuppressedAssociatedTypesWithDefaults -target %target-cpu-apple-macosx13 -llib -module-name main -I %t -L %t %s -o %t/a.out
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
  associatedtype Element: ~Copyable
}

ncIter2(S<Int, Int>())  // EXEC: hello S<Int, Int>()
ncIter2(S<Int, NC>())   // EXEC: hello S<Int, NC>()

// Just ensures no linking error
public func check<T: OldVersion>(_ s: S<T, T.Element>) {
  ncIter2(s)
}

#if EXPECTED_ERROR
ncIter2(S<NC, NC>()) // expected-error {{global function 'ncIter2' requires that 'NC' conform to 'Copyable'}}

func requireCopyable<T: Copyable>(_ t: T) {} // expected-note {{}}

func test<T: OldVersion>(_ t: borrowing T, _ e: borrowing T.Element) {
  requireCopyable(e) // expected-error {{global function 'requireCopyable' requires that 'T.Element' conform to 'Copyable'}}
}
#endif

