/// The order of imports in sources shouldn't matter for access-levels.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/Lib1.swift -module-name Lib1 -o %t
// RUN: %target-swift-frontend -emit-module %t/Lib2.swift -module-name Lib2 -o %t

/// Test main cases.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t -package-name pkg \
// RUN:   -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/Client_Clang.swift -I %t \
// RUN:   -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/Client_Clang.swift -I %t -DINVERT \
// RUN:   -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/Client_Clang_Submodules.swift -I %t \
// RUN:   -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/Client_Clang_Submodules.swift -I %t -DINVERT \
// RUN:   -verify -verify-ignore-unrelated

// REQUIRES: VENDOR=apple

//--- Lib1.swift
public struct Type1 {}

//--- Lib2.swift
public struct Type2 {}

//--- Client.swift

/// Simple public vs internal.
package import Lib1 // expected-note {{imported 'package' here}}
// expected-note @-1 {{struct 'Type1' imported as 'package' from 'Lib1' here}}
internal import Lib1 // expected-warning {{module 'Lib1' is imported as 'package' from the same file; this 'internal' access level will be ignored}}

/// Simple package vs internal, inverted.
internal import Lib2 // expected-warning {{module 'Lib2' is imported as 'package' from the same file; this 'internal' access level will be ignored}}
package import Lib2 // expected-note {{imported 'package' here}}
// expected-note @-1 {{struct 'Type2' imported as 'package' from 'Lib2' here}}

public func dummyAPI(t1: Type1) {} // expected-error {{function cannot be declared public because its parameter uses a package type}}
// expected-note @-1 {{struct 'Type1' is imported by this file as 'package' from 'Lib1'}}

public func dummyAPI(t2: Type2) {} // expected-error {{function cannot be declared public because its parameter uses a package type}}
// expected-note @-1 {{struct 'Type2' is imported by this file as 'package' from 'Lib2'}}

//--- Client_Clang.swift

#if INVERT
private import ClangLib
#endif

internal import ClangLib2 // expected-note {{struct 'ClangType' imported as 'internal' from 'ClangLib2' here}}

#if !INVERT
private import ClangLib
#endif

public func dummyAPI(t2: ClangType) {}
// expected-error @-1 {{function cannot be declared public because its parameter uses an internal type}}
// expected-note @-2 {{struct 'ClangType' is imported by this file as 'internal' from 'ClangLib2'}}

//--- Client_Clang_Submodules.swift

#if INVERT
private import ClangLib.Sub1
#endif

internal import ClangLib.Sub2 // expected-note {{struct 'SubType' imported as 'internal' from 'Sub2' here}}

#if !INVERT
private import ClangLib.Sub1
#endif

public func dummyAPI(t2: SubType) {}
// expected-error @-1 {{function cannot be declared public because its parameter uses an internal type}}
// expected-note @-2 {{struct 'SubType' is imported by this file as 'internal' from 'Sub2'}}

//--- module.modulemap

module ClangLib {
  header "ClangLib1.h"

  explicit module Sub1 {
    header "Sub1.h"
  }

  explicit module Sub2 {
    header "Sub2.h"
  }
}

module ClangLib2 {
  header "ClangLib2.h"
  export *
}

//--- ClangLib1.h
struct ClangType {};

//--- ClangLib2.h
#include <ClangLib1.h>

//--- Sub1.h
struct SubType {};

//--- Sub2.h
#include "Sub1.h"
