// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -I %t -cxx-interoperability-mode=default %t/test.swift

// Regression test involving a free function with two declarations: a friend
// declaration inside a class (canonical) and a separate inline out-of-line
// definition (most-recent in the redecl chain). This originally caused the
// same member to be imported twice.

// This pattern is based on std::__1::operator==(__thread_id, __thread_id),
// which we reach while buildign the Swift stdlib via the Equatable conformance
// witness lookup for std::string (which pulls in *every* operator==).

//--- module.modulemap
module FriendOpRedecl {
    header "friend-op-redecl.h"
    requires cplusplus
}

//--- friend-op-redecl.h
#pragma once

struct Id {
  int data;
  // This friend declaration is the canonical decl for `operator==`
  friend bool operator==(Id a, Id b);
};

// This out-of-line definition is the most-recent decl in the redecl chain
inline bool operator==(Id a, Id b) { return a.data == b.data; }

// Decoy used only to trigger Swift's unqualified lookup for '=='
struct Decoy { int x; };
inline bool operator==(Decoy a, Decoy b) { return a.x == b.x; }

//--- test.swift
import FriendOpRedecl

// Note: Swift code does NOT mention `Id`; resolving `==` between Decoys
// drives unqualified lookup for '=='
let _ = Decoy() == Decoy()
