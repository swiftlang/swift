// An FRT by inheritance whose definition is unreachable (NS::Anchor below) is
// imported as unavailable, instead of being rejected when it is imported.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// DEFINE: %{frontend} = %target-swift-frontend -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 -target %target-swift-5.8-abi-triple -parse-as-library -module-name main

// Using an unrelated operator doesn't trip over Anchor, even though resolving
// it imports a C++ operator== whose parameter types contain Anchor.
// RUN: %{frontend} -emit-ir %t/int-equals.swift -o %t/int-equals.ll

// RUN: %{frontend} -typecheck -verify %t%{fs-sep}uses.swift -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}Hidden.h

//--- Inputs/module.modulemap
module Lib {
  explicit module Base { header "Base.h" export * }
  // Defines Anchor. Explicit, and not imported by Swift, so Anchor's
  // definition is unreachable.
  explicit module Hidden { header "Hidden.h" export * }
  explicit module Visible { header "Visible.h" export * }
}

//--- Inputs/Base.h
#pragma once

namespace NS {
template <typename T>
class __attribute__((swift_attr("import_reference")))
      __attribute__((swift_attr("retain:.ref")))
      __attribute__((swift_attr("release:.deref")))
      __attribute__((swift_attr("returned_as_unretained_by_default")))
      RefCountedBase {
public:
  void ref() const {}
  void deref() const {}
};
} // namespace NS

//--- Inputs/Hidden.h
#pragma once
#include "Base.h"

namespace NS {
// An FRT by inheritance from RefCountedBase.
// The note is for the two uses in uses.swift that name Anchor.
// expected-note@+1 2 {{'Anchor' has been explicitly marked unavailable here}}
class Anchor : public RefCountedBase<Anchor> {};
} // namespace NS

//--- Inputs/Visible.h
#pragma once
#include "Base.h"

namespace NS {
class Anchor; // Only forward-declared.

struct Handle {
  const Anchor *anchor;
};
struct Key {
  Handle handle;
};
inline bool operator==(const Key &, const Key &) { return true; }
} // namespace NS

//--- int-equals.swift
import Lib.Visible

public func probe(a: Int, b: Int) -> Bool {
  a == b
}

//--- uses.swift
import Lib.Visible

public func namesAnchor(_ a: NS.Anchor) {} // expected-error {{foreign reference type 'Anchor' has unreachable definition}}

public func namesOptionalAnchor() {
  let _: NS.Anchor? = nil // expected-error {{foreign reference type 'Anchor' has unreachable definition}}
}
