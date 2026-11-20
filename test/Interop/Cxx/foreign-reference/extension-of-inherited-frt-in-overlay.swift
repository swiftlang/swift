// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Overlay.swift \
// RUN:   -module-name Overlay -o %t/Overlay.swiftmodule \
// RUN:   -cxx-interoperability-mode=default -cxx-interop-getters-setters-as-properties \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -I %t/Inputs
// RUN: %target-swift-frontend -typecheck %t/Client.swift \
// RUN:   -cxx-interoperability-mode=default -cxx-interop-getters-setters-as-properties \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -I %t/Inputs -I %t

// Importing Middle also imports Leaf, whose members are checked for overrides
// in Middle. That must not deserialize Overlay's extension of Middle, which is
// what led to importing Middle in the first place.

//--- Inputs/module.modulemap
module Lib {
  header "lib.h"
  requires cplusplus
  export *
}

//--- Inputs/lib.h
class __attribute__((swift_attr("import_reference")))
      __attribute__((swift_attr("retain:.retain")))
      __attribute__((swift_attr("release:.release"))) Root {
public:
  void retain() const;
  void release() const;
  virtual const Root *getMetaClass() const;
};

class Leaf;

class Middle : public Root {
public:
  const Root *getMetaClass() const override;
  const Leaf *leaf;
  bool operator==(const Leaf &) const;
  const Leaf *getLeaf() const;
};

class Leaf : public Middle {
public:
  const Root *getMetaClass() const override;
};

//--- Overlay.swift
@_exported import Lib

extension Middle: @retroactive Equatable {
  public static func == (a: Middle, b: Middle) -> Bool { true }
}

//--- Client.swift
import Overlay

// Looking up '==' deserializes Overlay's Middle.==.
public func isZero(_ n: Int32) -> Bool { n == 0 }
