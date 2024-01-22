//===--- LifetimeDependenceSpecifiers.h ------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines types and utilities related to Lifetime Dependence
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LIFETIMEDEPENDENCE_H
#define SWIFT_AST_LIFETIMEDEPENDENCE_H

#include "swift/AST/Decl.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/OptionSet.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {

enum class LifetimeDependenceKind : uint8_t {
  Copy = 0,
  Consume,
  Borrow,
  Mutate
};

class LifetimeDependenceSpecifier {
public:
  enum class SpecifierKind { Named, Ordered, Self };

private:
  SourceLoc loc;
  SpecifierKind specifierKind;
  LifetimeDependenceKind lifetimeDependenceKind;
  union Value {
    struct {
      Identifier name;
    } Named;
    struct {
      unsigned index;
    } Ordered;
    struct {
    } self;
    Value(Identifier name) : Named({name}) {}
    Value(unsigned index) : Ordered({index}) {}
    Value() {}
  } value;

  LifetimeDependenceSpecifier(SourceLoc loc, SpecifierKind specifierKind,
                              LifetimeDependenceKind lifetimeDependenceKind,
                              Value value)
      : loc(loc), specifierKind(specifierKind),
        lifetimeDependenceKind(lifetimeDependenceKind), value(value) {}

public:
  static LifetimeDependenceSpecifier getNamedLifetimeDependenceSpecifier(
      SourceLoc loc, LifetimeDependenceKind kind, Identifier name) {
    return {loc, SpecifierKind::Named, kind, name};
  }

  static LifetimeDependenceSpecifier getOrderedLifetimeDependenceSpecifier(
      SourceLoc loc, LifetimeDependenceKind kind, unsigned index) {
    return {loc, SpecifierKind::Ordered, kind, index};
  }

  static LifetimeDependenceSpecifier
  getSelfLifetimeDependenceSpecifier(SourceLoc loc,
                                     LifetimeDependenceKind kind) {
    return {loc, SpecifierKind::Self, kind, {}};
  }

  SourceLoc getLoc() const { return loc; }

  SpecifierKind getSpecifierKind() const { return specifierKind; }

  LifetimeDependenceKind getLifetimeDependenceKind() const {
    return lifetimeDependenceKind;
  }

  Identifier getName() const {
    assert(specifierKind == SpecifierKind::Named);
    return value.Named.name;
  }

  unsigned getIndex() const {
    assert(specifierKind == SpecifierKind::Ordered);
    return value.Ordered.index;
  }
  std::string getParamString() const {
    switch (specifierKind) {
    case SpecifierKind::Named:
      return value.Named.name.str().str();
    case SpecifierKind::Self:
      return "self";
    case SpecifierKind::Ordered:
      return std::to_string(value.Ordered.index);
    }
    llvm_unreachable("Invalid LifetimeDependenceSpecifier::SpecifierKind");
  }
};
} // namespace swift

#endif