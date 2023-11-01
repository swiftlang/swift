//===- InverseMarking.h - Utilities for tracking inverse types -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LIB_SEMA_INVERSEMARKING_H
#define SWIFT_LIB_SEMA_INVERSEMARKING_H

#include "swift/AST/KnownProtocols.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/OptionalEnum.h"

namespace swift {

/// Describes the way an inverse and its corresponding positive contraint
/// appears on a TypeDecl, i.e., the way it was marked.
struct InverseMarking {
  enum class Kind : uint8_t {
    None,            // No inverse marking is present
    Inferred,        // Inverse is inferred based on generic parameters.
    Explicit,        // Inverse is explicitly present.

    LAST = Explicit
  };

  // Describes what kind of mark was found, if any.
  struct Mark {
  private:
    OptionalEnum<Kind> kind;
    SourceLoc loc;
  public:
    // Creates an empty mark.
    Mark() {};

    // Creates a mark.
    Mark(Kind k, SourceLoc l = SourceLoc())
      : kind(k), loc(l) {};

    // Is there an inferred or explicit marking?
    bool isPresent() const {
      return getKind() != Kind::None;
    }
    operator bool() { return isPresent(); }

    Kind getKind() const {
      return kind.getValueOr(Kind::None);
    }

    SourceLoc getLoc() const { return loc; }

    void set(Kind k, SourceLoc l = SourceLoc()) {
      assert(!kind.hasValue());
      kind = k;
      loc = l;
    }

    void setIfUnset(Kind k, SourceLoc l = SourceLoc()) {
      if (kind.hasValue())
        return;
      set(k, l);
    }

    void setIfUnset(Mark other) {
      if (kind.hasValue())
        return;
      kind = other.kind;
      loc = other.loc;
    }

    Mark with(Kind k) {
      kind = k;
      return *this;
    }
  };

private:
  Mark inverse;
  Mark positive;
public:

  // Creates an empty marking.
  InverseMarking() {}

  Mark &getInverse() { return inverse; }
  Mark &getPositive() { return positive; }

  // Merge the results of another marking into this one.
  void merge(InverseMarking other) const {
    other.inverse.setIfUnset(other.inverse);
    other.positive.setIfUnset(other.positive);
  }

  static InverseMarking forInverse(Kind kind, SourceLoc loc = SourceLoc()) {
    InverseMarking marking;
    marking.inverse.set(kind, loc);
    marking.positive.set(Kind::None);
    return marking;
  }
};

}

#endif //SWIFT_LIB_SEMA_INVERSEMARKING_H
