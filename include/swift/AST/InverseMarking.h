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

#ifndef SWIFT_AST_INVERSEMARKING_H
#define SWIFT_AST_INVERSEMARKING_H

#include "swift/AST/KnownProtocols.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/OptionalEnum.h"

namespace swift {

/// Describes the way an inverse and its corresponding positive contraint
/// appears on a TypeDecl, i.e., the way it was marked.
struct InverseMarking {
  enum class Kind : uint8_t {
    None,            // No inverse marking is present
    Explicit,        // Inverse is explicitly present.
    LegacyExplicit,   // An equivalent, explicit legacy annotation is present.
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

    Kind getKind() const {
      return kind.getValueOr(Kind::None);
    }
    bool is(Kind k) const {
      return getKind() == k;
    }

    // Is there an inferred or explicit marking?
    bool isPresent() const {
      return !is(Kind::None);
    }
    operator bool() const { return isPresent(); }

    // Is there any kind of explicit marking?
    bool isAnyExplicit() const {
      return is(Kind::Explicit) || is(Kind::LegacyExplicit);
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
      if (!other.kind.hasValue())
        return;
      setIfUnset(other.kind.getValue(), other.loc);
    }

    Mark with(Kind k) const {
      return Mark(k, loc);
    }
  };
};

}

#endif //SWIFT_AST_INVERSEMARKING_H
