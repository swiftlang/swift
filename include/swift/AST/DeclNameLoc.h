//===--- DeclNameLoc.h - Declaration Name Location Info ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the DeclNameLoc class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_DECL_NAME_LOC_H
#define SWIFT_AST_DECL_NAME_LOC_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {

class ASTContext;

/// Source location information for a declaration name (\c DeclName)
/// written in the source.
class DeclNameLoc {
  /// Source location information.
  ///
  /// If \c NumArgumentLabels == 0, this is the SourceLoc for the base name.
  /// Otherwise, it points to an array of SourceLocs, which contains:
  /// * The base name location
  /// * The left parentheses location
  /// * The right parentheses location
  /// * The locations of each of the argument labels.
  const void *LocationInfo;

  /// The number of argument labels stored in the name.
  unsigned NumArgumentLabels;

  enum {
    BaseNameIndex = 0,
    LParenIndex = 1,
    RParenIndex = 2,
    FirstArgumentLabelIndex = 3,
  };

  /// Retrieve a pointer to either the only source location that was
  /// stored or to the array of source locations that was stored.
  SourceLoc const * getSourceLocs() const {
    if (NumArgumentLabels == 0) 
      return reinterpret_cast<SourceLoc const *>(&LocationInfo);

    return reinterpret_cast<SourceLoc const *>(LocationInfo);
  }

public:
  /// Create an invalid declaration name location.
  DeclNameLoc() : LocationInfo(0), NumArgumentLabels(0) { }

  /// Create declaration name location information for a base name.
  explicit DeclNameLoc(SourceLoc baseNameLoc)
    : LocationInfo(baseNameLoc.getOpaquePointerValue()),
      NumArgumentLabels(0) { }

  /// Create declaration name location information for a compound
  /// name.
  DeclNameLoc(ASTContext &ctx, SourceLoc baseNameLoc,
              SourceLoc lParenLoc,
              ArrayRef<SourceLoc> argumentLabelLocs,
              SourceLoc rParenLoc);

  /// Whether the location information is valid.
  bool isValid() const { return getBaseNameLoc().isValid(); }

  /// Whether the location information is invalid.
  bool isInvalid() const { return getBaseNameLoc().isInvalid(); }

  /// Whether this was written as a compound name.
  bool isCompound() const { return NumArgumentLabels > 0; }

  /// Retrieve the location of the base name.
  SourceLoc getBaseNameLoc() const {
    return getSourceLocs()[BaseNameIndex];
  }

  /// Retrieve the location of the left parentheses.
  SourceLoc getLParenLoc() const {
    if (NumArgumentLabels == 0) return SourceLoc();
    return getSourceLocs()[LParenIndex];
  }

  /// Retrieve the location of the right parentheses.
  SourceLoc getRParenLoc() const {
    if (NumArgumentLabels == 0) return SourceLoc();
    return getSourceLocs()[RParenIndex];
  }

  /// Retrieve the location of an argument label.
  SourceLoc getArgumentLabelLoc(unsigned index) const {
    if (index >= NumArgumentLabels)
      return SourceLoc();
    return getSourceLocs()[FirstArgumentLabelIndex + index];
  }

  /// Retrieve the complete source range for this declaration name.
  SourceRange getSourceRange() const {
    if (NumArgumentLabels == 0) return getBaseNameLoc();

    return SourceRange(getBaseNameLoc(), getRParenLoc());
  }
};

}

#endif // SWIFT_AST_DECL_NAME_LOC_H
