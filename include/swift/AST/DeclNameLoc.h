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

/// `DeclNameLoc.h` is imported into Swift. Be *very* careful with what you
/// include here and keep these includes minimal!
/// If you don't need to import a header into Swift, include it in the `#ifdef`
/// block below instead.
///
/// See include guidelines and caveats in `BasicBridging.h`.
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SwiftBridging.h"
#include <stdint.h>

// Not imported into Swift in pure bridging mode.
#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"

#endif // #ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

class BridgedASTContext;
class BridgedArrayRef;

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
  const void *_Nullable LocationInfo;

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
  SourceLoc const *_Nullable getSourceLocs() const {
    if (NumArgumentLabels == 0)
      return reinterpret_cast<SourceLoc const *>(&LocationInfo);

    return reinterpret_cast<SourceLoc const *>(LocationInfo);
  }

  DeclNameLoc(const void *_Nullable LocationInfo, unsigned NumArgumentLabels)
      : LocationInfo(LocationInfo), NumArgumentLabels(NumArgumentLabels) {}

public:
  /// Create an invalid declaration name location.
  DeclNameLoc() : DeclNameLoc(nullptr, 0) {}

  /// Create declaration name location information for a base name.
  explicit DeclNameLoc(SourceLoc baseNameLoc)
      : DeclNameLoc(baseNameLoc.getOpaquePointerValue(), 0) {}

// Not imported into Swift in pure bridging mode.
#ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

  explicit DeclNameLoc(ASTContext &ctx, SourceLoc moduleSelectorLoc,
                       SourceLoc baseNameLoc)
    : DeclNameLoc(baseNameLoc) { }

  /// Create declaration name location information for a compound
  /// name.
  DeclNameLoc(ASTContext &ctx, SourceLoc baseNameLoc,
              SourceLoc lParenLoc,
              ArrayRef<SourceLoc> argumentLabelLocs,
              SourceLoc rParenLoc);

  DeclNameLoc(ASTContext &ctx, SourceLoc moduleSelectorLoc,
              SourceLoc baseNameLoc,
              SourceLoc lParenLoc,
              ArrayRef<SourceLoc> argumentLabelLocs,
              SourceLoc rParenLoc)
    : DeclNameLoc(ctx, baseNameLoc, lParenLoc, argumentLabelLocs, rParenLoc) { }

#endif // #ifdef NOT_COMPILED_WITH_SWIFT_PURE_BRIDGING_MODE

  SWIFT_NAME("init(_:moduleSelectorLoc:baseNameLoc:)")
  DeclNameLoc(BridgedASTContext cContext, SourceLoc moduleSelectorLoc,
              SourceLoc baseNameLoc);

  SWIFT_NAME("init(_:baseNameLoc:lParenLoc:"
             "argumentLabelLocs:rParenLoc:)")
  DeclNameLoc(BridgedASTContext cContext, SourceLoc baseNameLoc,
              SourceLoc lParenLoc, BridgedArrayRef cArgumentLabelLocs,
              SourceLoc rParenLoc);

  SWIFT_NAME("init(_:moduleSelectorLoc:baseNameLoc:"
             "lParenLoc:argumentLabelLocs:rParenLoc:)")
  DeclNameLoc(BridgedASTContext cContext, SourceLoc moduleSelectorLoc,
              SourceLoc baseNameLoc, SourceLoc lParenLoc,
              BridgedArrayRef cArgumentLabelLocs, SourceLoc rParenLoc);

  /// Whether the location information is valid.
  bool isValid() const { return getBaseNameLoc().isValid(); }

  /// Whether the location information is invalid.
  bool isInvalid() const { return getBaseNameLoc().isInvalid(); }

  /// Whether this was written as a compound name.
  bool isCompound() const { return NumArgumentLabels > 0; }

  /// Retrieve the location of the base name.
  SWIFT_COMPUTED_PROPERTY
  SourceLoc getBaseNameLoc() const {
    return getSourceLocs()[BaseNameIndex];
  }

  /// Retrieve the location of the left parentheses.
  SWIFT_COMPUTED_PROPERTY
  SourceLoc getLParenLoc() const {
    if (NumArgumentLabels == 0) return SourceLoc();
    return getSourceLocs()[LParenIndex];
  }

  /// Retrieve the location of the right parentheses.
  SWIFT_COMPUTED_PROPERTY
  SourceLoc getRParenLoc() const {
    if (NumArgumentLabels == 0) return SourceLoc();
    return getSourceLocs()[RParenIndex];
  }

  /// Retrieve the location of an argument label.
  SWIFT_COMPUTED_PROPERTY
  SourceLoc getArgumentLabelLoc(unsigned index) const {
    if (index >= NumArgumentLabels)
      return SourceLoc();
    return getSourceLocs()[FirstArgumentLabelIndex + index];
  }

  SWIFT_COMPUTED_PROPERTY
  SourceLoc getModuleSelectorLoc() const {
    return SourceLoc();
  }

  SWIFT_COMPUTED_PROPERTY
  SourceLoc getStartLoc() const {
    return getBaseNameLoc();
  }

  SWIFT_COMPUTED_PROPERTY
  SourceLoc getEndLoc() const {
    return NumArgumentLabels == 0 ? getBaseNameLoc() : getRParenLoc();
  }
  
  /// Retrieve the complete source range for this declaration name.
  SWIFT_COMPUTED_PROPERTY
  SourceRange getSourceRange() const {
    if (NumArgumentLabels == 0) return getBaseNameLoc();

    return SourceRange(getBaseNameLoc(), getRParenLoc());
  }
};
}

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_AST_DECL_NAME_LOC_H
