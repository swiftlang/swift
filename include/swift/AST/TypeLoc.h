//===--- TypeLoc.h - Swift Language Type Locations --------------*- C++ -*-===//
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
// This file defines the TypeLoc struct and related structs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPELOC_H
#define SWIFT_TYPELOC_H

#include "swift/Basic/SourceLoc.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeRepr.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {

class ASTContext;

/// TypeLoc - Provides source location information for a parsed type.
/// A TypeLoc is stored in AST nodes which use an explicitly written type.
struct TypeLoc {
private:
  /// \brief The resolved type and a bit indicating if it was validated, which
  /// means it went through possible generic substitutions.
  llvm::PointerIntPair<Type, 1, bool> TAndValidBit;
  TypeRepr *TyR = nullptr;

  TypeLoc(Type T, TypeRepr *TyR) : TAndValidBit(T, false), TyR(TyR) {}

public:
  TypeLoc() {}
  TypeLoc(TypeRepr *TyR) : TyR(TyR) {}
  TypeLoc(TypeRepr *TyR, Type Ty) : TyR(TyR) {
    setType(Ty);
  }

  bool wasValidated() const { return TAndValidBit.getInt(); }
  bool isError() const;

  // FIXME: We generally shouldn't need to build TypeLocs without a location.
  static TypeLoc withoutLoc(Type T) {
    return TypeLoc(T, nullptr);
  }

  /// Get the representative location of this type, for diagnostic
  /// purposes.
  SourceLoc getLoc() const {
    if (TyR) return TyR->getLoc();
    return SourceLoc();
  }

  SourceRange getSourceRange() const;

  bool hasLocation() const { return TyR != nullptr; }
  TypeRepr *getTypeRepr() const { return TyR; }
  Type getType() const { return TAndValidBit.getPointer(); }

  bool isNull() const { return getType().isNull() && TyR == nullptr; }

  void setInvalidType(ASTContext &C);
  void setType(Type Ty, bool validated = false) {
    TAndValidBit.setPointerAndInt(Ty, validated);
  }

  TypeLoc clone(ASTContext &ctx) const {
    if (TyR) {
      TypeLoc result(TyR->clone(ctx));
      result.TAndValidBit = this->TAndValidBit;
      return result;
    }

    return *this;
  }
};

} // end namespace llvm

#endif
