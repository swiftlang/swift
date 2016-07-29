//===--- DebugTypeInfo.h - Type Info for Debugging --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the data structure that holds all the debug info
// we want to emit for types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_DEBUGTYPEINFO_H
#define SWIFT_IRGEN_DEBUGTYPEINFO_H

#include "IRGen.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"

namespace llvm {
class Type;
}

namespace swift {
class SILDebugScope;

namespace irgen {
class TypeInfo;

/// This data structure holds everything needed to emit debug info
/// for a type.
class DebugTypeInfo {
public:
  /// The DeclContext if this is has an Archetype.
  DeclContext *DeclCtx;
  /// The type we need to emit may be different from the type
  /// mentioned in the Decl, for example, stripped of qualifiers.
  TypeBase *Type;
  /// Needed to determine the size of basic types and to determine
  /// the storage type for undefined variables.
  llvm::Type *StorageType;
  Size size;
  Alignment align;

  // FIXME: feels like there might be too many constructors here

  DebugTypeInfo()
      : DeclCtx(nullptr), Type(nullptr), StorageType(nullptr), size(0),
        align(1) {}
  DebugTypeInfo(swift::Type Ty, llvm::Type *StorageTy, uint64_t SizeInBytes,
                uint32_t AlignInBytes, DeclContext *DC);
  DebugTypeInfo(swift::Type Ty, llvm::Type *StorageTy, Size size,
                Alignment align, DeclContext *DC);
  DebugTypeInfo(swift::Type Ty, const TypeInfo &Info, DeclContext *DC);
  DebugTypeInfo(TypeDecl *Decl, const TypeInfo &Info);
  DebugTypeInfo(ValueDecl *Decl, llvm::Type *StorageType, Size size,
                Alignment align);
  DebugTypeInfo(VarDecl *Decl, swift::Type Ty, const TypeInfo &Info,
                bool Unwrap);
  DebugTypeInfo(VarDecl *Decl, swift::Type Ty,
                llvm::Type *StorageType, Size size,
                Alignment align);
  TypeBase *getType() const { return Type; }

  TypeDecl *getDecl() const;
  DeclContext *getDeclContext() const { return DeclCtx; }

  void unwrapLValueOrInOutType() {
    Type = Type->getLValueOrInOutObjectType().getPointer();
  }

  // Determine whether this type is an Archetype itself.
  bool isArchetype() const {
    return Type->getLValueOrInOutObjectType()->getDesugaredType()->getKind() ==
           TypeKind::Archetype;
  }

  /// LValues, inout args, and Archetypes are implicitly indirect by
  /// virtue of their DWARF type.
  bool isImplicitlyIndirect() const {
    return Type->isLValueType() || isArchetype() ||
           (Type->getKind() == TypeKind::InOut);
  }

  bool isNull() const { return Type == nullptr; }
  bool operator==(DebugTypeInfo T) const;
  bool operator!=(DebugTypeInfo T) const;

  void dump() const;
};
}
}

namespace llvm {

// Dense map specialization.
template <> struct DenseMapInfo<swift::irgen::DebugTypeInfo> {
  static swift::irgen::DebugTypeInfo getEmptyKey() {
    return swift::irgen::DebugTypeInfo();
  }
  static swift::irgen::DebugTypeInfo getTombstoneKey() {
    return swift::irgen::DebugTypeInfo(
        llvm::DenseMapInfo<swift::TypeBase *>::getTombstoneKey(), nullptr, 0, 0,
        0);
  }
  static unsigned getHashValue(swift::irgen::DebugTypeInfo Val) {
    return DenseMapInfo<swift::CanType>::getHashValue(Val.getType());
  }
  static bool isEqual(swift::irgen::DebugTypeInfo LHS,
                      swift::irgen::DebugTypeInfo RHS) {
    return LHS == RHS;
  }
};
}

#endif
