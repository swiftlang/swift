//===--- DebugTypeInfo.h - Type Info for Debugging --------------*- C++ -*-===//
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
class SILGlobalVariable;

namespace irgen {
class TypeInfo;

/// This data structure holds everything needed to emit debug info
/// for a type.
class DebugTypeInfo {
public:
  /// The DeclContext of the function. This might not be the DeclContext of
  /// the variable if inlining took place.
  DeclContext *DeclCtx;
  /// The type we need to emit may be different from the type
  /// mentioned in the Decl, for example, stripped of qualifiers.
  TypeBase *Type;
  /// Needed to determine the size of basic types and to determine
  /// the storage type for undefined variables.
  llvm::Type *StorageType;
  Size size;
  Alignment align;

  DebugTypeInfo()
      : DeclCtx(nullptr), Type(nullptr), StorageType(nullptr), size(0),
        align(1) {}
  DebugTypeInfo(DeclContext *DC, swift::Type Ty, llvm::Type *StorageTy,
                Size SizeInBytes, Alignment AlignInBytes);
  /// Create type for a local variable.
  static DebugTypeInfo getLocalVariable(DeclContext *DeclCtx, VarDecl *Decl,
                                        swift::Type Ty, const TypeInfo &Info,
                                        bool Unwrap);
  /// Create type for an artificial metadata variable.
  static DebugTypeInfo getMetadata(swift::Type Ty, llvm::Type *StorageTy,
                                   Size size, Alignment align);
  /// Create a standalone type from a TypeInfo object.
  static DebugTypeInfo getFromTypeInfo(DeclContext *DC, swift::Type Ty,
                                       const TypeInfo &Info);
  /// Global variables.
  static DebugTypeInfo getGlobal(SILGlobalVariable *GV, llvm::Type *StorageType,
                                 Size size, Alignment align);
  /// ObjC classes.
  static DebugTypeInfo getObjCClass(ClassDecl *theClass,
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
    return Type->getLValueOrInOutObjectType()->is<ArchetypeType>();
  }

  /// LValues, inout args, and Archetypes are implicitly indirect by
  /// virtue of their DWARF type.
  //
  // FIXME: Should this check if the lowered SILType is address only
  // instead? Otherwise optionals of archetypes etc will still have
  // 'isImplicitlyIndirect()' return false.
  bool isImplicitlyIndirect() const {
    return Type->isLValueType() || isArchetype() ||
      Type->is<InOutType>();
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
    return {};
  }
  static swift::irgen::DebugTypeInfo getTombstoneKey() {
    return swift::irgen::DebugTypeInfo(
        nullptr, llvm::DenseMapInfo<swift::TypeBase *>::getTombstoneKey(),
          nullptr, swift::irgen::Size(0), swift::irgen::Alignment(0));
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
