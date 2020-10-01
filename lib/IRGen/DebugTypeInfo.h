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
  /// The type we need to emit may be different from the type
  /// mentioned in the Decl, for example, stripped of qualifiers.
  TypeBase *Type = nullptr;
  /// Needed to determine the size of basic types and to determine
  /// the storage type for undefined variables.
  llvm::Type *StorageType = nullptr;
  Size size = Size(0);
  Alignment align = Alignment();
  bool DefaultAlignment = true;
  bool IsMetadataType = false;

public:
  DebugTypeInfo() = default;
  DebugTypeInfo(swift::Type Ty, llvm::Type *StorageTy, Size SizeInBytes,
                Alignment AlignInBytes, bool HasDefaultAlignment,
                bool IsMetadataType);

  /// Create type for a local variable.
  static DebugTypeInfo getLocalVariable(VarDecl *Decl,
                                        swift::Type Ty, const TypeInfo &Info);
  /// Create type for global type metadata.
  static DebugTypeInfo getMetadata(swift::Type Ty, llvm::Type *StorageTy,
                                   Size size, Alignment align);
  /// Create type for an artificial metadata variable.
  static DebugTypeInfo getArchetype(swift::Type Ty, llvm::Type *StorageTy,
                                    Size size, Alignment align);

  /// Create a forward declaration for a type whose size is unknown.
  static DebugTypeInfo getForwardDecl(swift::Type Ty);

  /// Create a standalone type from a TypeInfo object.
  static DebugTypeInfo getFromTypeInfo(swift::Type Ty, const TypeInfo &Info);
  /// Global variables.
  static DebugTypeInfo getGlobal(SILGlobalVariable *GV, llvm::Type *StorageType,
                                 Size size, Alignment align);
  /// ObjC classes.
  static DebugTypeInfo getObjCClass(ClassDecl *theClass,
                                    llvm::Type *StorageType, Size size,
                                    Alignment align);
  /// Error type.
  static DebugTypeInfo getErrorResult(swift::Type Ty, llvm::Type *StorageType,
                                      Size size, Alignment align);

  TypeBase *getType() const { return Type; }

  TypeDecl *getDecl() const;

  // Determine whether this type is an Archetype dependent on a generic context.
  bool isContextArchetype() const {
    if (auto archetype = Type->getWithoutSpecifierType()->getAs<ArchetypeType>()) {
      return !isa<OpaqueTypeArchetypeType>(archetype->getRoot());
    }
    return false;
  }

  llvm::Type *getStorageType() const {
    assert((StorageType || size.isZero()) &&
           "only defined types may have a size");
    return StorageType;
  }
  Size getSize() const { return size; }
  void setSize(Size NewSize) { size = NewSize; }
  Alignment getAlignment() const { return align; }
  bool isNull() const { return Type == nullptr; }
  bool isForwardDecl() const { return StorageType == nullptr; }
  bool isMetadataType() const { return IsMetadataType; }
  bool hasDefaultAlignment() const { return DefaultAlignment; }
  
  bool operator==(DebugTypeInfo T) const;
  bool operator!=(DebugTypeInfo T) const;
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  LLVM_DUMP_METHOD void dump() const;
#endif
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
        llvm::DenseMapInfo<swift::TypeBase *>::getTombstoneKey(), nullptr,
        swift::irgen::Size(0), swift::irgen::Alignment(), false, false);
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
