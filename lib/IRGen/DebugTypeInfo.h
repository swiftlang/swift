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
class IRGenModule;

/// This data structure holds everything needed to emit debug info
/// for a type.
class DebugTypeInfo {
protected:
  /// The type we need to emit may be different from the type
  /// mentioned in the Decl, for example, stripped of qualifiers.
  TypeBase *Type = nullptr;
  /// Needed to determine the size of basic types and to determine
  /// the storage type for undefined variables.
  std::optional<uint32_t> NumExtraInhabitants;
  Alignment Align;
  bool DefaultAlignment = true;
  bool IsMetadataType = false;
  bool IsFixedBuffer = false;
  bool IsForwardDecl = false;

public:
  DebugTypeInfo() = default;
  DebugTypeInfo(swift::Type Ty, Alignment AlignInBytes = Alignment(1),
                bool HasDefaultAlignment = true, bool IsMetadataType = false,
                bool IsFixedBuffer = false,
                std::optional<uint32_t> NumExtraInhabitants = {});

  /// Create type for a local variable.
  static DebugTypeInfo getLocalVariable(VarDecl *Decl, swift::Type Ty,
                                        const TypeInfo &Info, IRGenModule &IGM);
  /// Create type for global type metadata.
  static DebugTypeInfo getGlobalMetadata(swift::Type Ty, Size size,
                                         Alignment align);
  /// Create type for an artificial metadata variable.
  static DebugTypeInfo getTypeMetadata(swift::Type Ty, Size size,
                                       Alignment align);

  /// Create a forward declaration for a type whose size is unknown.
  static DebugTypeInfo getForwardDecl(swift::Type Ty);

  /// Create a standalone type from a TypeInfo object.
  static DebugTypeInfo getFromTypeInfo(swift::Type Ty, const TypeInfo &Info,
                                       IRGenModule &IGM);
  /// Global variables.
  static DebugTypeInfo getGlobal(SILGlobalVariable *GV, IRGenModule &IGM);
  static DebugTypeInfo getGlobalFixedBuffer(SILGlobalVariable *GV,
                                            Alignment align, IRGenModule &IGM);
  /// ObjC classes.
  static DebugTypeInfo getObjCClass(ClassDecl *theClass, Size size,
                                    Alignment align);
  /// Error type.
  static DebugTypeInfo getErrorResult(swift::Type Ty, IRGenModule &IGM);

  TypeBase *getType() const { return Type; }

  TypeDecl *getDecl() const;

  // Determine whether this type is an Archetype dependent on a generic context.
  bool isContextArchetype() const {
    if (auto archetype =
            Type->getWithoutSpecifierType()->getAs<ArchetypeType>()) {
      return !isa<OpaqueTypeArchetypeType>(archetype);
    }
    return false;
  }

  Alignment getAlignment() const { return Align; }
  bool isNull() const { return !Type; }
  bool isMetadataType() const { return IsMetadataType; }
  bool hasDefaultAlignment() const { return DefaultAlignment; }
  bool isFixedBuffer() const { return IsFixedBuffer; }
  bool isForwardDecl() const { return IsForwardDecl; }
  std::optional<uint32_t> getNumExtraInhabitants() const {
    return NumExtraInhabitants;
  }

  bool operator==(DebugTypeInfo T) const;
  bool operator!=(DebugTypeInfo T) const;
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  LLVM_DUMP_METHOD void dump() const;
#endif
};

/// A DebugTypeInfo with a defined size (that may be 0).
class CompletedDebugTypeInfo : public DebugTypeInfo {
  Size::int_type SizeInBits;

  CompletedDebugTypeInfo(DebugTypeInfo DbgTy, Size::int_type SizeInBits)
    : DebugTypeInfo(DbgTy), SizeInBits(SizeInBits) {}

public:
  static std::optional<CompletedDebugTypeInfo>
  get(DebugTypeInfo DbgTy, std::optional<Size::int_type> SizeInBits) {
    if (!SizeInBits)
      return {};
    return CompletedDebugTypeInfo(DbgTy, *SizeInBits);
  }

  static std::optional<CompletedDebugTypeInfo>
  getFromTypeInfo(swift::Type Ty, const TypeInfo &Info, IRGenModule &IGM,
                  std::optional<Size::int_type> SizeInBits = {});

  Size::int_type getSizeInBits() const { return SizeInBits; }
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
        llvm::DenseMapInfo<swift::TypeBase *>::getTombstoneKey(),
        swift::irgen::Alignment(), /* HasDefaultAlignment = */ false);
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
