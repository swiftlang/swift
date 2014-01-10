//===--- DebugTypeInfo.h - Type Info for Debugging --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "IRGen.h"

namespace llvm {
  class Type;
}

namespace swift {
  class SILDebugScope;

  namespace irgen {
    class TypeInfo;

    /// This data structure holds all the debug info we want to emit
    /// for types. It also provides a useful abstraction while we
    /// decide on what type of debug info we want to emit for types.
    class DebugTypeInfo {
    public:
      /// Type info is stored in the form of a Decl or a Type+DeclContext.
      PointerUnion<ValueDecl*, TypeBase*> DeclOrType;
      DeclContext *DeclCtx;
      llvm::Type *StorageType;
      Size size;
      Alignment align;
      SILDebugScope *DebugScope;

      DebugTypeInfo()
        : StorageType(nullptr), size(0), align(1), DebugScope(nullptr) {
      }
      DebugTypeInfo(Type Ty, uint64_t SizeInBytes, uint32_t AlignInBytes,
                    DeclContext *DC);
      DebugTypeInfo(Type Ty, Size size, Alignment align,
                    DeclContext *DC);
      DebugTypeInfo(Type Ty, const TypeInfo &Info, DeclContext *DC);
      DebugTypeInfo(ValueDecl *Decl, const TypeInfo &Info,
                    SILDebugScope *DS = nullptr);
      DebugTypeInfo(ValueDecl *Decl, Size size, Alignment align,
                    SILDebugScope *DS = nullptr);
      inline TypeBase* getHash() const { return getType(); }
      inline TypeBase* getType() const {
        if (DeclOrType.isNull())
          return nullptr;

        if (auto Ty = DeclOrType.dyn_cast<TypeBase*>())
          return Ty;

        return DeclOrType.get<ValueDecl*>()->getType().getPointer();
      }
      inline ValueDecl* getDecl() const {
        return DeclOrType.dyn_cast<ValueDecl*>();
      }
      SILDebugScope *getDebugScope() const { return DebugScope; }
      DeclContext *getDeclContext() const {
        if (auto Decl = getDecl())
          return Decl->getDeclContext();
        return DeclCtx;
      }
      inline bool isNull() const { return DeclOrType.isNull(); }
      bool operator==(DebugTypeInfo T) const;
      bool operator!=(DebugTypeInfo T) const;

      void dump() const;
    };
  }
}

namespace llvm {

  // Dense map specialization.
  template<> struct DenseMapInfo<swift::irgen::DebugTypeInfo> {
    static swift::irgen::DebugTypeInfo getEmptyKey() {
      return swift::irgen::DebugTypeInfo();
    }
    static swift::irgen::DebugTypeInfo getTombstoneKey() {
      return swift::irgen::DebugTypeInfo(llvm::DenseMapInfo<swift::TypeBase*>
                                         ::getTombstoneKey(), 0, 0, 0);
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
