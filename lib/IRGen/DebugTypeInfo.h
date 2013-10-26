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
      /// Every Decl also has a type, but is otherwise preferred.
      PointerUnion<ValueDecl*, TypeBase*> DeclOrType;
      Size size;
      Alignment align;
      SILDebugScope *DebugScope;

      DebugTypeInfo()
        : size(0), align(1) {
      }
      // FIXME: retire the Type versions, they were useful for
      // bootstrapping, but don't work for generics.
      DebugTypeInfo(Type Ty, uint64_t SizeInBytes, uint32_t AlignInBytes);
      DebugTypeInfo(Type Ty, Size size, Alignment align);
      DebugTypeInfo(Type Ty, const TypeInfo &Info);
      DebugTypeInfo(ValueDecl *Decl, const TypeInfo &Info,
                    SILDebugScope *DS = nullptr);
      DebugTypeInfo(ValueDecl *Decl, Size Size, Alignment Align,
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
                                         ::getTombstoneKey(), 0, 0);
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
