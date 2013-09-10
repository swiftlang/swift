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

namespace llvm {
  class Type;
}

namespace swift {
  namespace irgen {
    class Alignment;
    class Size;
    class TypeInfo;

    /// This data structure holds all the debug info we want to emit
    /// for types. It also provides a useful abstraction while we
    /// decide on what type of debug info we want to emit for types.
    class DebugTypeInfo {
    public:
      /// Every Decl also has a type, but is otherwise preferred.
      PointerUnion<ValueDecl*, TypeBase*> DeclOrType;
      //DeclContext *DeclContext;
      uint64_t SizeInBytes;
      uint64_t AlignInBytes;

      DebugTypeInfo()
        : SizeInBytes(0), AlignInBytes(0) {
      }
      DebugTypeInfo(Type Ty, uint64_t Size, uint64_t Align);
      DebugTypeInfo(Type Ty, Size Size, Alignment Align);
      DebugTypeInfo(Type Ty, const TypeInfo &Info);
      DebugTypeInfo(ValueDecl *Decl, const TypeInfo &Info);
      DebugTypeInfo(ValueDecl *Decl, Size Size, Alignment Align);
      inline TypeBase* getHash() const { return getType(); } //Ty.getPointer(); }
      inline TypeBase* getType() const {
        if (DeclOrType.isNull())
          return nullptr;

        auto Ty = DeclOrType.dyn_cast<TypeBase*>();
        if (Ty) return Ty;

        return DeclOrType.get<ValueDecl*>()->getType().getPointer();
      }
      inline ValueDecl* getDecl() const {
        return DeclOrType.dyn_cast<ValueDecl*>();
      }
      inline bool isNull() const { return DeclOrType.isNull(); }
      bool operator==(DebugTypeInfo T) const;
      bool operator!=(DebugTypeInfo T) const;

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
