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
      CanType CanTy;
      uint64_t SizeInBits;
      uint64_t AlignmentInBits;

      DebugTypeInfo()
        : CanTy(),  SizeInBits(0), AlignmentInBits(0) {
      }
      DebugTypeInfo(CanType CTy, uint64_t Size, uint64_t Align);
      DebugTypeInfo(CanType CTy, Size Size, Alignment Align);
      DebugTypeInfo(CanType CTy, const TypeInfo &Info);
      DebugTypeInfo(const ValueDecl &Decl, const TypeInfo &Info);

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
      return swift::irgen::DebugTypeInfo(llvm::DenseMapInfo<
                                         swift::CanType>::getTombstoneKey(),
                                         0, 0);
    }
    static unsigned getHashValue(swift::irgen::DebugTypeInfo Val) {
      return DenseMapInfo<swift::CanType>::getHashValue(Val.CanTy);
    }
    static bool isEqual(swift::irgen::DebugTypeInfo LHS,
                        swift::irgen::DebugTypeInfo RHS) {
      return LHS == RHS;
    }
  };
}

#endif
