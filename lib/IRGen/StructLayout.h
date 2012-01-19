//===--- StructLayout.h - Structure layout ----------------------*- C++ -*-===//
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
// This file defines some routines that are useful for performing
// structure layout.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_STRUCTLAYOUT_H
#define SWIFT_IRGEN_STRUCTLAYOUT_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "IRGen.h"

namespace llvm {
  class Type;
}

namespace swift {
namespace irgen {
  class IRGenModule;
  class TypeInfo;

/// An algorithm for laying out a structure.
enum class LayoutStrategy {
  /// Compute an optimal layout;  there are no constraints at all.
  Optimal
};

/// An element layout is the layout for a single element of a type.
struct ElementLayout {
  /// The offset in bytes from the start of the struct.
  Size ByteOffset;

  /// The index of this element in the LLVM struct.
  unsigned StructIndex;
};

/// A struct layout is the result of laying out a complete structure.
class StructLayout {
  Alignment Align;
  Size TotalSize;
  llvm::Type *Ty;
  llvm::SmallVector<ElementLayout, 8> Elements;

public:
  StructLayout(IRGenModule &IGM, LayoutStrategy strategy,
               llvm::ArrayRef<const TypeInfo *> fields);

  /// Return the element layouts.  This is parallel to the fields
  /// passed in the constructor.
  llvm::ArrayRef<ElementLayout> getElements() const { return Elements; }
  llvm::Type *getType() const { return Ty; }
  Size getSize() const { return TotalSize; }
  Alignment getAlignment() const { return Align; }
  bool empty() const { return TotalSize.isZero(); }
};

} // end namespace irgen
} // end namespace swift

#endif
