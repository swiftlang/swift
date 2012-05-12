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
#include "llvm/ADT/Twine.h"
#include "IRGen.h"

namespace llvm {
  class StructType;
  class Type;
  class Value;
}

namespace swift {
namespace irgen {
  class Address;
  class IRGenFunction;
  class IRGenModule;
  class TypeInfo;

/// An algorithm for laying out a structure.
enum class LayoutStrategy {
  /// Compute an optimal layout;  there are no constraints at all.
  Optimal,

  /// The 'universal' strategy: all translation units must agree on
  /// the layout.
  Universal
};

/// The kind of object being laid out.
enum class LayoutKind {
  /// A non-heap object does not require a heap header.
  NonHeapObject,

  /// A heap object is destined to be allocated on the heap and must
  /// be emitted with the standard heap header.
  HeapObject,
};

/// An element layout is the layout for a single element of a type.
struct ElementLayout {
  /// The offset in bytes from the start of the struct.
  Size ByteOffset;

  /// The index of this element in the LLVM struct.
  unsigned StructIndex;

  /// The swift type information for this element.
  const TypeInfo *Type;

  Address project(IRGenFunction &IGF, Address addr,
                  const llvm::Twine &suffix = "") const;
};

/// A struct layout is the result of laying out a complete structure.
class StructLayout {
  Alignment Align;
  Size TotalSize;
  llvm::Type *Ty;
  llvm::SmallVector<ElementLayout, 8> Elements;

public:
  /// Create a structure layout.
  ///
  /// \param strategy - how much leeway the algorithm has to rearrange
  ///   and combine the storage of fields
  /// \param kind - the kind of layout to perform, including whether the
  ///   layout must include the reference-counting header
  /// \param typeToFill - if present, must be an opaque type whose body
  ///   will be filled with this layout
  StructLayout(IRGenModule &IGM, LayoutKind kind, LayoutStrategy strategy,
               llvm::ArrayRef<const TypeInfo *> fields,
               llvm::StructType *typeToFill = 0);

  /// Return the element layouts.  This is parallel to the fields
  /// passed in the constructor.
  llvm::ArrayRef<ElementLayout> getElements() const { return Elements; }
  llvm::Type *getType() const { return Ty; }
  Size getSize() const { return TotalSize; }
  Alignment getAlignment() const { return Align; }
  bool empty() const { return TotalSize.isZero(); }

  bool hasStaticLayout() const { return true; }
  llvm::Value *emitSize(IRGenFunction &IGF) const;
  llvm::Value *emitAlign(IRGenFunction &IGF) const;
};

void addHeapHeaderToLayout(IRGenModule &IGM, Size &size, Alignment &align,
                           SmallVectorImpl<llvm::Type*> &fieldTypes);

} // end namespace irgen
} // end namespace swift

#endif
