//===--- GenHeap.h - Heap-object layout and management ----------*- C++ -*-===//
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
// This file defines some routines that are useful for emitting
// operations on heap objects and their metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENHEAP_H
#define SWIFT_IRGEN_GENHEAP_H

#include "StructLayout.h"

namespace llvm {
  class Constant;
  template <class T> class SmallVectorImpl;
}

namespace swift {
namespace irgen {
  class Address;

/// A heap layout is the result of laying out a complete structure for
/// heap-allocation.
class HeapLayout : public StructLayout {
public:
  HeapLayout(IRGenModule &IGM, LayoutStrategy strategy,
             llvm::ArrayRef<const TypeInfo *> fields);

  /// Add the layout's required metadata fields to the given collection.
  /// The address point of the metadata should be at the first field added.
  ///
  /// It may be wasteful to call this multiple times.
  void buildMetadataInto(IRGenModule &IGM,
                   llvm::SmallVectorImpl<llvm::Constant*> &metadata) const;

  /// As a convenience, build a metadata object with internal linkage
  /// consisting solely of the standard heap metadata.
  llvm::Constant *getPrivateMetadata(IRGenModule &IGM) const ;


  /// Given that an object has been allocated, cast the result to the
  /// appropriate type.
  Address emitCastOfAlloc(IRGenFunction &IGF, llvm::Value *alloc,
                          const llvm::Twine &name = "") const;
};

} // end namespace irgen
} // end namespace swift

#endif
