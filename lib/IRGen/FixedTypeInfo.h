//===--- FixedTypeInfo.h - Convenience for fixed-layout types ---*- C++ -*-===//
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
// This file defines FixedTypeInfo, which supplements the TypeInfo
// interface for classes with (at least locally) fixed-layout type
// implementations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_FIXEDTYPEINFO_H
#define SWIFT_IRGEN_FIXEDTYPEINFO_H

#include "TypeInfo.h"

namespace swift {
namespace irgen {

/// FixedTypeInfo - An abstract class designed for use when
/// implementing a type that has a statically known layout.
class FixedTypeInfo : public TypeInfo {
private:
  /// The storage size of this type in bytes.  This may be zero even
  /// for well-formed and complete types, such as a trivial oneof or
  /// tuple.
  Size StorageSize;

protected:
  FixedTypeInfo(llvm::Type *type, Size size, Alignment align, IsPOD_t pod)
    : TypeInfo(type, align, pod, IsFixedSize), StorageSize(size) {}

public:
  // This is useful for metaprogramming.
  static bool isFixed() { return true; }

  /// Whether this type is known to be empty.
  bool isKnownEmpty() const { return StorageSize.isZero(); }

  OwnedAddress allocate(IRGenFunction &IGF, OnHeap_t onHeap,
                        const llvm::Twine &name) const;

  // We can give these reasonable default implementations.

  void initializeWithTake(IRGenFunction &IGF, Address destAddr,
                          Address srcAddr) const;

  void initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                          Address srcAddr) const;

  std::pair<llvm::Value*, llvm::Value*>
  getSizeAndAlignment(IRGenFunction &IGF) const;
  llvm::Value *getSize(IRGenFunction &IGF) const;
  llvm::Value *getAlignment(IRGenFunction &IGF) const;
  llvm::Value *getStride(IRGenFunction &IGF) const;

  llvm::Constant *getStaticSize(IRGenModule &IGM) const;
  llvm::Constant *getStaticAlignment(IRGenModule &IGM) const;
  llvm::Constant *getStaticStride(IRGenModule &IGM) const;

  void completeFixed(Size size, Alignment alignment) {
    StorageSize = size;
    setStorageAlignment(alignment);
  }

  /// Returns the known, fixed size required to store a value of this type.
  Alignment getFixedAlignment() const {
    return getBestKnownAlignment();
  }

  /// Returns the known, fixed alignment of a stored value of this type.
  Size getFixedSize() const {
    return StorageSize;
  }

  /// Returns the (assumed fixed) stride of the storage for this
  /// object.  The stride is the storage size rounded up to the
  /// alignment; its practical use is that, in an array, it is the
  /// offset from the size of one element to the offset of the next.
  Size getFixedStride() const {
    return StorageSize.roundUpToAlignment(getFixedAlignment());
  }

  static bool classof(const TypeInfo *type) { return type->isFixedSize(); }
};

}
}

#endif
