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
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"
#include "IRGen.h"

namespace llvm {
  class Constant;
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

  /// The 'universal' strategy: all modules must agree on the layout.
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

class NonFixedOffsetsImpl;

/// The type to pass around for non-fixed offsets.
typedef Optional<NonFixedOffsetsImpl*> NonFixedOffsets;

/// An abstract class for determining non-fixed offsets.
class NonFixedOffsetsImpl {
protected:
  virtual ~NonFixedOffsetsImpl() = default;
public:
  /// Return the offset (in bytes, as a size_t) of the element with
  /// the given index.
  virtual llvm::Value *getOffsetForIndex(IRGenFunction &IGF,
                                         unsigned index) = 0;

  operator NonFixedOffsets() { return NonFixedOffsets(this); }
};

/// An element layout is the layout for a single element of some sort
/// of aggregate structure.
class ElementLayout {
public:
  enum class Kind {
    /// The element is known to require no storage in the aggregate.
    Empty,

    /// The element can be positioned at a fixed offset within the
    /// aggregate.
    Fixed,

    /// The element cannot be positioned at a fixed offset within the
    /// aggregate.
    NonFixed,

    /// The element is an object lacking a fixed size but located at
    /// offset zero.  This is necessary because LLVM forbids even a
    /// 'gep 0' on an unsized type.
    InitialNonFixedSize
  };

private:
  enum : unsigned { IncompleteKind  = 4 };

  /// The swift type information for this element.
  const TypeInfo *Type;

  /// The offset in bytes from the start of the struct.
  unsigned ByteOffset;

  /// The index of this element, either in the LLVM struct (if fixed)
  /// or in the non-fixed elements array (if non-fixed).
  unsigned Index : 28;

  /// Whether this element is known to be POD in the local resilience
  /// domain.
  unsigned IsPOD : 1;

  /// The kind of layout performed for this element.
  unsigned TheKind : 3;

  explicit ElementLayout(const TypeInfo &type)
    : Type(&type), TheKind(IncompleteKind) {}

  bool isCompleted() const {
    return (TheKind != IncompleteKind);
  }

public:
  static ElementLayout getIncomplete(const TypeInfo &type) {
    return ElementLayout(type);
  }

  void completeFrom(const ElementLayout &other) {
    assert(!isCompleted());
    TheKind = other.TheKind;
    IsPOD = other.IsPOD;
    ByteOffset = other.ByteOffset;
    Index = other.Index;
  }

  void completeEmpty(IsPOD_t isPOD) {
    TheKind = unsigned(Kind::Empty);
    IsPOD = unsigned(isPOD);
    Index = 0; // make a complete write of the bitfield
  }

  void completeInitialNonFixedSize(IsPOD_t isPOD) {
    TheKind = unsigned(Kind::InitialNonFixedSize);
    IsPOD = unsigned(isPOD);
    Index = 0; // make a complete write of the bitfield
  }

  void completeFixed(IsPOD_t isPOD, Size byteOffset, unsigned structIndex) {
    TheKind = unsigned(Kind::Fixed);
    IsPOD = unsigned(isPOD);
    ByteOffset = byteOffset.getValue();
    Index = structIndex;

    assert(getByteOffset() == byteOffset);
  }

  /// Complete this element layout with a non-fixed offset.
  ///
  /// \param nonFixedElementIndex - the index into the elements array
  void completeNonFixed(IsPOD_t isPOD, unsigned nonFixedElementIndex) {
    TheKind = unsigned(Kind::NonFixed);
    IsPOD = unsigned(isPOD);
    Index = nonFixedElementIndex;
  }

  const TypeInfo &getType() const { return *Type; }

  Kind getKind() const {
    assert(isCompleted());
    return Kind(TheKind);
  }

  /// Is this element known to be empty?
  bool isEmpty() const {
    return getKind() == Kind::Empty;
  }

  /// Is this element known to be POD?
  IsPOD_t isPOD() const {
    assert(isCompleted());
    return IsPOD_t(IsPOD);
  }

  /// Given that this element has a fixed offset, return that offset in bytes.
  Size getByteOffset() const {
    assert(isCompleted() && getKind() == Kind::Fixed);
    return Size(ByteOffset);
  }

  /// Given that this element has a fixed offset, return the index in
  /// the LLVM struct.
  unsigned getStructIndex() const {
    assert(isCompleted() && getKind() == Kind::Fixed);
    return Index;
  }

  /// Given that this element does not have a fixed offset, return its
  /// index in the nonfixed-elements array.
  unsigned getNonFixedElementIndex() const {
    assert(isCompleted() && getKind() == Kind::NonFixed);
    return Index;
  }

  Address project(IRGenFunction &IGF, Address addr,
                  NonFixedOffsets offsets,
                  const llvm::Twine &suffix = "") const;
};

/// A class for building a structure layout.
class StructLayoutBuilder {
protected:
  IRGenModule &IGM;
private:
  SmallVector<llvm::Type*, 8> StructFields;
  Size CurSize = Size(0);
  Alignment CurAlignment = Alignment(1);
  llvm::BitVector CurSpareBits;
  unsigned NextNonFixedOffsetIndex = 0;
  bool IsFixedLayout = true;
  IsPOD_t IsKnownPOD = IsPOD;
  IsBitwiseTakable_t IsKnownBitwiseTakable = IsBitwiseTakable;
public:
  StructLayoutBuilder(IRGenModule &IGM) : IGM(IGM) {}

  /// Add a swift heap header to the layout.  This must be the first
  /// call to the layout.
  void addHeapHeader();

  /// Add a number of fields to the layout.  The field layouts need
  /// only have the TypeInfo set; the rest will be filled out.
  ///
  /// Returns true if the fields may have increased the storage
  /// requirements of the layout.
  bool addFields(llvm::MutableArrayRef<ElementLayout> fields,
                 LayoutStrategy strategy);

  /// Return whether the layout is known to be empty.
  bool empty() const { return IsFixedLayout && CurSize == Size(0); }

  /// Return the current set of fields.
  ArrayRef<llvm::Type *> getStructFields() const { return StructFields; }

  /// Return whether the structure has a fixed-size layout.
  bool isFixedLayout() const { return IsFixedLayout; }

  /// Return whether the structure is known to be POD in the local
  /// resilience scope.
  IsPOD_t isKnownPOD() const { return IsKnownPOD; }

  /// Return whether the structure is known to be bitwise-takable in the local
  /// resilience scope.
  IsBitwiseTakable_t isKnownBitwiseTakable() const {
    return IsKnownBitwiseTakable;
  }

  /// Return the size of the structure built so far.
  Size getSize() const { return CurSize; }

  /// Return the alignment of the structure built so far.
  Alignment getAlignment() const { return CurAlignment; }
  
  /// Return the spare bit mask of the structure built so far.
  const llvm::BitVector &getSpareBits() const { return CurSpareBits; }

  /// Return the spare bit mask of the structure built so far.
  llvm::BitVector &getSpareBits() { return CurSpareBits; }

  /// Build the current elements as a new anonymous struct type.
  llvm::StructType *getAsAnonStruct() const;

  /// Build the current elements as a new anonymous struct type.
  void setAsBodyOfStruct(llvm::StructType *type) const;

private:
  void addFixedSizeElement(ElementLayout &elt);
  void addNonFixedSizeElement(ElementLayout &elt);
  void addEmptyElement(ElementLayout &elt);

  void addElementAtFixedOffset(ElementLayout &elt);
  void addElementAtNonFixedOffset(ElementLayout &elt);
  void addNonFixedSizeElementAtOffsetZero(ElementLayout &elt);
};

/// A struct layout is the result of laying out a complete structure.
class StructLayout {
  /// The statically-known minimum bound on the alignment.
  Alignment MinimumAlign;

  /// The statically-known minimum bound on the size.
  Size MinimumSize;
  
  /// The statically-known spare bit mask.
  llvm::BitVector SpareBits;

  /// Whether this layout is fixed in size.  If so, the size and
  /// alignment are exact.
  bool IsFixedLayout;

  /// Whether the elements in this layout are all POD.
  IsPOD_t IsKnownPOD;
  /// Whether the elements in this layout are all bitwise-takable.
  IsBitwiseTakable_t IsKnownBitwiseTakable;
  
  llvm::Type *Ty;
  SmallVector<ElementLayout, 8> Elements;

  /// Use the spare bit mask from the builder if there are any fixed spare bits to
  /// speak of.
  static llvm::BitVector getSpareBitsFromBuilder(const StructLayoutBuilder &b) {
    return b.isFixedLayout() && b.getSpareBits().any()
      ? b.getSpareBits()
      : llvm::BitVector{};
  }
  
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
               ArrayRef<const TypeInfo *> fields,
               llvm::StructType *typeToFill = 0);

  /// Create a structure layout from a builder.
  StructLayout(const StructLayoutBuilder &builder,
               llvm::Type *type,
               ArrayRef<ElementLayout> elements)
    : MinimumAlign(builder.getAlignment()),
      MinimumSize(builder.getSize()),
      SpareBits(getSpareBitsFromBuilder(builder)),
      IsFixedLayout(builder.isFixedLayout()),
      IsKnownPOD(builder.isKnownPOD()),
      IsKnownBitwiseTakable(builder.isKnownBitwiseTakable()),
      Ty(type),
      Elements(elements.begin(), elements.end()) {}

  /// Return the element layouts.  This is parallel to the fields
  /// passed in the constructor.
  ArrayRef<ElementLayout> getElements() const { return Elements; }
  llvm::Type *getType() const { return Ty; }
  Size getSize() const { return MinimumSize; }
  Alignment getAlignment() const { return MinimumAlign; }
  const llvm::BitVector &getSpareBits() const { return SpareBits; }
  llvm::BitVector &getSpareBits() { return SpareBits; }
  bool isKnownEmpty() const { return isFixedLayout() && MinimumSize.isZero(); }
  IsPOD_t isKnownPOD() const { return IsKnownPOD; }
  IsBitwiseTakable_t isKnownBitwiseTakable() const {
    return IsKnownBitwiseTakable;
  }

  bool isFixedLayout() const { return IsFixedLayout; }
  llvm::Constant *emitSize(IRGenModule &IGM) const;
  llvm::Constant *emitAlignMask(IRGenModule &IGM) const;

  /// Bitcast the given pointer to this type.
  Address emitCastTo(IRGenFunction &IGF, llvm::Value *ptr,
                     const llvm::Twine &name = "") const;
};

Size getHeapHeaderSize(IRGenModule &IGM);

void addHeapHeaderToLayout(IRGenModule &IGM, Size &size, Alignment &align,
                           SmallVectorImpl<llvm::Type*> &fieldTypes);

} // end namespace irgen
} // end namespace swift

#endif
