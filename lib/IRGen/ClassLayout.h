//===--- ClassLayout.h - Class instance layout ------------------*- C++ -*-===//
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
// This file defines some routines that are useful for calculating class
// instance layout.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CLASSLAYOUT_H
#define SWIFT_IRGEN_CLASSLAYOUT_H

#include "llvm/ADT/ArrayRef.h"
#include "IRGen.h"
#include "StructLayout.h"

namespace swift {
namespace irgen {

/// Different policies for accessing a physical field.
enum class FieldAccess : uint8_t {
  /// Instance variable offsets are constant.
  ConstantDirect,
  
  /// Instance variable offsets must be loaded from "direct offset"
  /// global variables.
  NonConstantDirect,
  
  /// Instance variable offsets are kept in fields in metadata, but
  /// the offsets of those fields within the metadata are constant.
  ConstantIndirect
};

class ClassLayout {
  /// The statically-known minimum bound on the alignment.
  Alignment MinimumAlign;

  /// The statically-known minimum bound on the size.
  Size MinimumSize;

  /// Whether this layout is fixed in size. If so, the size and
  /// alignment are exact.
  bool IsFixedLayout;

  /// Do instances of this class have a size and layout known at compile time?
  ///
  /// Note: This is a stronger condition than having a fixed layout. The latter
  /// is true even when the class requires sliding ivars by the Objective-C
  /// runtime.
  bool IsFixedSize;

  /// Does the class metadata require initialization?
  bool MetadataRequiresInitialization;

  /// Does the class metadata require relocation?
  bool MetadataRequiresRelocation;

  /// The LLVM type for instances of this class.
  llvm::Type *Ty;

  /// Lazily-initialized array of all fragile stored properties directly defined
  /// in the class itself.
  ArrayRef<VarDecl *> AllStoredProperties;

  /// Lazily-initialized array of all field access methods.
  ArrayRef<FieldAccess> AllFieldAccesses;

  /// Fixed offsets of fields, if known (does not take Objective-C sliding into
  /// account).
  ArrayRef<ElementLayout> AllElements;

public:
  ClassLayout(const StructLayoutBuilder &builder,
              bool isFixedSize,
              bool metadataRequiresInitialization,
              bool metadataRequiresRelocation,
              llvm::Type *classTy,
              ArrayRef<VarDecl *> allStoredProps,
              ArrayRef<FieldAccess> allFieldAccesses,
              ArrayRef<ElementLayout> allElements);

  Size getInstanceStart() const;

  llvm::Type *getType() const { return Ty; }
  Size getSize() const { return MinimumSize; }
  Alignment getAlignment() const { return MinimumAlign; }
  Size getAlignMask() const { return getAlignment().asSize() - Size(1); }

  bool isFixedLayout() const { return IsFixedLayout; }

  bool isFixedSize() const { return IsFixedSize; }

  bool doesMetadataRequireInitialization() const {
    return MetadataRequiresInitialization;
  }

  bool doesMetadataRequireRelocation() const {
    return MetadataRequiresRelocation;
  }

  std::pair<FieldAccess, ElementLayout>
  getFieldAccessAndElement(VarDecl *field) const {
    // FIXME: This is algorithmically terrible.
    auto found = std::find(AllStoredProperties.begin(),
                           AllStoredProperties.end(), field);
    assert(found != AllStoredProperties.end() && "didn't find field in type?!");
    unsigned index = found - AllStoredProperties.begin();

    return std::make_pair(AllFieldAccesses[index], AllElements[index]);
  }
};

} // end namespace irgen
} // end namespace swift

#endif
