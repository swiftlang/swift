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

/// A set of flags describing properties of a class's metadata layout.
/// The presence or absence of these flags determines how much static
/// knowledge the compiler has of the layout of this class and its
/// metadata, which in turn will determine the strategy used to emit
/// and initialize class metadata.
enum class ClassMetadataFlags {
  /// Does the class or any of its superclasses have stored properties that
  /// where dropped due to the Swift language version availability of
  /// their types?
  ClassHasMissingMembers = (1 << 0),

  /// Does the class or any of its fragile superclasses have stored
  /// properties of unknown size, which do *not* depend on generic
  /// parameters?
  ///
  /// This is different from the class itself being resilient or
  /// having resilient ancestry, because we still have a fixed layout
  /// for the class metadata in this case.
  ///
  /// In fact, for a class with resilient ancestry, this can still be
  /// false if all of the fields known to us are fixed size.
  ClassHasResilientMembers = (1 << 1),

  /// Is this class or any of its superclasses generic?
  ClassHasGenericAncestry = (1 << 2),

  /// Is this class itself generic via the Swift generic system, ie. not a
  /// lightweight Objective-C generic class?
  ClassIsGeneric = (1 << 3),

  /// Does the class layout depend on the size or alignment of its
  /// generic parameters?
  ///
  /// This can be the case if the class has generic resilient ancestry
  /// that depends on the class's generic parameters, of it it has
  /// fields of generic type that are not fixed size.
  ClassHasGenericLayout = (1 << 4),

  /// Is this class or any of its superclasses resilient from the viewpoint
  /// of the current module? This means that their metadata can change size,
  /// hence field offsets, generic arguments and virtual methods must be
  /// accessed relative to a metadata base global variable.
  ///
  /// Note that a @_fixed_layout class in a resilient module still has
  /// resilient metadata, so any subclasses will have this flag set;
  /// to check for resilient stored property layout, check for
  /// ClassHasResilientMembers.
  ClassHasResilientAncestry = (1 << 5),

  /// Are any of this class's superclasses defined in Objective-C?
  /// This means that field offsets must be loaded from field offset globals
  /// or the field offset vector in the metadata, and the Objective-C runtime
  /// will slide offsets based on the actual superclass size, which is not
  /// known at compile time.
  ClassHasObjCAncestry = (1 << 6)
};

using ClassMetadataOptions = OptionSet<ClassMetadataFlags>;

class ClassLayout {
  /// The statically-known minimum bound on the alignment.
  Alignment MinimumAlign;

  /// The statically-known minimum bound on the size.
  Size MinimumSize;

  /// Whether this layout is fixed in size. If so, the size and
  /// alignment are exact.
  bool IsFixedLayout;

  ClassMetadataOptions Options;

  /// The LLVM type for instances of this class.
  llvm::Type *Ty;

  /// The header size of this class.
  Size HeaderSize;

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
              ClassMetadataOptions options,
              llvm::Type *classTy,
              ArrayRef<VarDecl *> allStoredProps,
              ArrayRef<FieldAccess> allFieldAccesses,
              ArrayRef<ElementLayout> allElements,
              Size headerSize);

  Size getInstanceStart() const;

  llvm::Type *getType() const { return Ty; }
  Size getSize() const { return MinimumSize; }
  Alignment getAlignment() const { return MinimumAlign; }
  Size getAlignMask() const { return getAlignment().asSize() - Size(1); }

  bool isFixedLayout() const { return IsFixedLayout; }

  /// Returns true if the stored property layout of instances of this class
  /// is known at compile time.
  ///
  /// Note that ClassHasResilientAncestry or ClassHasGenericAncestry might
  /// still be true; the former means the class has resilient metadata, so
  /// it might still be @_fixed_layout; the latter means we have a generic
  /// superclass, but it doesn't mean the layout actually depends on any
  /// generic parameters.
  bool isFixedSize() const {
    return !(Options.contains(ClassMetadataFlags::ClassHasMissingMembers) ||
             Options.contains(ClassMetadataFlags::ClassHasResilientMembers) ||
             Options.contains(ClassMetadataFlags::ClassHasGenericLayout) ||
             Options.contains(ClassMetadataFlags::ClassHasObjCAncestry));
  }

  /// Returns true if the runtime may attempt to assign non-zero offsets to
  /// empty fields for this class.  The ObjC runtime will do this if it
  /// decides it needs to slide ivars.  This is the one exception to the
  /// general rule that the runtime will not try to assign a different offset
  /// than was computed statically for a field with a fixed offset.
  bool mayRuntimeAssignNonZeroOffsetsToEmptyFields() const {
    return Options.contains(ClassMetadataFlags::ClassHasObjCAncestry);
  }

  /// Returns true iff everything about the class metadata layout is statically
  /// known except field offsets and the instance size and alignment.
  ///
  /// Will assert if the class metadata is "more" dynamic; you must check
  /// doesMetadataRequireRelocation() and doesMetadataRequireInitialization()
  /// first.
  bool doesMetadataRequireUpdate() const {
    assert(!doesMetadataRequireInitialization());
    return (Options.contains(ClassMetadataFlags::ClassHasResilientMembers) ||
            Options.contains(ClassMetadataFlags::ClassHasMissingMembers));
  }

  /// Returns true iff everything about the class metadata layout is statically
  /// known except the superclass field must be instantiated at runtime because
  /// it is a generic class type.
  ///
  /// Will assert if the class metadata is "more" dynamic; you must check
  /// doesMetadataRequireRelocation() first.
  bool doesMetadataRequireInitialization() const {
    assert(!doesMetadataRequireRelocation());
    return Options.contains(ClassMetadataFlags::ClassHasGenericAncestry);
  }

  /// Returns true if the class metadata must be built at runtime because its
  /// size is not known at compile time. This is the most general case.
  bool doesMetadataRequireRelocation() const {
    return (Options.contains(ClassMetadataFlags::ClassHasResilientAncestry) ||
            Options.contains(ClassMetadataFlags::ClassIsGeneric));
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
