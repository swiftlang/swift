//===--- MetadataLayout.h - Type metadata layout ----------------*- C++ -*-===//
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
// Information recording the layout of type metadata objects.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_METADATALAYOUT_H
#define SWIFT_IRGEN_METADATALAYOUT_H

#include "IRGen.h"
#include "swift/SIL/SILDeclRef.h"

namespace swift {
class ClassDecl;
class EnumDecl;
class StructDecl;
class VarDecl;

namespace irgen {
class Address;
class IRGenFunction;
class IRGenModule;

/// The total size and address point of a metadata object.
struct MetadataSize {
  Size FullSize;
  Size AddressPoint;

  /// Return the offset from the address point to the end of the
  /// metadata object.
  Size getOffsetToEnd() const {
    return FullSize - AddressPoint;
  }
};

/// A base class for various kinds of metadata layout.
class MetadataLayout {
public:
  enum class Kind {
    Class,
    Struct,
    Enum,
    ForeignClass,
  };

  class StoredOffset {
  public:
    enum Kind {
      /// The high bits are an integer displacement.
      Static = 0,

      /// The high bits are an integer displacement relative to a offset stored
      /// in a class metadata base offset global variable. This is used to
      /// access members of class metadata where the superclass is resilient to
      /// us, and therefore has an unknown size.
      Dynamic,
    };

  private:
    enum : uint64_t {
      KindBits = 1,
      KindMask = (1 << KindBits) - 1,
      PayloadMask = ~uint64_t(KindMask)
    };

    mutable uintptr_t Data;
  public:
    StoredOffset() : Data(0) {}
    explicit StoredOffset(Size offset, Kind kind)
      : Data((static_cast<uint64_t>(offset.getValue()) << KindBits) | kind) {
      assert(kind == Kind::Dynamic || !offset.isZero() &&
             "cannot store a zero static offset");
      if (kind == Kind::Static)
        assert(getStaticOffset() == offset && "overflow");
      if (kind == Kind::Dynamic)
        assert(getRelativeOffset() == offset && "overflow");
    }

    bool isValid() const { return Data != 0; }
    bool isStatic() const { return isValid() && (Data & KindMask) == Static; }
    bool isDynamic() const { return (Data & KindMask) == Dynamic; }

    /// If this is a metadata offset into a resilient class, returns the offset
    /// relative to the size of the superclass metadata.
    Size getRelativeOffset() const {
      assert(isDynamic());
      return Size(static_cast<int64_t>(Data) >> KindBits);
    }

    /// Returns the offset relative to start of metadata. Only used for
    /// metadata fields whose offset is completely known at compile time.
    Size getStaticOffset() const {
      assert(isStatic());
      return Size(static_cast<int64_t>(Data) >> KindBits);
    }
  };

private:
  Kind TheKind;

protected:
  MetadataSize TheSize;

  MetadataLayout(Kind theKind) : TheKind(theKind) {}

  MetadataLayout(const MetadataLayout &other) = delete;
  MetadataLayout &operator=(const MetadataLayout &other) = delete;

public:
  /// Destruct and deallocate this layout object.
  void destroy() const;

  Kind getKind() const { return TheKind; }

  MetadataSize getSize() const { return TheSize; }
};

/// Base class for nominal type metadata layouts.
class NominalMetadataLayout : public MetadataLayout {
protected:
  NominalTypeDecl *Nominal;
  StoredOffset GenericRequirements;

  NominalMetadataLayout(Kind kind, NominalTypeDecl *nominal)
      : MetadataLayout(kind), Nominal(nominal) {}

  Offset emitOffset(IRGenFunction &IGF, StoredOffset offset) const;

public:
  NominalTypeDecl *getDecl() const {
    return Nominal;
  }

  bool hasGenericRequirements() const {
    return GenericRequirements.isValid();
  }

  /// Should only be used when emitting the nominal type descriptor.
  Size getStaticGenericRequirementsOffset() const;

  Offset getGenericRequirementsOffset(IRGenFunction &IGF) const;

  static bool classof(const MetadataLayout *layout) {
    switch (layout->getKind()) {
    case MetadataLayout::Kind::Class:
    case MetadataLayout::Kind::Enum:
    case MetadataLayout::Kind::Struct:
      return true;

    case MetadataLayout::Kind::ForeignClass:
      return false;
    }
  }
};

/// Layout for class type metadata.
class ClassMetadataLayout : public NominalMetadataLayout {
public:
  class MethodInfo {
    Offset TheOffset;
  public:
    MethodInfo(Offset offset)
      : TheOffset(offset) {}
    Offset getOffset() const { return TheOffset; }
  };

private:
  bool HasResilientSuperclass = false;

  StoredOffset StartOfImmediateMembers;

  StoredOffset MetadataSize;
  StoredOffset MetadataAddressPoint;

  StoredOffset InstanceSize;
  StoredOffset InstanceAlignMask;

  struct StoredMethodInfo {
    StoredOffset TheOffset;
    StoredMethodInfo(StoredOffset offset) : TheOffset(offset) {}
  };
  llvm::DenseMap<SILDeclRef, StoredMethodInfo> MethodInfos;

  /// Field offsets for various fields.
  llvm::DenseMap<VarDecl*, StoredOffset> FieldOffsets;

  /// The start of the vtable.
  StoredOffset VTableOffset;

  /// The start of the field-offset vector.
  StoredOffset FieldOffsetVector;

  /// The number of members to add after superclass metadata.
  unsigned NumImmediateMembers;

  const StoredMethodInfo &getStoredMethodInfo(SILDeclRef method) const {
    auto it = MethodInfos.find(method);
    assert(it != MethodInfos.end());
    return it->second;
  }

  const StoredOffset &getStoredFieldOffset(VarDecl *field) const {
    auto it = FieldOffsets.find(field);
    assert(it != FieldOffsets.end());
    return it->second;
  }

  friend class IRGenModule;
  ClassMetadataLayout(IRGenModule &IGM, ClassDecl *theClass);

public:
  ClassDecl *getDecl() const {
    return cast<ClassDecl>(Nominal);
  }

  bool hasResilientSuperclass() const {
    return HasResilientSuperclass;
  }

  constexpr static bool areImmediateMembersNegative() {
    return false;
  }

  Size getMetadataSizeOffset() const;

  Size getMetadataAddressPointOffset() const;

  Size getInstanceSizeOffset() const;

  Size getInstanceAlignMaskOffset() const;

  /// Returns the start of the vtable in the class metadata.
  Offset getVTableOffset(IRGenFunction &IGF) const;

  /// Returns the size of the vtable, in words.
  unsigned getVTableSize() const {
    return MethodInfos.size();
  }

  MethodInfo getMethodInfo(IRGenFunction &IGF, SILDeclRef method) const;

  /// Assuming that the given method is at a static offset in the metadata,
  /// return that static offset.
  ///
  /// DEPRECATED: callers should be updated to handle this in a
  /// more arbitrary fashion.
  Size getStaticMethodOffset(SILDeclRef method) const;

  Offset getFieldOffset(IRGenFunction &IGF, VarDecl *field) const;

  /// Assuming that the given field offset is at a static offset in
  /// the metadata, return that static offset.
  ///
  /// DEPRECATED: callers should be updated to handle this in a
  /// more arbitrary fashion.
  Size getStaticFieldOffset(VarDecl *field) const;

  /// Should only be used when emitting the nominal type descriptor.
  Size getRelativeGenericRequirementsOffset() const;

  Size getStaticFieldOffsetVectorOffset() const;
  Size getRelativeFieldOffsetVectorOffset() const;

  Size getStaticVTableOffset() const;
  Size getRelativeVTableOffset() const;

  Offset getFieldOffsetVectorOffset(IRGenFunction &IGF) const;

  /// If the start of the immediate members is statically known, this
  /// method will return it. Otherwise, it will assert.
  Size getStartOfImmediateMembers() const {
    return StartOfImmediateMembers.getStaticOffset();
  }

  /// The number of members to add after superclass metadata. The size of
  /// this metadata is the superclass size plus the number of immediate
  /// members in the class itself.
  unsigned getNumImmediateMembers() const {
    return NumImmediateMembers;
  }

  static bool classof(const MetadataLayout *layout) {
    return layout->getKind() == Kind::Class;
  }
};

/// Layout for enum type metadata.
class EnumMetadataLayout : public NominalMetadataLayout {
  /// The offset of the payload size field, if there is one.
  StoredOffset PayloadSizeOffset;

  // TODO: presumably it would be useful to store *something* here
  // for resilience.

  friend class IRGenModule;
  EnumMetadataLayout(IRGenModule &IGM, EnumDecl *theEnum);

public:
  EnumDecl *getDecl() const {
    return cast<EnumDecl>(Nominal);
  }

  bool hasPayloadSizeOffset() const {
    return PayloadSizeOffset.isValid();
  }

  Offset getPayloadSizeOffset() const;

  static bool classof(const MetadataLayout *layout) {
    return layout->getKind() == Kind::Enum;
  }
};

/// Layout for struct type metadata.
class StructMetadataLayout : public NominalMetadataLayout {
  llvm::DenseMap<VarDecl*, StoredOffset> FieldOffsets;

  /// The start of the field-offset vector.
  StoredOffset FieldOffsetVector;

  const StoredOffset &getStoredFieldOffset(VarDecl *field) const {
    auto it = FieldOffsets.find(field);
    assert(it != FieldOffsets.end());
    return it->second;
  }

  friend class IRGenModule;
  StructMetadataLayout(IRGenModule &IGM, StructDecl *theStruct);

public:
  StructDecl *getDecl() const {
    return cast<StructDecl>(Nominal);
  }

  Offset getFieldOffset(IRGenFunction &IGF, VarDecl *field) const;

  /// Assuming that the given field offset is at a static offset in
  /// the metadata, return that static offset.
  ///
  /// DEPRECATED: callers should be updated to handle this in a
  /// more arbitrary fashion.
  Size getStaticFieldOffset(VarDecl *field) const;

  Offset getFieldOffsetVectorOffset() const;

  static bool classof(const MetadataLayout *layout) {
    return layout->getKind() == Kind::Struct;
  }
};

/// Layout for foreign class type metadata.
class ForeignClassMetadataLayout : public MetadataLayout {
  ClassDecl *Class;
  StoredOffset SuperClassOffset;

  friend class IRGenModule;
  ForeignClassMetadataLayout(IRGenModule &IGM, ClassDecl *theClass);

public:
  StoredOffset getSuperClassOffset() const { return SuperClassOffset; }

  static bool classof(const MetadataLayout *layout) {
    return layout->getKind() == Kind::ForeignClass;
  }
};

/// Emit the address of the field-offset slot in the given class metadata.
Address emitAddressOfClassFieldOffset(IRGenFunction &IGF,
                                      llvm::Value *metadata,
                                      ClassDecl *theClass,
                                      VarDecl *field);

/// Get the offset to a field offset in the class type metadata.
///
/// DEPRECATED: callers should be updated to handle this in a more
/// arbitrary fashion.
Size getClassFieldOffsetOffset(IRGenModule &IGM,
                               ClassDecl *theClass,
                               VarDecl *field);

/// Emit the address of the field-offset vector in the given class or struct
/// metadata.
Address emitAddressOfFieldOffsetVector(IRGenFunction &IGF,
                                       llvm::Value *metadata,
                                       NominalTypeDecl *theDecl);

/// Given a reference to class type metadata of the given type,
/// decide the offset to the given field.  This assumes that the
/// offset is stored in the metadata, i.e. its offset is potentially
/// dependent on generic arguments.  The result is a ptrdiff_t.
llvm::Value *emitClassFieldOffset(IRGenFunction &IGF,
                                  ClassDecl *theClass,
                                  VarDecl *field,
                                  llvm::Value *metadata);

/// Given a class metadata pointer, emit the address of its superclass field.  
Address emitAddressOfSuperclassRefInClassMetadata(IRGenFunction &IGF,
                                                  llvm::Value *metadata);

} // end namespace irgen
} // end namespace swift

#endif
