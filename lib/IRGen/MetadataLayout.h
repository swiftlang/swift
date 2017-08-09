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

/// A base class for various kinds of metadata layout.
class MetadataLayout {
public:
  enum class Kind {
    Class,
    Struct,
    Enum
    // Update NominalMetadataLayout::classof if you add a non-nominal layout.
  };

  class StoredOffset {
    enum State {
      /// The high bits are an integer displacement.
      Static = 0,

      /// The high bits are an llvm::Constant* for the displacement,
      /// which may be null if it hasn't been computed yet.
      Dynamic,
    };
    enum : uint64_t {
      KindBits = 1,
      KindMask = (1 << KindBits) - 1,
      PayloadMask = ~uint64_t(KindMask)
    };

    mutable uintptr_t Data;
  public:
    StoredOffset() : Data(0) {}
    explicit StoredOffset(llvm::Constant *offset)
      : Data(reinterpret_cast<uintptr_t>(offset) | Dynamic) {}
    explicit StoredOffset(Size offset)
      : Data((static_cast<uint64_t>(offset.getValue()) << KindBits) | Static) {
      assert(!offset.isZero() && "cannot store a zero offset");
      assert(getStaticOffset() == offset && "overflow");
    }

    bool isValid() const { return Data != 0; }
    bool isStatic() const { return isValid() && (Data & KindMask) == Static; }
    bool isDynamic() const { return (Data & KindMask) == Dynamic; }
    Size getStaticOffset() const {
      assert(isStatic());
      return Size(static_cast<int64_t>(Data) >> KindBits);
    }
    llvm::Constant *getDynamicOffsetVariable() const {
      assert(isDynamic());
      return reinterpret_cast<llvm::Constant*>(Data & PayloadMask);
    }
    void setDynamicOffsetVariable(llvm::Constant *pointer) const {
      assert(isDynamic());
      Data = reinterpret_cast<uintptr_t>(pointer) | Dynamic;
    }
  };

private:
  Kind TheKind;

protected:
  MetadataLayout(Kind theKind) : TheKind(theKind) {}

  MetadataLayout(const MetadataLayout &other) = delete;
  MetadataLayout &operator=(const MetadataLayout &other) = delete;

public:
  /// Destruct and deallocate this layout object.
  void destroy() const;

  Kind getKind() const { return TheKind; }
};

/// Base class for nominal type metadata layouts.
class NominalMetadataLayout : public MetadataLayout {
protected:
  StoredOffset GenericRequirements;
  StoredOffset Parent;

  NominalMetadataLayout(Kind kind) : MetadataLayout(kind) {}

public:
  bool hasGenericRequirements() const {
    return GenericRequirements.isValid();
  }

  Offset getGenericRequirementsOffset(IRGenFunction &IGF) const;
  Offset getParentOffset(IRGenFunction &IGF) const;

  static bool classof(const MetadataLayout *layout) {
    return true; // No non-nominal metadata for now.
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
  struct StoredMethodInfo {
    StoredOffset TheOffset;
    StoredMethodInfo(StoredOffset offset) : TheOffset(offset) {}
  };
  llvm::DenseMap<SILDeclRef, StoredMethodInfo> MethodInfos;

  /// Field offsets for various fields.
  llvm::DenseMap<VarDecl*, StoredOffset> FieldOffsets;

  /// The start of the field-offset vector.
  StoredOffset FieldOffsetVector;

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

  Offset getFieldOffsetVectorOffset(IRGenFunction &IGF) const;

  static bool classof(const MetadataLayout *layout) {
    return layout->getKind() == Kind::Class;
  }
};

/// Layout for enum type metadata.
class EnumMetadataLayout : public NominalMetadataLayout {
  // TODO: presumably it would be useful to store *something* here
  // for resilience.

  friend class IRGenModule;
  EnumMetadataLayout(IRGenModule &IGM, EnumDecl *theEnum);

public:
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

/// Emit the address of the 'parent' slot in the given nominal-type metadata.
Address emitAddressOfParentMetadataSlot(IRGenFunction &IGF,
                                        llvm::Value *metadata,
                                        NominalTypeDecl *decl);

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

} // end namespace irgen
} // end namespace swift

#endif
