//===--- GenStruct.cpp - Swift IR Generation For 'struct' Types -----------===//
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
//  This file implements IR generation for struct types.
//
//===----------------------------------------------------------------------===//

#include "GenStruct.h"

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Pattern.h"
#include "swift/Basic/Optional.h"
#include "swift/SIL/SILModule.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

#include "GenMeta.h"
#include "GenSequential.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "IndirectTypeInfo.h"
#include "NonFixedTypeInfo.h"
#include "StructMetadataLayout.h"

using namespace swift;
using namespace irgen;

namespace {
  class StructFieldInfo : public SequentialField<StructFieldInfo> {
  public:
    StructFieldInfo(VarDecl *field, const TypeInfo &type)
      : SequentialField(type), Field(field) {}

    /// The field.
    VarDecl * const Field;

    StringRef getFieldName() const {
      return Field->getName().str();
    }
    
    CanType getType(IRGenModule &IGM, CanType T) const {
      return T->getTypeOfMember(IGM.SILMod->getSwiftModule(),
                                Field, nullptr)
        ->getCanonicalType();
    }
  };

  /// A common base class for structs.
  template <class Impl, class Base>
  class StructTypeInfoBase :
     public SequentialTypeInfo<Impl, Base, StructFieldInfo> {
    typedef SequentialTypeInfo<Impl, Base, StructFieldInfo> super;

  protected:
    template <class... As>
    StructTypeInfoBase(As &&...args) : super(std::forward<As>(args)...) {}

    using super::asImpl;

  public:
    const StructFieldInfo &getFieldInfo(VarDecl *field) const {
      // FIXME: cache the physical field index in the VarDecl.
      for (auto &fieldInfo : asImpl().getFields()) {
        if (fieldInfo.Field == field)
          return fieldInfo;
      }
      llvm_unreachable("field not in struct?");
    }

    /// Given a full struct explosion, project out a single field.
    void projectFieldFromExplosion(IRGenFunction &IGF,
                                   Explosion &in,
                                   VarDecl *field,
                                   Explosion &out) const {
      assert(in.getKind() == out.getKind());
      auto &fieldInfo = getFieldInfo(field);

      // If the field requires no storage, there's nothing to do.
      if (fieldInfo.isEmpty())
        return;
  
      // Otherwise, project from the base.
      auto fieldRange = fieldInfo.getProjectionRange(out.getKind());
      auto elements = in.getRange(fieldRange.first, fieldRange.second);
      out.add(elements);
    }

    /// Given the address of a tuple, project out the address of a
    /// single element.
    Address projectFieldAddress(IRGenFunction &IGF,
                                Address addr,
                                CanType T,
                                VarDecl *field) const {
      const StructFieldInfo &fieldInfo = getFieldInfo(field);
      if (fieldInfo.isEmpty())
        return fieldInfo.getTypeInfo().getUndefAddress();

      auto offsets = asImpl().getNonFixedOffsets(IGF, T);
      return fieldInfo.projectAddress(IGF, addr, offsets);
    }
       
    /// Return the constant offset of a field as a SizeTy, or nullptr if the
    /// field is not at a fixed offset.
    llvm::Constant *getConstantFieldOffset(IRGenModule &IGM,
                                           VarDecl *field) const {
      const StructFieldInfo &fieldInfo = getFieldInfo(field);
      if (fieldInfo.getKind() == ElementLayout::Kind::Fixed) {
        return llvm::ConstantInt::get(IGM.SizeTy,
                                    fieldInfo.getFixedByteOffset().getValue());
      }
      return nullptr;
    }

    // For now, just use extra inhabitants from the first field.
    // FIXME: generalize
    bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
      if (asImpl().getFields().empty()) return false;
      return asImpl().getFields()[0].getTypeInfo().mayHaveExtraInhabitants(IGM);
    }

    // This is dead code in NonFixedStructTypeInfo.
    unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const {
      if (asImpl().getFields().empty()) return 0;
      auto &fieldTI = cast<FixedTypeInfo>(asImpl().getFields()[0].getTypeInfo());
      return fieldTI.getFixedExtraInhabitantCount(IGM);
    }

    // This is dead code in NonFixedStructTypeInfo.
    llvm::ConstantInt *getFixedExtraInhabitantValue(IRGenModule &IGM,
                                                    unsigned bits,
                                                    unsigned index) const {
      auto &fieldTI = cast<FixedTypeInfo>(asImpl().getFields()[0].getTypeInfo());
      return fieldTI.getFixedExtraInhabitantValue(IGM, bits, index);
    }

    // This is dead code in NonFixedStructTypeInfo.
    llvm::Value *maskFixedExtraInhabitant(IRGenFunction &IGF,
                                          llvm::Value *structValue) const {
      // Truncate down to the width of the field, mask it recursively,
      // and then zext back out to the payload size.
      auto &fieldTI = cast<FixedTypeInfo>(asImpl().getFields()[0].getTypeInfo());
      unsigned fieldWidth = fieldTI.getFixedSize().getValueInBits();
      auto fieldTy = llvm::IntegerType::get(IGF.IGM.getLLVMContext(), fieldWidth);
      auto fieldValue = IGF.Builder.CreateTrunc(structValue, fieldTy);
      fieldValue = fieldTI.maskFixedExtraInhabitant(IGF, fieldValue);
      return IGF.Builder.CreateZExt(fieldValue, structValue->getType());
    }

    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                         Address structAddr,
                                         CanType structType) const override {
      auto &field = asImpl().getFields()[0];
      Address fieldAddr =
        asImpl().projectFieldAddress(IGF, structAddr, structType, field.Field);
      return field.getTypeInfo().getExtraInhabitantIndex(IGF, fieldAddr,
                                          field.getType(IGF.IGM, structType));
    }

    void storeExtraInhabitant(IRGenFunction &IGF,
                              llvm::Value *index,
                              Address structAddr,
                              CanType structType) const override {
      auto &field = asImpl().getFields()[0];
      Address fieldAddr =
        asImpl().projectFieldAddress(IGF, structAddr, structType, field.Field);
      field.getTypeInfo().storeExtraInhabitant(IGF, index, fieldAddr,
                                          field.getType(IGF.IGM, structType));
    }
  };


  /// A type implementation for loadable struct types.
  class LoadableStructTypeInfo
      : public StructTypeInfoBase<LoadableStructTypeInfo, LoadableTypeInfo> {
  public:
    // FIXME: Spare bits between struct members.
    LoadableStructTypeInfo(unsigned numFields, llvm::Type *T, Size size,
                           llvm::BitVector spareBits,
                           Alignment align, IsPOD_t isPOD)
      : StructTypeInfoBase(numFields, T, size, std::move(spareBits),
                           align, isPOD)
    {}

    bool isIndirectArgument(ResilienceExpansion kind) const override { return false; }
    void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                              Address addr, CanType T) const override {
      LoadableStructTypeInfo::initialize(IGF, params, addr);
    }
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF, CanType T) const {
      return Nothing;
    }
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const {
      return Nothing;
    }
  };

  /// A type implementation for non-loadable but fixed-size struct types.
  class FixedStructTypeInfo
      : public StructTypeInfoBase<FixedStructTypeInfo,
                                  IndirectTypeInfo<FixedStructTypeInfo,
                                                   FixedTypeInfo>> {
  public:
    // FIXME: Spare bits between struct members.
    FixedStructTypeInfo(unsigned numFields, llvm::Type *T, Size size,
                        llvm::BitVector spareBits,
                        Alignment align, IsPOD_t isPOD, IsBitwiseTakable_t isBT)
      : StructTypeInfoBase(numFields, T, size, std::move(spareBits), align,
                           isPOD, isBT)
    {}
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF, CanType T) const {
      return Nothing;
    }
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const { return Nothing; }
  };
  
  /// Find the beginning of the field offset vector in a struct's metadata.
  static Address
  emitAddressOfFieldOffsetVector(IRGenFunction &IGF,
                                 StructDecl *S,
                                 llvm::Value *metadata) {
    struct GetStartOfFieldOffsets
      : StructMetadataScanner<GetStartOfFieldOffsets>
    {
      GetStartOfFieldOffsets(IRGenModule &IGM, StructDecl *target)
        : StructMetadataScanner(IGM, target) {}
      
      Size StartOfFieldOffsets = Size::invalid();
      
      void noteAddressPoint() {
        assert(StartOfFieldOffsets == Size::invalid()
               && "found field offsets before address point?");
        NextOffset = Size(0);
      }
      void noteStartOfFieldOffsets() { StartOfFieldOffsets = NextOffset; }
    };
    
    // Find where the field offsets begin.
    GetStartOfFieldOffsets scanner(IGF.IGM, S);
    scanner.layout();
    assert(scanner.StartOfFieldOffsets != Size::invalid()
           && "did not find start of field offsets?!");
    
    Size StartOfFieldOffsets = scanner.StartOfFieldOffsets;
    
    // Find that offset into the metadata.
    llvm::Value *fieldVector
      = IGF.Builder.CreateBitCast(metadata, IGF.IGM.SizeTy->getPointerTo());
    return IGF.Builder.CreateConstArrayGEP(
                            Address(fieldVector, IGF.IGM.getPointerAlignment()),
                            StartOfFieldOffsets / IGF.IGM.getPointerSize(),
                            StartOfFieldOffsets);
  }
  
  /// Accessor for the non-fixed offsets of a struct type.
  class StructNonFixedOffsets : public NonFixedOffsetsImpl {
    CanType TheStruct;
  public:
    StructNonFixedOffsets(CanType type) : TheStruct(type) {
      assert(TheStruct->getStructOrBoundGenericStruct());
    }
    
    llvm::Value *getOffsetForIndex(IRGenFunction &IGF, unsigned index) {
      // Get the field offset vector from the struct metadata.
      llvm::Value *metadata = IGF.emitTypeMetadataRef(TheStruct);
      Address fieldVector = emitAddressOfFieldOffsetVector(IGF,
                                    TheStruct->getStructOrBoundGenericStruct(),
                                    metadata);
      
      // Grab the indexed offset.
      fieldVector = IGF.Builder.CreateConstArrayGEP(fieldVector, index,
                                                    IGF.IGM.getPointerSize());
      return IGF.Builder.CreateLoad(fieldVector);
    }
  };

  /// A type implementation for non-fixed struct types.
  class NonFixedStructTypeInfo
      : public StructTypeInfoBase<NonFixedStructTypeInfo,
                                  WitnessSizedTypeInfo<NonFixedStructTypeInfo>>
  {
  public:
    NonFixedStructTypeInfo(unsigned numFields, llvm::Type *T,
                           Alignment align,
                           IsPOD_t isPOD, IsBitwiseTakable_t isBT)
      : StructTypeInfoBase(numFields, T, align, isPOD, isBT) {
    }

    // We have an indirect schema.
    void getSchema(ExplosionSchema &s) const override {
      s.add(ExplosionSchema::Element::forAggregate(getStorageType(),
                                                   getBestKnownAlignment()));
    }

    StructNonFixedOffsets
    getNonFixedOffsets(IRGenFunction &IGF, CanType T) const {
      return StructNonFixedOffsets(T);
    }

    void initializeMetadata(IRGenFunction &IGF,
                            llvm::Value *metadata,
                            llvm::Value *vwtable,
                            CanType T) const override {
      // Get the field offset vector.
      llvm::Value *fieldVector = emitAddressOfFieldOffsetVector(IGF,
                                    T->getStructOrBoundGenericStruct(),
                                    metadata).getAddress();

      // Collect the stored properties of the type.
      llvm::SmallVector<VarDecl*, 4> storedProperties;
      for (auto prop : T->getStructOrBoundGenericStruct()
                        ->getStoredProperties()) {
        storedProperties.push_back(prop);
      }
      // Fill out an array with the field type metadata records.
      Address fields = IGF.createAlloca(
                       llvm::ArrayType::get(IGF.IGM.TypeMetadataPtrTy,
                                            storedProperties.size()),
                       IGF.IGM.getPointerAlignment(), "structFields");
      fields = IGF.Builder.CreateBitCast(fields,
                                     IGF.IGM.TypeMetadataPtrTy->getPointerTo());
      unsigned index = 0;
      for (auto prop : storedProperties) {
        llvm::Value *metadata = IGF.emitTypeMetadataRef(
                                          prop->getType()->getCanonicalType());
        Address field = IGF.Builder.CreateConstArrayGEP(fields, index,
                                                      IGF.IGM.getPointerSize());
        IGF.Builder.CreateStore(metadata, field);
        ++index;
      }
      
      // Ask the runtime to lay out the struct.
      auto numFields = llvm::ConstantInt::get(IGF.IGM.SizeTy,
                                              storedProperties.size());
      IGF.Builder.CreateCall4(IGF.IGM.getInitStructMetadataUniversalFn(),
                              numFields, fields.getAddress(),
                              fieldVector, vwtable);
    }
  };

  class StructTypeBuilder :
    public SequentialTypeBuilder<StructTypeBuilder, StructFieldInfo, VarDecl*> {

    llvm::StructType *StructTy;
    CanType TheStruct;
  public:
    StructTypeBuilder(IRGenModule &IGM, llvm::StructType *structTy,
                      CanType type) :
      SequentialTypeBuilder(IGM), StructTy(structTy), TheStruct(type) {
    }

    LoadableStructTypeInfo *createLoadable(ArrayRef<StructFieldInfo> fields,
                                           const StructLayout &layout) {
      return create<LoadableStructTypeInfo>(fields, layout.getType(),
                                            layout.getSize(),
                                            layout.getSpareBits(),
                                            layout.getAlignment(),
                                            layout.isKnownPOD());
    }

    FixedStructTypeInfo *createFixed(ArrayRef<StructFieldInfo> fields,
                                     const StructLayout &layout) {
      return create<FixedStructTypeInfo>(fields, layout.getType(),
                                         layout.getSize(),
                                         layout.getSpareBits(),
                                         layout.getAlignment(),
                                         layout.isKnownPOD(),
                                         layout.isKnownBitwiseTakable());
    }

    NonFixedStructTypeInfo *createNonFixed(ArrayRef<StructFieldInfo> fields,
                                           const StructLayout &layout) {
      return create<NonFixedStructTypeInfo>(fields, layout.getType(),
                                            layout.getAlignment(),
                                            layout.isKnownPOD(),
                                            layout.isKnownBitwiseTakable());
    }

    StructFieldInfo getFieldInfo(unsigned index,
                                 VarDecl *field, const TypeInfo &fieldTI) {
      return StructFieldInfo(field, fieldTI);
    }

    SILType getType(VarDecl *field) {
      assert(field->getDeclContext() == TheStruct->getAnyNominal());
      auto silType = SILType::getPrimitiveAddressType(TheStruct);
      return silType.getFieldType(field, *IGM.SILMod);
    }

    StructLayout performLayout(ArrayRef<const TypeInfo *> fieldTypes) {
      return StructLayout(IGM, LayoutKind::NonHeapObject,
                          LayoutStrategy::Optimal, fieldTypes, StructTy);
    }
  };
}  // end anonymous namespace.

/// A convenient macro for delegating an operation to all of the
/// various struct implementations.
#define FOR_STRUCT_IMPL(IGF, type, op, ...) do {                       \
  auto &structTI = IGF.getTypeInfo(type);                              \
  if (isa<LoadableStructTypeInfo>(structTI)) {                         \
    return structTI.as<LoadableStructTypeInfo>().op(IGF, __VA_ARGS__); \
  } else if (isa<FixedTypeInfo>(structTI)) {                           \
    return structTI.as<FixedStructTypeInfo>().op(IGF, __VA_ARGS__);    \
  } else {                                                             \
    return structTI.as<NonFixedStructTypeInfo>().op(IGF, __VA_ARGS__); \
  }                                                                    \
} while(0)

Address irgen::projectPhysicalStructMemberAddress(IRGenFunction &IGF,
                                                  Address base,
                                                  SILType baseType,
                                                  VarDecl *field) {
  FOR_STRUCT_IMPL(IGF, baseType, projectFieldAddress, base,
                  baseType.getSwiftRValueType(), field);
}

void irgen::projectPhysicalStructMemberFromExplosion(IRGenFunction &IGF,
                                                     SILType baseType,
                                                     Explosion &base,
                                                     VarDecl *field,
                                                     Explosion &out) {
  FOR_STRUCT_IMPL(IGF, baseType, projectFieldFromExplosion, base, field, out);
}

llvm::Constant *irgen::emitPhysicalStructMemberFixedOffset(IRGenModule &IGM,
                                                           SILType baseType,
                                                           VarDecl *field) {
  FOR_STRUCT_IMPL(IGM, baseType, getConstantFieldOffset, field);
}

/// emitStructDecl - Emit all the declarations associated with this struct type.
void IRGenModule::emitStructDecl(StructDecl *st) {
  emitStructMetadata(*this, st);

  // FIXME: This is mostly copy-paste from emitExtension;
  // figure out how to refactor! 
  for (Decl *member : st->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
    case DeclKind::Destructor:
    case DeclKind::EnumCase:
    case DeclKind::EnumElement:
    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
    case DeclKind::Param:
      llvm_unreachable("decl not allowed in struct!");

    // We can have meaningful initializers for variables, but
    // we can't handle them yet.  For the moment, just ignore them.
    case DeclKind::PatternBinding:
      continue;

    // Active members of the IfConfig block are handled separately.
    case DeclKind::IfConfig:
      continue;

    case DeclKind::Subscript:
      // Getter/setter will be handled separately.
      continue;
    case DeclKind::TypeAlias:
    case DeclKind::AssociatedType:
    case DeclKind::GenericTypeParam:
      continue;
    case DeclKind::Enum:
      emitEnumDecl(cast<EnumDecl>(member));
      continue;
    case DeclKind::Struct:
      emitStructDecl(cast<StructDecl>(member));
      continue;
    case DeclKind::Class:
      emitClassDecl(cast<ClassDecl>(member));
      continue;
    case DeclKind::Var:
      if (!cast<VarDecl>(member)->hasStorage())
        // Getter/setter will be handled separately.
        continue;
      // FIXME: Will need an implementation here for resilience
      continue;
    case DeclKind::Func:
      emitLocalDecls(cast<FuncDecl>(member));
      continue;
    case DeclKind::Constructor:
      emitLocalDecls(cast<ConstructorDecl>(member));
      continue;
    }
    llvm_unreachable("bad extension member kind");
  }
}
#include "llvm/Support/raw_ostream.h"

const TypeInfo *TypeConverter::convertStructType(TypeBase *key, CanType type,
                                                 StructDecl *D) {
  // Collect all the fields from the type.
  SmallVector<VarDecl*, 8> fields;
  for (VarDecl *VD : D->getStoredProperties())
    fields.push_back(VD);

  // Create the struct type.
  auto ty = IGM.createNominalType(D);

  // Register a forward declaration before we look at any of the child types.
  addForwardDecl(key, ty);

  // Build the type.
  StructTypeBuilder builder(IGM, ty, type);
  return builder.layout(fields);
}
