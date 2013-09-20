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
#include "swift/AST/Pattern.h"
#include "swift/Basic/Optional.h"
#include "swift/IRGen/Options.h"
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

using namespace swift;
using namespace irgen;

namespace {
  class StructFieldInfo : public SequentialField<StructFieldInfo> {
  public:
    StructFieldInfo(VarDecl *field, const TypeInfo &type)
      : SequentialField(type), Field(field) {}

    /// The field.
    VarDecl *Field;

    StringRef getFieldName() const {
      return Field->getName().str();
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
                                VarDecl *field) const {
      auto &fieldInfo = getFieldInfo(field);
      if (fieldInfo.isEmpty())
        return fieldInfo.getTypeInfo().getUndefAddress();

      auto offsets = asImpl().getNonFixedOffsets(IGF);
      return fieldInfo.projectAddress(IGF, addr, offsets);
    }
  };


  /// A type implementation for loadable struct types.
  class LoadableStructTypeInfo
      : public StructTypeInfoBase<LoadableStructTypeInfo, LoadableTypeInfo> {
  public:
    // FIXME: Spare bits between struct members.
    LoadableStructTypeInfo(unsigned numFields, llvm::Type *T, Size size,
                           Alignment align, IsPOD_t isPOD)
      : StructTypeInfoBase(numFields, T, size, llvm::BitVector{}, align, isPOD)
    {}

    bool isIndirectArgument(ExplosionKind kind) const override { return false; }
    void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                              Address addr) const override {
      LoadableStructTypeInfo::initialize(IGF, params, addr);
    }
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const { return Nothing; }
  };

  /// A type implementation for non-loadable but fixed-size struct types.
  class FixedStructTypeInfo
      : public StructTypeInfoBase<FixedStructTypeInfo,
                                  IndirectTypeInfo<FixedStructTypeInfo,
                                                   FixedTypeInfo>> {
  public:
    // FIXME: Spare bits between struct members.
    FixedStructTypeInfo(unsigned numFields, llvm::Type *T, Size size,
                        Alignment align, IsPOD_t isPOD)
      : StructTypeInfoBase(numFields, T, size, llvm::BitVector{}, align, isPOD)
    {}

    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const { return Nothing; }
  };

  /// A type implementation for non-fixed struct types.
  class NonFixedStructTypeInfo
      : public StructTypeInfoBase<NonFixedStructTypeInfo,
                                  WitnessSizedTypeInfo<NonFixedStructTypeInfo>> {
    CanType TheType;
  public:
    NonFixedStructTypeInfo(unsigned numFields, llvm::Type *T, CanType theType,
                           Alignment align, IsPOD_t isPOD)
      : StructTypeInfoBase(numFields, T, align, isPOD), TheType(theType) {
    }

    // FIXME: implement
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const { return Nothing; }

    llvm::Value *getMetadataRef(IRGenFunction &IGF) const {
      return IGF.emitTypeMetadataRef(TheType);
    }

    llvm::Value *getValueWitnessTable(IRGenFunction &IGF) const {
      auto metadata = getMetadataRef(IGF);
      return IGF.emitValueWitnessTableRefForMetadata(metadata);
    }
                                    
    void initializeValueWitnessTable(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     llvm::Value *vwtable) const override {
      // FIXME
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
                                            layout.getAlignment(),
                                            layout.isKnownPOD());
    }

    FixedStructTypeInfo *createFixed(ArrayRef<StructFieldInfo> fields,
                                     const StructLayout &layout) {
      return create<FixedStructTypeInfo>(fields, layout.getType(),
                                         layout.getSize(),
                                         layout.getAlignment(),
                                         layout.isKnownPOD());
    }

    NonFixedStructTypeInfo *createNonFixed(ArrayRef<StructFieldInfo> fields,
                                           const StructLayout &layout) {
      return create<NonFixedStructTypeInfo>(fields, layout.getType(),
                                            TheStruct,
                                            layout.getAlignment(),
                                            layout.isKnownPOD());
    }

    StructFieldInfo getFieldInfo(VarDecl *field, const TypeInfo &fieldTI) {
      return StructFieldInfo(field, fieldTI);
    }

    Type getType(VarDecl *field) { return field->getType(); }

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
  FOR_STRUCT_IMPL(IGF, baseType, projectFieldAddress, base, field);
}

void irgen::projectPhysicalStructMemberFromExplosion(IRGenFunction &IGF,
                                                     SILType baseType,
                                                     Explosion &base,
                                                     VarDecl *field,
                                                     Explosion &out) {
  FOR_STRUCT_IMPL(IGF, baseType, projectFieldFromExplosion, base, field, out);
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
      llvm_unreachable("decl not allowed in struct!");

    // We can have meaningful initializers for variables, but
    // we can't handle them yet.  For the moment, just ignore them.
    case DeclKind::PatternBinding:
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
      if (cast<VarDecl>(member)->isProperty())
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

const TypeInfo *TypeConverter::convertStructType(CanType type, StructDecl *D) {
  // Collect all the fields from the type.
  SmallVector<VarDecl*, 8> fields;
  for (Decl *D : D->getMembers())
    if (VarDecl *VD = dyn_cast<VarDecl>(D))
      if (!VD->isProperty()) {
        if (!IGM.Opts.EnableDynamicValueTypeLayout &&
            !IGM.getTypeInfo(VD->getType()).isFixedSize()) {
          IGM.unimplemented(VD->getLoc(), "dynamic field layout in structs");
          exit(1);
        }
        fields.push_back(VD);
      }

  // Create the struct type.
  auto ty = IGM.createNominalType(D);

  // Register a forward declaration before we look at any of the child types.
  addForwardDecl(type.getPointer(), ty);

  // Build the type.
  StructTypeBuilder builder(IGM, ty, type);
  return builder.layout(fields);
}
