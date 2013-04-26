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
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

#include "GenMeta.h"
#include "GenSequential.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "Explosion.h"

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

  /// Layout information for struct types.
  class StructTypeInfo : // FIXME: FixedTypeInfo as the base class is a lie.
    public SequentialTypeInfo<StructTypeInfo, FixedTypeInfo, StructFieldInfo> {
  public:
    StructTypeInfo(unsigned numFields, llvm::Type *T, Size size, Alignment align,
                   IsPOD_t isPOD)
      : SequentialTypeInfo(numFields, T, size, align, isPOD) {
    }

    /// FIXME: implement
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const { return Nothing; }
  };

  class StructTypeBuilder :
    public SequentialTypeBuilder<StructTypeBuilder, StructFieldInfo, VarDecl*> {

    llvm::StructType *StructTy;
  public:
    StructTypeBuilder(IRGenModule &IGM, llvm::StructType *structTy) :
      SequentialTypeBuilder(IGM), StructTy(structTy) {
    }

    StructTypeInfo *createFixed(ArrayRef<StructFieldInfo> fields,
                                const StructLayout &layout) {
      return create<StructTypeInfo>(fields, layout.getType(), layout.getSize(),
                                    layout.getAlignment(), layout.isKnownPOD());
    }

    StructTypeInfo *createNonFixed(ArrayRef<StructFieldInfo> fields,
                                   const StructLayout &layout) {
      // FIXME: implement properly
      return createFixed(fields, layout);
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

OwnedAddress irgen::projectPhysicalStructMemberAddress(IRGenFunction &IGF,
                                                       OwnedAddress base,
                                                       CanType baseType,
                                                       unsigned fieldIndex) {
  assert((baseType->is<StructType>() || baseType->is<BoundGenericStructType>())
         && "not a struct type");
  auto &baseTI = IGF.getFragileTypeInfo(baseType).as<StructTypeInfo>();
  auto &fieldI = baseTI.getFields()[fieldIndex];
  auto offsets = baseTI.getNonFixedOffsets(IGF);
  Address project = fieldI.projectAddress(IGF, base, offsets);
  return OwnedAddress(project, base.getOwner());
}

void irgen::projectPhysicalStructMemberFromExplosion(IRGenFunction &IGF,
                                                     CanType baseType,
                                                     Explosion &base,
                                                     unsigned fieldNo,
                                                     Explosion &out) {
  auto &baseTI = IGF.getFragileTypeInfo(baseType).as<StructTypeInfo>();
  auto &fieldI = baseTI.getFields()[fieldNo];
  // If the field requires no storage, there's nothing to do.
  if (fieldI.isEmpty()) {
    return IGF.emitFakeExplosion(fieldI.getTypeInfo(), out);
  }
  
  // Otherwise, project from the base.
  auto fieldRange = fieldI.getProjectionRange(out.getKind());
  ArrayRef<llvm::Value *> element = base.getRange(fieldRange.first,
                                                 fieldRange.second);
  out.add(element);
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
    case DeclKind::OneOfElement:
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
      continue;
    case DeclKind::OneOf:
      emitOneOfDecl(cast<OneOfDecl>(member));
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

const TypeInfo *TypeConverter::convertStructType(StructDecl *D) {
  // Collect all the fields from the type.
  SmallVector<VarDecl*, 8> fields;
  for (Decl *D : D->getMembers())
    if (VarDecl *VD = dyn_cast<VarDecl>(D))
      if (!VD->isProperty())
        fields.push_back(VD);

  // Create the struct type.
  auto ty = IGM.createNominalType(D);

  // Register a forward declaration before we look at any of the child types.
  auto typesMapKey = D->getDeclaredType().getPointer();
  addForwardDecl(typesMapKey, ty);

  // Build the type.
  StructTypeBuilder builder(IGM, ty);
  return builder.layout(fields);
}
