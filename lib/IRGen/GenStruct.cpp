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

#include "GenSequential.h"

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"

#include "GenSequential.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
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

    Address projectAddress(IRGenFunction &IGF, Address seq) const {
      seq = IGF.Builder.CreateStructGEP(seq, 0, Size(0));
      return SequentialField<StructFieldInfo>::projectAddress(IGF, seq);
    }
  };

  /// Layout information for struct types.
  class StructTypeInfo :
    public SequentialTypeInfo<StructTypeInfo, StructFieldInfo> {
  public:
    StructTypeInfo(llvm::Type *T, unsigned numFields)
      : SequentialTypeInfo(T, numFields) {
    }

    void emitInjectionFunctionBody(IRGenFunction &IGF,
                                   OneOfElementDecl *elt,
                                   Explosion &params) const {
      ExplosionSchema schema(params.getKind());
      getSchema(schema);
      if (schema.requiresIndirectResult()) {
        Address returnSlot(params.claimUnmanagedNext(),
                           StorageAlignment);
        initialize(IGF, params, returnSlot);
        IGF.Builder.CreateRetVoid();
      } else {
        IGF.emitScalarReturn(params);
      }
    }
  };

  class StructTypeBuilder :
    public SequentialTypeBuilder<StructTypeBuilder, StructTypeInfo> {
  private:
    StructDecl *D;
  public:
    StructTypeBuilder(IRGenModule &IGM, StructDecl *D) :
        SequentialTypeBuilder(IGM), D(D) {}

    StructTypeInfo *construct(void *buffer, ArrayRef<VarDecl*> fields) {
      return ::new(buffer) StructTypeInfo(IGM.Int8Ty, fields.size());
    }

    StructFieldInfo getFieldInfo(VarDecl *field,
                                const TypeInfo &fieldTI) {
      return StructFieldInfo(field, fieldTI);
    }

    Type getType(VarDecl *field) { return field->getType(); }

    void performLayout(ArrayRef<const TypeInfo *> fieldTypes) {
      StructLayout layout(IGM, LayoutKind::NonHeapObject,
                          LayoutStrategy::Optimal, fieldTypes);
      llvm::StructType *structTy = IGM.createNominalType(D);
      structTy->setBody(ArrayRef<llvm::Type*>(layout.getType()));
      recordLayout(layout, structTy);
    }
  };
}  // end anonymous namespace.

namespace {
  class PhysicalStructMember : public PhysicalPathComponent {
    const StructFieldInfo &Field;

  public:
    PhysicalStructMember(const StructFieldInfo &field) : Field(field) {}

    OwnedAddress offset(IRGenFunction &IGF, OwnedAddress addr) const {
      Address project = Field.projectAddress(IGF, addr);
      return OwnedAddress(project, addr.getOwner());
    }
  };
}

static void emitInjectionFunction(IRGenModule &IGM,
                                  llvm::Function *fn,
                                  const StructTypeInfo &structTI,
                                  OneOfElementDecl *elt) {
  ExplosionKind explosionKind = ExplosionKind::Minimal;
  IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(), explosionKind,
                    /*uncurry level*/ 0, fn, Prologue::Bare);

  Explosion explosion = IGF.collectParameters();
  structTI.emitInjectionFunctionBody(IGF, elt, explosion);
}

LValue swift::irgen::emitPhysicalStructMemberLValue(IRGenFunction &IGF,
                                                    MemberRefExpr *E) {
  LValue lvalue = IGF.emitLValue(E->getBase());
  StructType *T = cast<StructDecl>(E->getDecl()->getDeclContext())->getDeclaredType();
  const StructTypeInfo &info = TypeConverter::getFragileTypeInfo(IGF.IGM, T).as<StructTypeInfo>();

  // FIXME: This field index computation is an ugly hack.
  unsigned FieldIndex = 0;
  for (ValueDecl *D : T->getDecl()->getMembers()) {
    if (D == E->getDecl())
      break;
    if (isa<VarDecl>(D))
      ++FieldIndex;
  }

  const StructFieldInfo &field = info.getFields()[FieldIndex];
  lvalue.add<PhysicalStructMember>(field);
  return lvalue;
}


/// emitStructType - Emit all the declarations associated with this oneof type.
void IRGenModule::emitStructType(StructType *oneof) {
  const StructTypeInfo &typeInfo = getFragileTypeInfo(oneof).as<StructTypeInfo>();
  OneOfElementDecl *elt = cast<OneOfElementDecl>(oneof->getDecl()->getMembers().back());
  llvm::Function *fn = getAddrOfGlobalInjectionFunction(elt);
  emitInjectionFunction(*this, fn, typeInfo, elt);
}

void IRGenFunction::emitStructType(StructType *oneof) {
  const StructTypeInfo &typeInfo = getFragileTypeInfo(oneof).as<StructTypeInfo>();
  OneOfElementDecl *elt = cast<OneOfElementDecl>(oneof->getDecl()->getMembers().back());
  llvm::Function *fn = getAddrOfLocalInjectionFunction(elt);
  emitInjectionFunction(IGM, fn, typeInfo, elt);
}

const TypeInfo *
TypeConverter::convertStructType(IRGenModule &IGM, StructType *T) {
  StructTypeBuilder builder(IGM, T->getDecl());
  SmallVector<VarDecl*, 8> Fields;
  for (ValueDecl *D : T->getDecl()->getMembers())
    if (VarDecl *VD = dyn_cast<VarDecl>(D))
      Fields.push_back(VD);
  builder.create(ArrayRef<VarDecl*>(Fields));
  return builder.complete(ArrayRef<VarDecl*>(Fields));
}
