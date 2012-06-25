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
#include "swift/AST/Expr.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"

#include "GenSequential.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
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
    public SequentialTypeBuilder<StructTypeBuilder, StructTypeInfo, VarDecl*> {

    llvm::StructType *StructTy;
  public:
    StructTypeBuilder(IRGenModule &IGM, llvm::StructType *structTy) :
      SequentialTypeBuilder(IGM), StructTy(structTy) {
    }

    StructTypeInfo *construct(void *buffer, ArrayRef<VarDecl*> fields) {
      return ::new(buffer) StructTypeInfo(StructTy, fields.size());
    }

    StructFieldInfo getFieldInfo(VarDecl *field, const TypeInfo &fieldTI) {
      return StructFieldInfo(field, fieldTI);
    }

    Type getType(VarDecl *field) { return field->getType(); }

    void performLayout(ArrayRef<const TypeInfo *> fieldTypes) {
      StructLayout layout(IGM, LayoutKind::NonHeapObject,
                          LayoutStrategy::Optimal, fieldTypes, StructTy);
      recordLayout(layout, StructTy);
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
  const StructTypeInfo &info = IGF.getFragileTypeInfo(T).as<StructTypeInfo>();

  // FIXME: This field index computation is an ugly hack.
  unsigned FieldIndex = 0;
  for (Decl *D : T->getDecl()->getMembers()) {
    if (D == E->getDecl())
      break;
    if (isa<VarDecl>(D) && !cast<VarDecl>(D)->isProperty())
      ++FieldIndex;
  }

  const StructFieldInfo &field = info.getFields()[FieldIndex];
  lvalue.add<PhysicalStructMember>(field);
  return lvalue;
}


/// emitStructType - Emit all the declarations associated with this oneof type.
void IRGenModule::emitStructType(StructType *st) {
  // FIXME: This is mostly copy-paste from emitExtension;
  // figure out how to refactor! 
  for (Decl *member : st->getDecl()->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
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
      emitOneOfType(cast<OneOfDecl>(member)->getDeclaredType());
      continue;
    case DeclKind::Struct:
      emitStructType(cast<StructDecl>(member)->getDeclaredType());
      continue;
    case DeclKind::Class:
      emitClassType(cast<ClassDecl>(member)->getDeclaredType());
      continue;
    case DeclKind::Var:
      if (cast<VarDecl>(member)->isProperty())
        // Getter/setter will be handled separately.
        continue;
      // FIXME: Will need an implementation here for resilience
      continue;
    case DeclKind::Func: {
      FuncDecl *func = cast<FuncDecl>(member);
      if (func->isStatic()) {
        // Eventually this won't always be the right thing.
        emitStaticMethod(func);
      } else {
        emitInstanceMethod(func);
      }
      continue;
    }
    case DeclKind::OneOfElement: {
      const StructTypeInfo &typeInfo =
          getFragileTypeInfo(st).as<StructTypeInfo>();
      OneOfElementDecl *elt = cast<OneOfElementDecl>(member);
      llvm::Function *fn = getAddrOfInjectionFunction(elt);
      emitInjectionFunction(*this, fn, typeInfo, elt);
      continue;
    }
    case DeclKind::Constructor: {
      emitConstructor(cast<ConstructorDecl>(member));
      continue;
    }
    }
    llvm_unreachable("bad extension member kind");
  }
}

const TypeInfo *TypeConverter::convertStructType(StructType *T) {
  StructTypeBuilder builder(IGM, IGM.createNominalType(T->getDecl()));

  // Collect all the fields from the type.
  SmallVector<VarDecl*, 8> fields;
  for (Decl *D : T->getDecl()->getMembers())
    if (VarDecl *VD = dyn_cast<VarDecl>(D))
      if (!VD->isProperty())
        fields.push_back(VD);

  // Allocate the TypeInfo and register it as a forward-declaration.
  auto structTI = builder.create(fields);
  Types.insert(std::make_pair(T, structTI));

  // Complete the type and return it.
  return builder.complete(fields);
}
