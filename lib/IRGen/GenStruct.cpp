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
#include "swift/AST/Pattern.h"
#include "swift/Basic/Optional.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

#include "GenInit.h"
#include "GenMeta.h"
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

static void emitValueConstructor(IRGenModule &IGM,
                                 llvm::Function *fn,
                                 ConstructorDecl *ctor) {
  auto thisDecl = ctor->getImplicitThisDecl();

  Pattern *pats[] = {
    new (IGM.Context) AnyPattern(SourceLoc()),
    ctor->getArguments()
  };
  pats[0]->setType(MetaTypeType::get(thisDecl->getType(), IGM.Context));
  IRGenFunction IGF(IGM, ctor->getType()->getCanonicalType(), pats,
                    ExplosionKind::Minimal, 1, fn, Prologue::Standard);

  const StructTypeInfo &thisTI =
      IGF.getFragileTypeInfo(thisDecl->getType()).as<StructTypeInfo>();
  Initialization I;
  I.registerObject(IGF, I.getObjectForDecl(thisDecl),
                   thisDecl->hasFixedLifetime() ? NotOnHeap : OnHeap, thisTI);
  OwnedAddress addr = I.emitVariable(IGF, thisDecl, thisTI);

  unsigned FieldIndex = 0;
  TuplePattern *TP = cast<TuplePattern>(ctor->getArguments());
  StructDecl *SD = cast<StructDecl>(ctor->getDeclContext());
  for (Decl *D : SD->getMembers()) {
    if (isa<VarDecl>(D) && !cast<VarDecl>(D)->isProperty()) {
      TypedPattern *P = cast<TypedPattern>(TP->getFields()[FieldIndex].getPattern());
      NamedPattern *NP = cast<NamedPattern>(P->getSubPattern());
      OwnedAddress inaddr = IGF.getLocalVar(NP->getDecl());

      Explosion e(ExplosionKind::Minimal);
      const StructFieldInfo &field = thisTI.getFields()[FieldIndex];
      field.getTypeInfo().load(IGF, inaddr, e);
      field.getTypeInfo().initialize(IGF, e, field.projectAddress(IGF, addr));
      FieldIndex++;
    }
  }

  I.markInitialized(IGF, I.getObjectForDecl(thisDecl));

  IGF.emitConstructorBody(ctor);
}

static unsigned getFieldIndex(StructDecl *baseStruct, VarDecl *target) {
  // FIXME: This is an ugly hack.
  unsigned index = 0;
  for (Decl *member : baseStruct->getMembers()) {
    if (member == target) return index;
    if (auto var = dyn_cast<VarDecl>(member))
      if (!var->isProperty())
        ++index;
  }
  llvm_unreachable("didn't find field in type!");
}

static LValue emitPhysicalStructMemberLValue(IRGenFunction &IGF,
                                             Expr *base,
                                             StructDecl *baseStruct,
                                             const StructTypeInfo &baseTI,
                                             VarDecl *field) {
  LValue lvalue = IGF.emitLValue(base);
  unsigned fieldIndex = getFieldIndex(baseStruct, field);
  auto &fieldI = baseTI.getFields()[fieldIndex];
  lvalue.add<PhysicalStructMember>(fieldI);
  return lvalue;
}

LValue irgen::emitPhysicalStructMemberLValue(IRGenFunction &IGF,
                                             MemberRefExpr *E) {
  auto lvalueType = E->getBase()->getType()->castTo<LValueType>();
  auto baseType = lvalueType->getObjectType()->castTo<StructType>();
  auto &baseTI = IGF.getFragileTypeInfo(baseType).as<StructTypeInfo>();
  return ::emitPhysicalStructMemberLValue(IGF, E->getBase(),
                                          baseType->getDecl(), baseTI,
                                          E->getDecl());
}

LValue irgen::emitPhysicalStructMemberLValue(IRGenFunction &IGF,
                                             GenericMemberRefExpr *E) {
  auto lvalueType = E->getBase()->getType()->castTo<LValueType>();
  auto baseType = lvalueType->getObjectType()->castTo<BoundGenericType>();
  auto &baseTI = IGF.getFragileTypeInfo(baseType).as<StructTypeInfo>();
  return ::emitPhysicalStructMemberLValue(IGF, E->getBase(),
                                          cast<StructDecl>(baseType->getDecl()),
                                          baseTI, cast<VarDecl>(E->getDecl()));
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
    case DeclKind::Constructor: {
      auto ctor = cast<ConstructorDecl>(member);
      if (!ctor->getBody()) {
        llvm::Function *fn = getAddrOfConstructor(ctor, ExplosionKind::Minimal);
        emitValueConstructor(*this, fn, ctor);
      } else {
        emitConstructor(ctor);
      }
      continue;
    }
    }
    llvm_unreachable("bad extension member kind");
  }
}

const TypeInfo *TypeConverter::convertStructType(StructDecl *D) {
  StructTypeBuilder builder(IGM, IGM.createNominalType(D));

  // Collect all the fields from the type.
  SmallVector<VarDecl*, 8> fields;
  for (Decl *D : D->getMembers())
    if (VarDecl *VD = dyn_cast<VarDecl>(D))
      if (!VD->isProperty())
        fields.push_back(VD);

  // Allocate the TypeInfo and register it as a forward-declaration.
  // We do this before we look at any of the child types.
  auto structTI = builder.create(fields);
  auto typesMapKey = D->getDeclaredType().getPointer();
  Types.insert(std::make_pair(typesMapKey, structTI));

  // Complete the type and return it.
  return builder.complete(fields);
}
