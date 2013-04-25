//===--- GenTuple.cpp - Swift IR Generation For Tuple Types ---------------===//
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
//  This file implements IR generation for tuple types in Swift.  This
//  includes creating the IR type as  well as emitting the primitive access
//  operations.
//
//  It is assumed in several places in IR-generation that the
//  explosion schema of a tuple type is always equal to the appended
//  explosion schemas of the component types.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Pattern.h"
#include "swift/Basic/Optional.h"
#include "llvm/IR/DerivedTypes.h"

#include "ASTVisitor.h"
#include "GenHeap.h"
#include "GenSequential.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"

#include "GenTuple.h"

using namespace swift;
using namespace irgen;

namespace {
  class TupleFieldInfo : public SequentialField<TupleFieldInfo> {
  public:
    TupleFieldInfo(const TupleTypeElt &field, const TypeInfo &type)
      : SequentialField(type), Field(field) {}

    /// The field.
    const TupleTypeElt &Field;

    StringRef getFieldName() const {
      if (Field.hasName())
        return Field.getName().str();
      return "elt";
    }
  };

  /// Layout information for tuple types.
  class TupleTypeInfo : // FIXME: FixedTypeInfo as base is a lie
    public SequentialTypeInfo<TupleTypeInfo, FixedTypeInfo, TupleFieldInfo> {
  public:
    TupleTypeInfo(llvm::Type *T, unsigned numFields)
      : SequentialTypeInfo(T, numFields) {
    }

    /// FIXME: implement
    Nothing_t getNonFixedOffsets(IRGenFunction &IGF) const { return Nothing; }
  };

  class TupleTypeBuilder :
    public SequentialTypeBuilder<TupleTypeBuilder, TupleTypeInfo, TupleTypeElt>{

  public:
    TupleTypeBuilder(IRGenModule &IGM) : SequentialTypeBuilder(IGM) {}

    TupleTypeInfo *construct(void *buffer, ArrayRef<TupleTypeElt> fields) {
      return ::new(buffer) TupleTypeInfo(IGM.Int8Ty, fields.size());
    }

    TupleFieldInfo getFieldInfo(const TupleTypeElt &field,
                                const TypeInfo &fieldTI) {
      return TupleFieldInfo(field, fieldTI);
    }

    Type getType(const TupleTypeElt &field) { return field.getType(); }

    void performLayout(ArrayRef<const TypeInfo *> fieldTypes) {
      StructLayout layout(IGM, LayoutKind::NonHeapObject,
                          LayoutStrategy::Universal, fieldTypes);
      recordLayout(layout, layout.getType());
    }
  };
}

static const TupleTypeInfo &getAsTupleTypeInfo(const TypeInfo &typeInfo) {
  // It'd be nice to get some better verification than this.
#ifdef __GXX_RTTI
  assert(dynamic_cast<const TupleTypeInfo*>(&typeInfo));
#endif

  return typeInfo.as<TupleTypeInfo>();
}

static const TupleTypeInfo &getAsTupleTypeInfo(IRGenFunction &IGF, Type type) {
  assert(type->is<TupleType>());
  return getAsTupleTypeInfo(IGF.getFragileTypeInfo(type));
}

const TypeInfo *TypeConverter::convertTupleType(TupleType *T) {
  TupleTypeBuilder builder(IGM);
  builder.create(T->getFields());
  return builder.complete(T->getFields());
}

void swift::irgen::projectTupleElementFromExplosion(IRGenFunction &IGF,
                                                    CanType tupleType,
                                                    Explosion &tuple,
                                                    unsigned fieldNo,
                                                    Explosion &out) {
  const TupleTypeInfo &tupleTI = getAsTupleTypeInfo(IGF, tupleType);
  const TupleFieldInfo &field = tupleTI.getFields()[fieldNo];
  // If the field requires no storage, there's nothing to do.
  if (field.isEmpty()) {
    return IGF.emitFakeExplosion(field.getTypeInfo(), out);
  }
  
  // Otherwise, project from the base.
  auto fieldRange = field.getProjectionRange(out.getKind());
  ArrayRef<llvm::Value *> element = tuple.getRange(fieldRange.first,
                                                   fieldRange.second);
  out.add(element);
}


OwnedAddress swift::irgen::projectTupleElementAddress(IRGenFunction &IGF,
                                                      OwnedAddress base,
                                                      CanType tupleType,
                                                      unsigned fieldNo) {
  const TupleTypeInfo &tupleTI = getAsTupleTypeInfo(IGF, tupleType);
  const TupleFieldInfo &field = tupleTI.getFields()[fieldNo];
  if (field.isEmpty())
    return {field.getTypeInfo().getUndefAddress(), nullptr};

  auto offsets = tupleTI.getNonFixedOffsets(IGF);
  Address fieldAddr = field.projectAddress(IGF, base.getAddress(), offsets);
  return {fieldAddr, base.getOwner()};
}




/// Emit a string literal, either as a C string pointer or as a (pointer, size)
/// tuple.
// FIXME: Why is this here?
void swift::irgen::emitStringLiteral(IRGenFunction &IGF,
                                     StringRef string,
                                     bool includeSize,
                                     Explosion &out) {
  auto ptr = IGF.IGM.getAddrOfGlobalString(string);
  out.add(ptr);
  if (includeSize)
    out.add(IGF.Builder.getInt64(string.size()));
}
