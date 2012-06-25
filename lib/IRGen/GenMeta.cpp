//===--- GenMeta.cpp - IR generation for metadata constructs --------------===//
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
//  This file implements IR generation for metadata constructs like
//  metatypes and modules.  These is presently always trivial, but in
//  the future we will likely have some sort of physical
//  representation for at least some metatypes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"

#include "Address.h"
#include "IRGenModule.h"
#include "ScalarTypeInfo.h"
#include "GenType.h"

#include "GenMeta.h"

using namespace swift;
using namespace irgen;

namespace {
  struct EmptyTypeInfo : ScalarTypeInfo<EmptyTypeInfo, TypeInfo> {
    EmptyTypeInfo(llvm::Type *ty)
      : ScalarTypeInfo(ty, Size(0), Alignment(1), IsPOD) {}
    unsigned getExplosionSize(ExplosionKind kind) const { return 0; }
    void getSchema(ExplosionSchema &schema) const {}
    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {}
    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {}
    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {}
    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {}
    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {}
    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {}
    void destroy(IRGenFunction &IGF, Address addr) const {}
  };
}

const TypeInfo *TypeConverter::convertMetaTypeType(MetaTypeType *T) {
  return new EmptyTypeInfo(IGM.Int8Ty);
}

const TypeInfo *TypeConverter::convertModuleType(ModuleType *T) {
  return new EmptyTypeInfo(IGM.Int8Ty);
}

/// Emit a reference to a metatype.
void swift::irgen::emitMetaTypeRef(IRGenFunction &IGF, Type type,
                                   Explosion &explosion) {
  // For now, all metatype types are empty.
}
