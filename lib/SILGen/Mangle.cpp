//===--- Mangle.cpp - Symbol mangling of SILDeclRefs -----------*- C++ -*-===//
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

#include "SILGen.h"
#include "swift/AST/Mangle.h"

using namespace swift;
using namespace Lowering;
using namespace Mangle;

SILFunction *
SILGenModule::getOrCreateReabstractionThunk(SILLocation loc,
                                            CanSILFunctionType thunkType,
                                            CanSILFunctionType fromType,
                                            CanSILFunctionType toType) {
  // Mangle the reabstraction thunk.
  llvm::SmallString<256> buffer;
  {
    llvm::raw_svector_ostream stream(buffer);
    Mangler mangler(stream);

    // This is actually the SIL helper function.  For now, IR-gen
    // makes the actual thunk.
    stream << "_TTR";
    if (auto generics = thunkType->getGenericParams()) {
      stream << 'G';
      mangler.bindGenericParameters(generics, /*mangle*/ true);
    }
    mangler.mangleType(fromType, ExplosionKind::Minimal, /*uncurry*/ 0);
    mangler.mangleType(toType, ExplosionKind::Minimal, /*uncurry*/ 0);
  }

  return M.getOrCreateSharedFunction(loc, buffer.str(), thunkType,
                                     IsBare, IsTransparent);
}

/// Mangle this entity into the given stream.
void SILGenModule::mangleConstant(SILDeclRef c, SILFunction *f) {
  SmallVector<char, 128> buffer;
  f->getMutableName() = c.mangle(buffer);
}
