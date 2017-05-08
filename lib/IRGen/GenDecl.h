//===--- GenDecl.h - Swift IR generation for some decl ----------*- C++ -*-===//
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
//  This file provides the private interface to some decl emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENDECL_H
#define SWIFT_IRGEN_GENDECL_H

#include "llvm/IR/CallingConv.h"
#include "DebugTypeInfo.h"
#include "IRGen.h"

namespace llvm {
  class AttributeList;
  class Function;
  class FunctionType;
}
namespace swift {
namespace irgen {
  class IRGenModule;
  class LinkInfo;

  llvm::Function *createFunction(IRGenModule &IGM,
                                 LinkInfo &linkInfo,
                                 llvm::FunctionType *fnType,
                                 llvm::CallingConv::ID cc,
                                 const llvm::AttributeList &attrs,
                                 llvm::Function *insertBefore = nullptr);


  llvm::GlobalVariable *createVariable(IRGenModule &IGM,
                                       LinkInfo &linkInfo,
                                       llvm::Type *objectType,
                                       Alignment alignment,
                                       DebugTypeInfo DebugType=DebugTypeInfo(),
                                       Optional<SILLocation> DebugLoc = None,
                                       StringRef DebugName = StringRef());
}
}

#endif
