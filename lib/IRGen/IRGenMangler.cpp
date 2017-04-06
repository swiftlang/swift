//===--- IRGenMangler.cpp - mangling of IRGen symbols ---------------------===//
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

#include "IRGenMangler.h"
#include "swift/Demangling/ManglingMacros.h"

using namespace swift;
using namespace irgen;

std::string IRGenMangler::mangleValueWitness(Type type, ValueWitness witness) {
  beginMangling();
  appendType(type);

  const char *Code = nullptr;
  switch (witness) {
#define VALUE_WITNESS(MANGLING, NAME) \
    case ValueWitness::NAME: Code = #MANGLING; break;
#include "swift/Demangling/ValueWitnessMangling.def"
    case ValueWitness::Size:
    case ValueWitness::Flags:
    case ValueWitness::Stride:
    case ValueWitness::ExtraInhabitantFlags:
      llvm_unreachable("not a function witness");
  }
  appendOperator("w", Code);
  return finalize();
}

std::string IRGenMangler::manglePartialApplyForwarder(StringRef FuncName) {
  if (FuncName.empty()) {
    beginMangling();
  } else {
    if (FuncName.startswith(MANGLING_PREFIX_STR)) {
      Buffer << FuncName;
    } else {
      beginMangling();
      appendIdentifier(FuncName);
    }
  }
  appendOperator("TA");
  return finalize();
}

std::string IRGenMangler::mangleTypeForReflection(Type Ty,
                                                  ModuleDecl *Module,
                                                  bool isSingleFieldOfBox) {
  Mod = Module;
  OptimizeProtocolNames = false;
  appendType(Ty);
  if (isSingleFieldOfBox)
    appendOperator("Xb");
  return finalize();
}

std::string IRGenMangler::mangleTypeForLLVMTypeName(CanType Ty) {
  // To make LLVM IR more readable we always add a 'T' prefix so that type names
  // don't start with a digit and don't need to be quoted.
  Buffer << 'T';
  if (auto P = dyn_cast<ProtocolType>(Ty)) {
    appendProtocolName(P->getDecl());
    appendOperator("P");
  } else {
    appendType(Ty);
  }
  return finalize();
}

std::string IRGenMangler::
mangleProtocolForLLVMTypeName(ProtocolCompositionType *type) {
  SmallVector<ProtocolDecl *, 4> protocols;
  type->getExistentialTypeProtocols(protocols);

  if (protocols.empty()) {
    Buffer << "Any";
  } else {
    // To make LLVM IR more readable we always add a 'T' prefix so that type names
    // don't start with a digit and don't need to be quoted.
    Buffer << 'T';
    for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
      appendProtocolName(protocols[i]);
      if (i == 0)
        appendOperator("_");
    }
    appendOperator("p");
  }
  return finalize();
}
