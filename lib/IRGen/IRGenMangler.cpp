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
#include "swift/Basic/ManglingMacros.h"

using namespace swift;
using namespace irgen;

std::string IRGenMangler::mangleValueWitness(Type type, ValueWitness witness) {
  beginMangling();
  appendType(type);

  const char *Code = nullptr;
  switch (witness) {
#define VALUE_WITNESS(MANGLING, NAME) \
    case ValueWitness::NAME: Code = #MANGLING; break;
#include "swift/Basic/ValueWitnessMangling.def"
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
