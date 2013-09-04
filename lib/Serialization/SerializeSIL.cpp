//===--- SerializeSIL.cpp - Read and write SIL ----------------------------===//
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

#include "Serialization.h"
#include "swift/SIL/SILModule.h"

using namespace swift;
using namespace swift::serialization;

namespace {
  class SILSerializer {
  public:
    Serializer &S;

    /// The SILModule currently being serialized.
    const SILModule *M = nullptr;

  public:
    void writeSILFunction(const SILFunction &F);
    void writeSILBasicBlock(const SILBasicBlock &BB);
    void writeSILInstruction(const SILInstruction &SI);

  public:
    SILSerializer(Serializer &S, const SILModule *M) : S(S), M(M) {}
  };
} // end anonymous namespace

void SILSerializer::writeSILFunction(const SILFunction &F) {
  // TODO: handle name, linkage, type.
  for (const SILBasicBlock &BB : F)
    writeSILBasicBlock(BB);
}

void SILSerializer::writeSILBasicBlock(const SILBasicBlock &BB) {
  for (const SILInstruction &SI : BB)
    writeSILInstruction(SI);
}

void SILSerializer::writeSILInstruction(const SILInstruction &SI) {
}

void Serializer::writeSILFunctions(const SILModule *M) {
  if (!M)
    return;

  SILSerializer SILSer(*this, M);
  // Go through all SILFunctions in M, and if it is transparent,
  // write out the SILFunction.
  for (const SILFunction &F : *M) {
    if (F.isTransparent())
      SILSer.writeSILFunction(F);
  }
}
