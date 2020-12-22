//===--- SILVerifierCAPI.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SILVerifierCAPI.h"
#include "VerifierPrivate.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"

using namespace swift;
using namespace silverifier;

/// Pass in a type erased SILFunction. Inside this API, we convert back to
/// SILFunction and run the verifier.
extern "C" void SILVerifierCAPI_SILFunction_verify(void *silFunction,
                                                   bool singleFunction) {
  auto *fn = reinterpret_cast<SILFunction *>(silFunction);
  verifySILFunction(fn, singleFunction);
}

/// Pass in a type erased SILModule. Inside this API, we convert back to
/// SILModule and run the verifier.
extern "C" void SILVerifierCAPI_SILModule_verify(void *silModule) {
  auto *mod = reinterpret_cast<SILModule *>(silModule);
  verifySILModule(*mod);
}

/// Pass in a type erased SILFunction. Inside this API, we convert back to
/// SILFunction and verifying critical edges.
extern "C" void
SILVerifierCAPI_SILFunction_verifyCriticalEdges(void *silFunction) {
  auto *fn = reinterpret_cast<SILFunction *>(silFunction);
  verifyCriticalEdgesSILFunction(fn);
}

extern "C" void SILVerifierCAPI_SILProperty_verify(void *silProperty,
                                                   void *silModule) {
  auto *prop = reinterpret_cast<SILProperty *>(silProperty);
  auto *mod = reinterpret_cast<SILModule *>(silModule);
  verifySILProperty(prop, *mod);
}

extern "C" void SILVerifierCAPI_SILVTable_verify(void *silVTable,
                                                 void *silModule) {
  auto *vt = reinterpret_cast<SILVTable *>(silVTable);
  auto *mod = reinterpret_cast<SILModule *>(silModule);
  verifySILVTable(vt, *mod);
}

extern "C" void SILVerifierCAPI_SILWitnessTable_verify(void *silWitnessTable,
                                                       void *silModule) {
  auto *wt = reinterpret_cast<SILWitnessTable *>(silWitnessTable);
  auto *mod = reinterpret_cast<SILModule *>(silModule);
  verifySILWitnessTable(wt, *mod);
}

extern "C" void
SILVerifierCAPI_SILDefaultWitnessTable_verify(void *silWitnessTable,
                                              void *silModule) {
  auto *wt = reinterpret_cast<SILDefaultWitnessTable *>(silWitnessTable);
  auto *mod = reinterpret_cast<SILModule *>(silModule);
  verifySILDefaultWitnessTable(wt, *mod);
}

extern "C" void
SILVerifierCAPI_SILGlobalVariable_verify(void *silGlobalVariable) {
  auto *gv = reinterpret_cast<SILGlobalVariable *>(silGlobalVariable);
  verifySILGlobalVariable(gv);
}
