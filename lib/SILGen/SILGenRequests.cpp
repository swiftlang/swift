//===--- SILGenRequests.cpp - Requests for SIL Generation  ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/SILGenRequests.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/SourceFile.h"
#include "swift/SIL/SILModule.h"
#include "swift/Subsystems.h"

using namespace swift;

namespace swift {
// Implement the SILGen type zone (zone 12).
#define SWIFT_TYPEID_ZONE SILGen
#define SWIFT_TYPEID_HEADER "swift/AST/SILGenTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
} // end namespace swift

void swift::simple_display(llvm::raw_ostream &out,
                           const SILGenDescriptor &desc) {
  auto *MD = desc.context.dyn_cast<ModuleDecl *>();
  auto *unit = desc.context.dyn_cast<FileUnit *>();
  if (MD) {
    out << "SIL Generation for module " << MD->getName();
  } else {
    assert(unit);
    out << "SIL Generation for file ";
    simple_display(out, unit);
  }
}

SourceLoc swift::extractNearestSourceLoc(const SILGenDescriptor &desc) {
  return SourceLoc();
}

// Define request evaluation functions for each of the SILGen requests.
static AbstractRequestFunction *silGenRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/AST/SILGenTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerSILGenRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::SILGen,
                                     silGenRequestFunctions);
}
