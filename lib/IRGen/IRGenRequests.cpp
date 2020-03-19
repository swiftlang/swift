//===--- IRGenRequests.cpp - Requests for LLVM IR Generation --------------===//
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

#include "swift/AST/IRGenRequests.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/SIL/SILModule.h"
#include "swift/Subsystems.h"
#include "llvm/IR/Module.h"

using namespace swift;

namespace swift {
// Implement the IRGen type zone (zone 20).
#define SWIFT_TYPEID_ZONE IRGen
#define SWIFT_TYPEID_HEADER "swift/AST/IRGenTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
} // end namespace swift


void swift::simple_display(llvm::raw_ostream &out,
                           const IRGenDescriptor &desc) {
  auto *MD = desc.Ctx.dyn_cast<ModuleDecl *>();
  auto *SF = desc.Ctx.dyn_cast<SourceFile *>();
  if (MD) {
    out << "IR Generation for module " << MD->getName();
  } else {
    assert(SF);
    out << "IR Generation for file ";
    out << '\"' << cast<LoadedFile>(SF)->getFilename() << '\"';
  }
}

SourceLoc swift::extractNearestSourceLoc(const IRGenDescriptor &desc) {
  return SourceLoc();
}

// Define request evaluation functions for each of the IRGen requests.
static AbstractRequestFunction *irGenRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/AST/IRGenTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerIRGenRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::IRGen, irGenRequestFunctions);
}
