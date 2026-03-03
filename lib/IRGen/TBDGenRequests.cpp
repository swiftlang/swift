//===--- TBDGenRequests.cpp - Requests for TBD Generation  ----------------===//
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

#include "swift/AST/TBDGenRequests.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/Module.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/IRGen/TBDGen.h"
#include "swift/Subsystems.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/TextAPI/InterfaceFile.h"

#include "APIGen.h"

using namespace swift;

namespace swift {
// Implement the TBDGen type zone (zone 14).
#define SWIFT_TYPEID_ZONE TBDGen
#define SWIFT_TYPEID_HEADER "swift/AST/TBDGenTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
} // end namespace swift

//----------------------------------------------------------------------------//
// GenerateTBDRequest computation.
//----------------------------------------------------------------------------//

FileUnit *TBDGenDescriptor::getSingleFile() const {
  return Input.dyn_cast<FileUnit *>();
}

ModuleDecl *TBDGenDescriptor::getParentModule() const {
  if (auto *module = Input.dyn_cast<ModuleDecl *>())
    return module;
  return cast<FileUnit *>(Input)->getParentModule();
}

const StringRef TBDGenDescriptor::getDataLayoutString() const {
  auto &ctx = getParentModule()->getASTContext();
  auto *clang = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
  return llvm::StringRef(clang->getTargetInfo().getDataLayoutString());
}

const llvm::Triple &TBDGenDescriptor::getTarget() const {
  return getParentModule()->getASTContext().LangOpts.Target;
}

bool TBDGenDescriptor::operator==(const TBDGenDescriptor &other) const {
  return Input == other.Input && Opts == other.Opts;
}

llvm::hash_code swift::hash_value(const TBDGenDescriptor &desc) {
  return llvm::hash_combine(desc.getFileOrModule(), desc.getOptions());
}

void swift::simple_display(llvm::raw_ostream &out,
                           const TBDGenDescriptor &desc) {
  out << "Generate TBD for ";
  if (auto *module = desc.getFileOrModule().dyn_cast<ModuleDecl *>()) {
    out << "module ";
    simple_display(out, module);
  } else {
    out << "file ";
    simple_display(out, cast<FileUnit *>(desc.getFileOrModule()));
  }
}

SourceLoc swift::extractNearestSourceLoc(const TBDGenDescriptor &desc) {
  return extractNearestSourceLoc(desc.getFileOrModule());
}

// Define request evaluation functions for each of the TBDGen requests.
static AbstractRequestFunction *tbdGenRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/AST/TBDGenTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerTBDGenRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::TBDGen, tbdGenRequestFunctions);
}
