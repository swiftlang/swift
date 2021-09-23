//===--- ClangImporterRequests.cpp - Clang Importer Requests --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "swift/Subsystems.h"

using namespace swift;

// Define request evaluation functions for each of the name lookup requests.
static AbstractRequestFunction *clangImporterRequestFunctions[] = {
  nullptr // TODO: remove this whenever a request is actually added.
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/ClangImporter/ClangImporterTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerClangImporterRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::ClangImporter,
                                     clangImporterRequestFunctions);
}

