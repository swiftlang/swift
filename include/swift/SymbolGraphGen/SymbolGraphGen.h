//===--- SymbolGraphGen.h - Swift SymbolGraph Generator -------------------===//
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

#ifndef SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHGEN_H
#define SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHGEN_H

#include "swift/AST/Module.h"
#include "SymbolGraphOptions.h"

namespace swift {
namespace symbolgraphgen {

/// Emit a Symbol Graph JSON file for a module.
int emitSymbolGraphForModule(ModuleDecl *M, const SymbolGraphOptions &Options);

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHGEN_H
