//===--- SymbolGraphOptions.h - Swift SymbolGraph Options -----------------===//
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

#include "llvm/ADT/Triple.h"
#include "swift/AST/AttrKind.h"

#ifndef SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHOPTIONS_H
#define SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHOPTIONS_H

namespace swift {
namespace symbolgraphgen {

struct SymbolGraphOptions {
  /// The directory to output the symbol graph JSON files.
  StringRef OutputDir;

  /// The target of the module.
  llvm::Triple Target; 
  /// Pretty-print the JSON with newlines and indentation.
  bool PrettyPrint;

  /// The minimum access level that symbols must have in order to be
  /// included in the graph.
  AccessLevel MinimumAccessLevel;

  /// Emit members gotten through class inheritance or protocol default
  /// implementations with compound, "SYNTHESIZED" USRs.
  bool EmitSynthesizedMembers;
};

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHOPTIONS_H
