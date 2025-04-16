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

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/TargetParser/Triple.h"

#include "swift/AST/AttrKind.h"

#ifndef SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHOPTIONS_H
#define SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHOPTIONS_H

namespace swift {
namespace symbolgraphgen {

struct SymbolGraphOptions {
  /// The directory to output the symbol graph JSON files.
  StringRef OutputDir = {};

  /// The target of the module.
  llvm::Triple Target = {};
  /// Pretty-print the JSON with newlines and indentation.
  bool PrettyPrint = false;

  /// The minimum access level that symbols must have in order to be
  /// included in the graph.
  AccessLevel MinimumAccessLevel = AccessLevel::Public;

  /// Emit members gotten through class inheritance or protocol default
  /// implementations with compound, "SYNTHESIZED" USRs.
  bool EmitSynthesizedMembers = false;
  
  /// Whether to print informational messages when rendering
  /// a symbol graph.
  bool PrintMessages = false;
  
  /// Whether to skip docs for symbols with compound, "SYNTHESIZED" USRs.
  bool SkipInheritedDocs = false;

  /// Whether to skip emitting symbols that are implementations of protocol requirements or
  /// inherited from protocol extensions.
  bool SkipProtocolImplementations = false;

  /// Whether to emit symbols with SPI information.
  bool IncludeSPISymbols = false;

  /// Whether to include documentation for clang nodes or not.
  bool IncludeClangDocs = false;

  /// Whether to emit "swift.extension" symbols for extensions to external types
  /// along with "extensionTo" relationships instead of directly associating
  /// members and conformances with the extended nominal.
  bool EmitExtensionBlockSymbols = false;

  /// Whether to print information for private symbols in system modules.
  /// This should be left as `false` when printing a full-module symbol graph,
  /// but SourceKit should be able to load the information when pulling symbol
  /// information for individual queries.
  bool PrintPrivateSystemSymbols = false;

  /// If this has a value specifies an explicit allow list of reexported module
  /// names that should be included symbol graph.
  std::optional<llvm::ArrayRef<StringRef>> AllowedReexportedModules = {};

  /// If set, a list of availability platforms to restrict (or block) when
  /// rendering symbol graphs.
  std::optional<llvm::DenseSet<StringRef>> AvailabilityPlatforms = {};

  /// Whether `AvailabilityPlatforms` is an allow list or a block list.
  bool AvailabilityIsBlockList = false;
};

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_SYMBOLGRAPHOPTIONS_H
