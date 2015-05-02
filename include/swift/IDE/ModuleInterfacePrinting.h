//===--- ModuleInterfacePrinting.h - Routines to print module interface ---===//
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

#ifndef SWIFT_IDE_MODULE_INTERFACE_PRINTING_H
#define SWIFT_IDE_MODULE_INTERFACE_PRINTING_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/OptionSet.h"

namespace swift {
class ASTContext;
class ASTPrinter;
class Module;
struct PrintOptions;

namespace ide {

/// Flags used when traversing a module for printing.
enum class ModuleTraversal : unsigned {
  /// Visit modules even if their contents wouldn't be visible to name lookup.
  VisitHidden     = 0x01,
  /// Visit submodules.
  VisitSubmodules = 0x02,
  /// Skip the declarations in a Swift overlay module.
  SkipOverlay     = 0x04,
};

/// Options used to describe the traversal of a module for printing.
typedef OptionSet<ModuleTraversal> ModuleTraversalOptions;

void printModuleInterface(Module *M,
                          ModuleTraversalOptions TraversalOptions,
                          ASTPrinter &Printer, const PrintOptions &Options);

// FIXME: this API should go away when Swift can represent Clang submodules as
// 'swift::Module *' objects.
void printSubmoduleInterface(Module *M, ArrayRef<StringRef> FullModuleName,
                             ModuleTraversalOptions TraversalOptions,
                             ASTPrinter &Printer, const PrintOptions &Options);

/// Print the interface for a header that has been imported via the implicit
/// objc header importing feature.
void printHeaderInterface(StringRef Filename, ASTContext &Ctx,
                          ASTPrinter &Printer, const PrintOptions &Options);

} // namespace ide

} // namespace swift

#endif // SWIFT_IDE_MODULE_INTERFACE_PRINTING_H

