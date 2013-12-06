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

#ifndef SWIFT_AST_MODULE_INTERFACE_PRINTING_H
#define SWIFT_AST_MODULE_INTERFACE_PRINTING_H

#include "swift/Basic/LLVM.h"

namespace swift {
class Module;
struct PrintOptions;

void printModuleInterface(Module *M, raw_ostream &OS,
                          const PrintOptions &Options);

} // namespace swift

#endif // SWIFT_AST_MODULE_INTERFACE_PRINTING_H

