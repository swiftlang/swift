//===--- ImportedModules.h -- generates the list of imported modules ------===//
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

#ifndef SWIFT_FRONTENDTOOL_IMPORTEDMODULES_H
#define SWIFT_FRONTENDTOOL_IMPORTEDMODULES_H

namespace swift {

class ASTContext;
class FrontendOptions;
class ModuleDecl;

/// Emit the names of the modules imported by \c mainModule.
bool emitImportedModules(ASTContext &Context, ModuleDecl *mainModule,
                         const FrontendOptions &opts);
} // end namespace swift

#endif
