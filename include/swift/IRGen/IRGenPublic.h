//===--- IRGenPublic.h - Public interface to IRGen --------------*- C++ -*-===//
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
#ifndef SWIFT_IRGEN_IRGENPUBLIC_H
#define SWIFT_IRGEN_IRGENPUBLIC_H

namespace llvm {
  class LLVMContext;
  template<typename T, unsigned N> class SmallVector;
}

namespace swift {
class ASTContext;
class LinkLibrary;
class SILModule;

namespace irgen {

class IRGenerator;
class IRGenModule;

/// Create an IRGen module.
std::pair<IRGenerator *, IRGenModule *>
createIRGenModule(SILModule *SILMod, StringRef OutputFilename,
                  StringRef MainInputFilenameForDebugInfo,
                  llvm::LLVMContext &LLVMContext);

/// Delete the IRGenModule and IRGenerator obtained by the above call.
void deleteIRGenModule(std::pair<IRGenerator *, IRGenModule *> &Module);

/// Collect the set of libraries to autolink against by mining the
/// external definitions stored in an AST context.
///
/// This entire thing is a hack that we shouldn't need, but it reflects the
/// fact that we can end up referencing something via an external definition
/// (e.g., an imported Clang declaration) indirectly via the Clang importer
/// that would not be visible directly. In such cases, we would fail to
/// link against the shared library that defines the entity or an overlay that
/// is needed as part of its import from Clang (e.g., the Foundation overlay
/// is needed when bridging NSString, even if there is no other mention of
/// an entity from the Foundation overlay).
llvm::SmallVector<LinkLibrary, 4> collectLinkLibrariesFromExternals(
                                                           ASTContext &ctx);

} // end namespace irgen
} // end namespace swift

#endif
