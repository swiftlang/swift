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

} // end namespace irgen
} // end namespace swift

#endif
