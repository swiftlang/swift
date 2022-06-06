//===--- SwiftToClangInteropContext.h - Interop context ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTASCLANG_SWIFTTOCLANGINTEROPCONTEXT_H
#define SWIFT_PRINTASCLANG_SWIFTTOCLANGINTEROPCONTEXT_H

#include <memory>

namespace swift {

class IRABIDetailsProvider;
class IRGenOptions;
class ModuleDecl;

/// The \c SwiftToClangInteropContext class is responsible for providing
/// access to the other required subsystems of the compiler during the emission
/// of a clang header. It provides access to the other subsystems lazily to
/// ensure avoid any additional setup cost that's not required.
class SwiftToClangInteropContext {
public:
  SwiftToClangInteropContext(ModuleDecl &mod, const IRGenOptions &irGenOpts);
  ~SwiftToClangInteropContext();

  IRABIDetailsProvider &getIrABIDetails();

private:
  ModuleDecl &mod;
  const IRGenOptions &irGenOpts;
  std::unique_ptr<IRABIDetailsProvider> irABIDetails;
};

} // end namespace swift

#endif
