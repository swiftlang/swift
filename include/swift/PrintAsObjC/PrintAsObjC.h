//===-- PrintAsObjC.h - Emit a header file for a Swift AST --------------===//
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

#ifndef SWIFT_PRINTASOBJC_H
#define SWIFT_PRINTASOBJC_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Attr.h"

namespace swift {
  class ModuleDecl;

  /// Print the Objective-C-compatible declarations in a module as a Clang
  /// header.
  ///
  /// Returns true on error.
  bool printAsObjC(raw_ostream &out, ModuleDecl *M, StringRef bridgingHeader,
                   Accessibility minRequiredAccess);
}

#endif
