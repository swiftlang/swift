//===--- PrintAsObjC.h - Emit a header file for a Swift AST -----*- C++ -*-===//
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

#ifndef SWIFT_PRINTASOBJC_H
#define SWIFT_PRINTASOBJC_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/Identifier.h"

namespace swift {
  class ModuleDecl;
  class ValueDecl;

  /// Print the Objective-C-compatible declarations in a module as a Clang
  /// header.
  ///
  /// Returns true on error.
  bool printAsObjC(raw_ostream &out, ModuleDecl *M, StringRef bridgingHeader,
                   Accessibility minRequiredAccess);

  /// Get the name for a value decl D if D is exported to ObjC, PreferredName is
  /// specified to perform what-if analysis, shadowing D's original name during
  /// computation.
  ///
  /// Returns a pair of Identifier and ObjCSelector, only one of which is valid.
  std::pair<Identifier, ObjCSelector> getObjCNameForSwiftDecl(const ValueDecl *VD,
    DeclName PreferredName);
}

#endif
