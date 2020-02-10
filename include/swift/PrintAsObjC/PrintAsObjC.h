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

  enum class ObjcHeaderKind {
    /// The header contains only public/open declarations from the module
    Public,
    /// The header contains only SPI declarations from the module. The header
    /// includes the public header.
    Private,
    /// The header contains only internal declarations from the module. The header
    /// includes both the public header and the private header.
    Internal,

    /// The legacy mode where we print only one header that contains everything.
    Single_All,
  };

  /// Print the Objective-C-compatible declarations in a module as a Clang
  /// header.
  ///
  /// Returns true on error.
  bool printAsObjC(raw_ostream &out, ModuleDecl *M, StringRef bridgingHeader,
                   ObjcHeaderKind Kind, ArrayRef<StringRef> Includes);
}

#endif
