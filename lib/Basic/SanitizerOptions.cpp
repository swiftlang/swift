//===--- SanitizerOptions.cpp - Swift Sanitizer options ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// \file
// This file implements the parsing of sanitizer arguments.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/SanitizerOptions.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"

namespace swift {

SanitizerKind parseSanitizerArgValues(const llvm::opt::Arg *A,
                                      DiagnosticEngine *Diags) {
  SanitizerKind kind = SanitizerKind::None;
  for (int i = 0, n = A->getNumValues(); i != n; ++i) {
    const char *Value = A->getValue(i);
    if (StringRef(Value).equals("address")) {
      kind = SanitizerKind::Address;
    } else if (Diags) {
      Diags->diagnose(SourceLoc(), diag::error_unsupported_option_argument,
                      A->getOption().getName(), A->getValue(i));
    }
  }
  return kind;
}

}
