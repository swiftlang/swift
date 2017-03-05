//===--- DiagnosticsSema.h - Diagnostic Definitions -------------*- C++ -*-===//
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
//
/// \file
/// \brief This file defines diagnostics for semantic analysis.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DIAGNOSTICSSEMA_H
#define SWIFT_DIAGNOSTICSSEMA_H

#include "swift/AST/DiagnosticsCommon.h"

namespace swift {
  class SwitchStmt;
  namespace diag {

    /// Describes the kind of requirement in a protocol.
    enum class RequirementKind : uint8_t {
      Constructor,
      Func,
      Var,
      Subscript
    };

  // Declare common diagnostics objects with their appropriate types.
#define DIAG(KIND,ID,Options,Text,Signature) \
    extern detail::DiagWithArguments<void Signature>::type ID;
#include "DiagnosticsSema.def"
  }
  void diagnoseMissingCases(ASTContext &Context, const SwitchStmt *SwitchS,
                            Diagnostic Id);
}

#endif
