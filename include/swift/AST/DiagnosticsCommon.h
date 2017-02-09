//===--- DiagnosticsCommon.h - Shared Diagnostic Definitions ----*- C++ -*-===//
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
/// \brief This file defines common diagnostics for the whole compiler, as well
/// as some diagnostic infrastructure.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DIAGNOSTICSCOMMON_H
#define SWIFT_DIAGNOSTICSCOMMON_H

#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/LLVM.h"
#include "swift/Config.h"

namespace swift {
  template<typename ...ArgTypes>
  struct Diag;

  namespace detail {
    template<typename T>
    struct DiagWithArguments;
    
    template<typename ...ArgTypes>
    struct DiagWithArguments<void(ArgTypes...)> {
      typedef Diag<ArgTypes...> type;
    };
  }

  enum class StaticSpellingKind : uint8_t;

  namespace diag {

    enum class RequirementKind : uint8_t;

    using DeclAttribute = const DeclAttribute *;

  // Declare common diagnostics objects with their appropriate types.
#define DIAG(KIND,ID,Options,Text,Signature) \
    extern detail::DiagWithArguments<void Signature>::type ID;
#include "DiagnosticsCommon.def"
  }
}

#endif
