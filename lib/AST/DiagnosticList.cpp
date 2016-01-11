//===--- DiagnosticList.cpp - Diagnostic Definitions ----------------------===//
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
//  This file defines all of the diagnostics emitted by Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsCommon.h"
using namespace swift;

enum class swift::DiagID : uint32_t {
#define DIAG(KIND,ID,Category,Options,Text,Signature) ID,
#include "swift/AST/DiagnosticsAll.def"
};

// Define all of the diagnostic objects and initialize them with their 
// diagnostic IDs.
namespace swift {
  namespace diag {
#define DIAG(KIND,ID,Category,Options,Text,Signature) \
    detail::DiagWithArguments<void Signature>::type ID = { DiagID::ID };
#include "swift/AST/DiagnosticsAll.def"
  }
}
