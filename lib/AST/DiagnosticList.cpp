//===--- DiagnosticList.cpp - Diagnostic Definitions ----------------------===//
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
//  This file defines all of the diagnostics emitted by Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticList.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/Basic/Assertions.h"
using namespace swift;

// Define all of the diagnostic objects and initialize them with their 
// diagnostic IDs.
namespace swift {
  namespace diag {
#define DIAG(KIND, ID, Group, Options, Text, Signature)                      \
    detail::DiagWithArguments<void Signature>::type ID = {DiagID::ID};
#define FIXIT(ID, Text, Signature) \
    detail::StructuredFixItWithArguments<void Signature>::type ID = {FixItID::ID};
#include "swift/AST/DiagnosticsAll.def"
  } // end namespace diag
} // end namespace swift
