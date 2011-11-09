//===- DiagnosticConsumer.cpp - Diagnostic Consumer Impl --------*- C++ -*-===//
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
//
//  This file implements the DiagnosticConsumer class.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/DiagnosticConsumer.h"
#include "llvm/ADT/StringRef.h"

using namespace swift;

DiagnosticConsumer::~DiagnosticConsumer() { }
