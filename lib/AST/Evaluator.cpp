//===--- Evaluator.cpp - Request Evaluator Implementation -----------------===//
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
// This file implements the Evaluator class that evaluates and caches
// requests.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/Evaluator.h"
#include "swift/Basic/Range.h"

using namespace swift;

AnyRequest::HolderBase::~HolderBase() { }

Evaluator::Evaluator(DiagnosticEngine &diags) : diags(diags) { }

void Evaluator::diagnoseCycle(const AnyRequest &request) {
  request.diagnoseCycle(diags);
  for (const auto &step : reversed(activeRequests)) {
    if (step == request) return;

    step.noteCycleStep(diags);
  }

  llvm_unreachable("Diagnosed a cycle but it wasn't represented in the stack");
}

