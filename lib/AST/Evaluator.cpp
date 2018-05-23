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
#include "llvm/Support/Debug.h"

using namespace swift;

AnyRequest::HolderBase::~HolderBase() { }

Evaluator::Evaluator(DiagnosticEngine &diags) : diags(diags) { }

bool Evaluator::checkDependency(const AnyRequest &request) {
  // If there is an active request, record it's dependency on this request.
  if (!activeRequests.empty())
    dependencies[activeRequests.back()].push_back(request);

  // Record this as an active request.
  if (activeRequests.insert(request)) {
    return false;
  }

  diagnoseCycle(request);
  return true;
}

void Evaluator::diagnoseCycle(const AnyRequest &request) {
  request.diagnoseCycle(diags);
  for (const auto &step : reversed(activeRequests)) {
    if (step == request) return;

    step.noteCycleStep(diags);
  }

  llvm_unreachable("Diagnosed a cycle but it wasn't represented in the stack");
}

void Evaluator::printDependencies(const AnyRequest &request,
                                  llvm::raw_ostream &out,
                                  llvm::DenseSet<AnyRequest> &visited,
                                  std::string &prefixStr,
                                  bool lastChild) const {
  out << prefixStr << " `--";

  // Print this node.
  simple_display(out, request);

  if (!visited.insert(request).second) {
    // We've already seed this node, so we have a cyclic dependency.
    out.changeColor(llvm::raw_ostream::RED);
    out << " (cyclic dependency)\n";
    out.resetColor();
  } else if (dependencies.count(request) == 0) {
    // We have not seen this node before, so we don't know its dependencies.
    out.changeColor(llvm::raw_ostream::GREEN);
    out << " (dependency not evaluated)\n";
    out.resetColor();

    // Remove from the visited set.
    visited.erase(request);
  } else {
    // Print children.
    out << "\n";

    // Setup the prefix to print the children.
    prefixStr += ' ';
    prefixStr += (lastChild ? ' ' : '|');
    prefixStr += "  ";

    // Print the children.
    auto &dependsOn = dependencies.find(request)->second;
    for (unsigned i : indices(dependsOn)) {
      printDependencies(dependsOn[i], out, visited, prefixStr,
                        i == dependsOn.size()-1);
    }

    // Drop our changes to the prefix.
    prefixStr.erase(prefixStr.end() - 4, prefixStr.end());

    // Remove from the visited set.
    visited.erase(request);
  }
}

void Evaluator::printDependencies(const AnyRequest &request,
                                  llvm::raw_ostream &out) const {
  std::string prefixStr;
  llvm::DenseSet<AnyRequest> visited;
  printDependencies(request, out, visited, prefixStr, /*lastChild=*/true);
}

void Evaluator::dumpDependencies(const AnyRequest &request) const {
  printDependencies(request, llvm::dbgs());
}


