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
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Debug.h"

using namespace swift;

AnyRequest::HolderBase::~HolderBase() { }

std::string AnyRequest::getAsString() const {
  std::string result;
  {
    llvm::raw_string_ostream out(result);
    simple_display(out, *this);
  }
  return result;
}

AbstractRequestFunction *
Evaluator::getAbstractRequestFunction(uint8_t zoneID, uint8_t requestID) const {
  for (const auto &zone : requestFunctionsByZone) {
    if (zone.first == zoneID) {
      if (requestID < zone.second.size())
        return zone.second[requestID];

      return nullptr;
    }
  }

  return nullptr;
}

void Evaluator::registerRequestFunctions(
                               uint8_t zoneID,
                               ArrayRef<AbstractRequestFunction *> functions) {
#ifndef NDEBUG
  for (const auto &zone : requestFunctionsByZone) {
    assert(zone.first != zoneID);
  }
#endif

  requestFunctionsByZone.push_back({zoneID, functions});
}

Evaluator::Evaluator(DiagnosticEngine &diags,
                     CycleDiagnosticKind shouldDiagnoseCycles)
  : diags(diags), shouldDiagnoseCycles(shouldDiagnoseCycles) { }

bool Evaluator::checkDependency(const AnyRequest &request) {
  // If there is an active request, record it's dependency on this request.
  if (!activeRequests.empty())
    dependencies[activeRequests.back()].push_back(request);

  // Record this as an active request.
  if (activeRequests.insert(request)) {
    return false;
  }

  // Diagnose cycle.
  switch (shouldDiagnoseCycles) {
  case CycleDiagnosticKind::NoDiagnose:
    return true;

  case CycleDiagnosticKind::DebugDiagnose: {
    llvm::errs() << "===CYCLE DETECTED===\n";
    llvm::DenseSet<AnyRequest> visitedAnywhere;
    llvm::SmallVector<AnyRequest, 4> visitedAlongPath;
    std::string prefixStr;
    printDependencies(activeRequests.front(), llvm::errs(), visitedAnywhere,
                      visitedAlongPath, activeRequests.getArrayRef(),
                      prefixStr, /*lastChild=*/true);
    return true;
  }

  case CycleDiagnosticKind::FullDiagnose:
    diagnoseCycle(request);
    return true;
  }

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

void Evaluator::printDependencies(
                            const AnyRequest &request,
                            llvm::raw_ostream &out,
                            llvm::DenseSet<AnyRequest> &visitedAnywhere,
                            llvm::SmallVectorImpl<AnyRequest> &visitedAlongPath,
                            llvm::ArrayRef<AnyRequest> highlightPath,
                            std::string &prefixStr,
                            bool lastChild) const {
  out << prefixStr << " `--";

  // Determine whether this node should be highlighted.
  bool isHighlighted = false;
  if (std::find(highlightPath.begin(), highlightPath.end(), request)
        != highlightPath.end()) {
    isHighlighted = true;
    out.changeColor(llvm::buffer_ostream::Colors::GREEN);
  }

  // Print this node.
  simple_display(out, request);

  // Turn off the highlight.
  if (isHighlighted) {
    out.resetColor();
  }

  // Print the cached value, if known.
  auto cachedValue = cache.find(request);
  if (cachedValue != cache.end()) {
    out << " -> ";
    PrintEscapedString(cachedValue->second.getAsString(), out);
  }

  if (!visitedAnywhere.insert(request).second) {
    // We've already seed this node. Check whether it's part of a cycle.
    if (std::find(visitedAlongPath.begin(), visitedAlongPath.end(), request)
          != visitedAlongPath.end()) {
      // We have a cyclic dependency.
      out.changeColor(llvm::raw_ostream::RED);
      out << " (cyclic dependency)\n";
    } else {
      // We have seen this node before, but it's not a cycle. Elide its
      // children.
      out << " (elided)\n";
    }

    out.resetColor();
  } else if (dependencies.count(request) == 0) {
    // We have not seen this node before, so we don't know its dependencies.
    out.changeColor(llvm::raw_ostream::GREEN);
    out << " (dependency not evaluated)\n";
    out.resetColor();

    // Remove from the visited set.
    visitedAnywhere.erase(request);
  } else {
    // Print children.
    out << "\n";

    // Setup the prefix to print the children.
    prefixStr += ' ';
    prefixStr += (lastChild ? ' ' : '|');
    prefixStr += "  ";

    // Note that this request is along the path.
    visitedAlongPath.push_back(request);

    // Print the children.
    auto &dependsOn = dependencies.find(request)->second;
    for (unsigned i : indices(dependsOn)) {
      printDependencies(dependsOn[i], out, visitedAnywhere, visitedAlongPath,
                        highlightPath, prefixStr, i == dependsOn.size()-1);
    }

    // Drop our changes to the prefix.
    prefixStr.erase(prefixStr.end() - 4, prefixStr.end());

    // Remove from the visited set and path.
    visitedAnywhere.erase(request);
    assert(visitedAlongPath.back() == request);
    visitedAlongPath.pop_back();
  }
}

void Evaluator::printDependencies(const AnyRequest &request,
                                  llvm::raw_ostream &out) const {
  std::string prefixStr;
  llvm::DenseSet<AnyRequest> visitedAnywhere;
  llvm::SmallVector<AnyRequest, 4> visitedAlongPath;
  printDependencies(request, out, visitedAnywhere, visitedAlongPath, { },
                    prefixStr, /*lastChild=*/true);
}

void Evaluator::dumpDependencies(const AnyRequest &request) const {
  printDependencies(request, llvm::dbgs());
}

void Evaluator::printDependenciesGraphviz(llvm::raw_ostream &out) const {
  // Form a list of all of the requests we know about.
  std::vector<AnyRequest> allRequests;
  for (const auto &knownRequest : dependencies) {
    allRequests.push_back(knownRequest.first);
  }

  // Sort the list of requests based on the display strings, so we get
  // deterministic output.
  auto getDisplayString = [&](const AnyRequest &request) {
    std::string result;
    {
      llvm::raw_string_ostream out(result);
      simple_display(out, request);
    }
    return result;
  };
  std::sort(allRequests.begin(), allRequests.end(),
            [&](const AnyRequest &lhs, const AnyRequest &rhs) {
              return getDisplayString(lhs) < getDisplayString(rhs);
            });

  // Manage request IDs to use in the resulting output graph.
  llvm::DenseMap<AnyRequest, unsigned> requestIDs;
  unsigned nextID = 0;

  // Prepopulate the known requests.
  for (const auto &request : allRequests) {
    requestIDs[request] = nextID++;
  }

  auto getRequestID = [&](const AnyRequest &request) {
    auto known = requestIDs.find(request);
    if (known != requestIDs.end()) return known->second;

    // We discovered a new request; record it's ID and add it to the list of
    // all requests.
    allRequests.push_back(request);
    requestIDs[request] = nextID;
    return nextID++;
  };

  auto getNodeName = [&](const AnyRequest &request) {
    std::string result;
    {
      llvm::raw_string_ostream out(result);
      out << "request_" << getRequestID(request);
    }
    return result;
  };

  // Emit the graph header.
  out << "digraph Dependencies {\n";

  // Emit the edges.
  for (const auto &source : allRequests) {
    auto known = dependencies.find(source);
    assert(known != dependencies.end());
    for (const auto &target : known->second) {
      out << "  " << getNodeName(source) << " -> " << getNodeName(target)
          << ";\n";
    }
  }

  out << "\n";

  // Emit the nodes.
  for (unsigned i : indices(allRequests)) {
    const auto &request = allRequests[i];
    out << "  " << getNodeName(request);
    out << " [label=\"";
    PrintEscapedString(request.getAsString(), out);

    auto cachedValue = cache.find(request);
    if (cachedValue != cache.end()) {
      out << " -> ";
      PrintEscapedString(cachedValue->second.getAsString(), out);
    }
    out << "\"];\n";
  }

  // Done!
  out << "}\n";
}

void Evaluator::dumpDependenciesGraphviz() const {
  printDependenciesGraphviz(llvm::dbgs());
}
