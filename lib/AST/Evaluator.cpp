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
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

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
                               Zone zone,
                               ArrayRef<AbstractRequestFunction *> functions) {
  uint8_t zoneID = static_cast<uint8_t>(zone);
#ifndef NDEBUG
  for (const auto &zone : requestFunctionsByZone) {
    assert(zone.first != zoneID);
  }
#endif

  requestFunctionsByZone.push_back({zoneID, functions});
}

static evaluator::DependencyRecorder::Mode
computeDependencyModeFromFlags(const LangOptions &opts) {
  using Mode = evaluator::DependencyRecorder::Mode;
  if (opts.DirectIntramoduleDependencies) {
    return Mode::DirectDependencies;
  }

  return Mode::LegacyCascadingDependencies;
}

Evaluator::Evaluator(DiagnosticEngine &diags, const LangOptions &opts)
    : diags(diags),
      debugDumpCycles(opts.DebugDumpCycles),
      buildDependencyGraph(opts.BuildRequestDependencyGraph),
      recorder{computeDependencyModeFromFlags(opts)} {}

void Evaluator::emitRequestEvaluatorGraphViz(llvm::StringRef graphVizPath) {
  std::error_code error;
  llvm::raw_fd_ostream out(graphVizPath, error, llvm::sys::fs::F_Text);
  printDependenciesGraphviz(out);
}

bool Evaluator::checkDependency(const ActiveRequest &request) {
  if (buildDependencyGraph) {
    // Insert the request into the dependency graph if we haven't already.
    auto req = AnyRequest(request);
    dependencies.insert({req, {}});

    // If there is an active request, record it's dependency on this request.
    if (!activeRequests.empty()) {
      auto activeDeps = dependencies.find_as(activeRequests.back());
      assert(activeDeps != dependencies.end());
      activeDeps->second.push_back(req);
    }
  }

  // Record this as an active request.
  if (activeRequests.insert(request))
    return false;

  // Diagnose cycle.
  diagnoseCycle(request);
  return true;
}

void Evaluator::diagnoseCycle(const ActiveRequest &request) {
  if (debugDumpCycles) {
    llvm::errs() << "===CYCLE DETECTED===\n";
    llvm::DenseSet<AnyRequest> visitedAnywhere;
    llvm::SmallVector<AnyRequest, 4> visitedAlongPath;
    std::string prefixStr;
    SmallVector<AnyRequest, 8> highlightPath;
    for (auto &req : activeRequests)
      highlightPath.push_back(AnyRequest(req));
    printDependencies(AnyRequest(activeRequests.front()), llvm::errs(),
                      visitedAnywhere, visitedAlongPath, highlightPath,
                      prefixStr, /*lastChild=*/true);
  }

  request.diagnoseCycle(diags);
  for (const auto &step : llvm::reverse(activeRequests)) {
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
    printEscapedString(cachedValue->second.getAsString(), out);
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
  llvm::DenseMap<AnyRequest, unsigned> inDegree;
  for (const auto &source : allRequests) {
    auto known = dependencies.find(source);
    assert(known != dependencies.end());
    for (const auto &target : known->second) {
      out << "  " << getNodeName(source) << " -> " << getNodeName(target)
          << ";\n";
      ++inDegree[target];
    }
  }

  out << "\n";

  static const char *colorNames[] = {
    "aquamarine",
    "blueviolet",
    "brown",
    "burlywood",
    "cadetblue",
    "chartreuse",
    "chocolate",
    "coral",
    "cornflowerblue",
    "crimson"
  };
  const unsigned numColorNames = sizeof(colorNames) / sizeof(const char *);

  llvm::DenseMap<unsigned, unsigned> knownBuffers;
  auto getColor = [&](const AnyRequest &request) -> Optional<const char *> {
    SourceLoc loc = request.getNearestLoc();
    if (loc.isInvalid())
      return None;

    unsigned bufferID = diags.SourceMgr.findBufferContainingLoc(loc);
    auto knownId = knownBuffers.find(bufferID);
    if (knownId == knownBuffers.end()) {
      knownId = knownBuffers.insert({bufferID, knownBuffers.size()}).first;
    }
    return colorNames[knownId->second % numColorNames];
  };

  // Emit the nodes.
  for (unsigned i : indices(allRequests)) {
    const auto &request = allRequests[i];
    out << "  " << getNodeName(request);
    out << " [label=\"";
    printEscapedString(request.getAsString(), out);

    auto cachedValue = cache.find(request);
    if (cachedValue != cache.end()) {
      out << " -> ";
      printEscapedString(cachedValue->second.getAsString(), out);
    }
    out << "\"";

    if (auto color = getColor(request)) {
      out << ", fillcolor=\"" << *color << "\"";
    }

    out << "];\n";
  }

  // Emit "fake" nodes for each of the source buffers we encountered, so
  // we know which file we're working from.
  // FIXME: This approximates a "top level" request for, e.g., type checking
  // an entire source file.
  std::vector<unsigned> sourceBufferIDs;
  for (const auto &element : knownBuffers) {
    sourceBufferIDs.push_back(element.first);
  }
  std::sort(sourceBufferIDs.begin(), sourceBufferIDs.end());
  for (unsigned bufferID : sourceBufferIDs) {
    out << "  buffer_" << bufferID << "[label=\"";
    printEscapedString(diags.SourceMgr.getIdentifierForBuffer(bufferID), out);
    out << "\"";

    out << ", shape=\"box\"";
    out << ", fillcolor=\""
        << colorNames[knownBuffers[bufferID] % numColorNames] << "\"";
    out << "];\n";
  }

  // Emit "false" dependencies from source buffer IDs to any requests that (1)
  // have no other incomining edges and (2) can be associated with a source
  // buffer.
  for (const auto &request : allRequests) {
    if (inDegree[request] > 0)
      continue;

    SourceLoc loc = request.getNearestLoc();
    if (loc.isInvalid())
      continue;

    unsigned bufferID = diags.SourceMgr.findBufferContainingLoc(loc);
    out << "  buffer_" << bufferID << " -> " << getNodeName(request) << ";\n";
  }

  // Done!
  out << "}\n";
}

void Evaluator::dumpDependenciesGraphviz() const {
  printDependenciesGraphviz(llvm::dbgs());
}

void evaluator::DependencyRecorder::realize(
    const DependencyCollector::Reference &ref) {
  auto *source = getActiveDependencySourceOrNull();
  assert(source && "cannot realize dependency without associated file!");
  if (!source->isPrimary()) {
    return;
  }
  fileReferences[source].insert(ref);
}

void evaluator::DependencyCollector::addUsedMember(NominalTypeDecl *subject,
                                                   DeclBaseName name) {
  if (parent.mode == DependencyRecorder::Mode::DirectDependencies) {
    scratch.insert(
        Reference::usedMember(subject, name, parent.isActiveSourceCascading()));
  }
  return parent.realize(
      Reference::usedMember(subject, name, parent.isActiveSourceCascading()));
}

void evaluator::DependencyCollector::addPotentialMember(
    NominalTypeDecl *subject) {
  if (parent.mode == DependencyRecorder::Mode::DirectDependencies) {
    scratch.insert(
        Reference::potentialMember(subject, parent.isActiveSourceCascading()));
  }
  return parent.realize(
      Reference::potentialMember(subject, parent.isActiveSourceCascading()));
}

void evaluator::DependencyCollector::addTopLevelName(DeclBaseName name) {
  if (parent.mode == DependencyRecorder::Mode::DirectDependencies) {
    scratch.insert(Reference::topLevel(name, parent.isActiveSourceCascading()));
  }
  return parent.realize(
      Reference::topLevel(name, parent.isActiveSourceCascading()));
}

void evaluator::DependencyCollector::addDynamicLookupName(DeclBaseName name) {
  if (parent.mode == DependencyRecorder::Mode::DirectDependencies) {
    scratch.insert(Reference::dynamic(name, parent.isActiveSourceCascading()));
  }
  return parent.realize(
      Reference::dynamic(name, parent.isActiveSourceCascading()));
}

void evaluator::DependencyRecorder::record(
    const llvm::SetVector<swift::ActiveRequest> &stack,
    llvm::function_ref<void(DependencyCollector &)> rec) {
  assert(!isRecording && "Probably not a good idea to allow nested recording");
  auto *source = getActiveDependencySourceOrNull();
  if (!source || !source->isPrimary()) {
    return;
  }

  llvm::SaveAndRestore<bool> restore(isRecording, true);

  DependencyCollector collector{*this};
  rec(collector);
  if (collector.empty()) {
    return;
  }

  return unionNearestCachedRequest(stack.getArrayRef(), collector.scratch);
}

void evaluator::DependencyRecorder::replay(
    const llvm::SetVector<swift::ActiveRequest> &stack,
    const swift::ActiveRequest &req) {
  assert(!isRecording && "Probably not a good idea to allow nested recording");

  auto *source = getActiveDependencySourceOrNull();
  if (mode == Mode::LegacyCascadingDependencies) {
    return;
  }

  if (!source || !source->isPrimary()) {
    return;
  }

  if (!req.isCached()) {
    return;
  }

  auto entry = requestReferences.find_as(req);
  if (entry == requestReferences.end()) {
    return;
  }

  for (const auto &ref : entry->second) {
    realize(ref);
  }

  // N.B. This is a particularly subtle detail of the replay unioning step. The
  // evaluator does not push cached requests onto the active request stack,
  // so it is possible (and, in fact, quite likely) we'll wind up with an
  // empty request stack. The remaining troublesome case is when we have a
  // cached request being run through the uncached path - take the
  // InterfaceTypeRequest, which involves many component requests, most of which
  // are themselves cached. In such a case, the active stack will look like
  //
  // -> TypeCheckSourceFileRequest
  // -> ...
  // -> InterfaceTypeRequest
  // -> ...
  // -> UnderlyingTypeRequest
  //
  // We want the UnderlyingTypeRequest to union its names into the
  // InterfaceTypeRequest, and if we were to just start searching the active
  // stack backwards for a cached request we would find...
  // the UnderlyingTypeRequest! So, we'll just drop it from consideration.
  //
  // We do *not* have to consider this during the recording step because none
  // of the name lookup requests (or any dependency sinks in general) are
  // cached. Should this change in the future, we will need to sink this logic
  // into the union step itself.
  const size_t d = (!stack.empty() && req == stack.back()) ? 1 : 0;
  return unionNearestCachedRequest(stack.getArrayRef().drop_back(d),
                                   entry->second);
}

void evaluator::DependencyRecorder::unionNearestCachedRequest(
    ArrayRef<swift::ActiveRequest> stack,
    const DependencyCollector::ReferenceSet &scratch) {
  assert(mode != Mode::LegacyCascadingDependencies);
  auto nearest = std::find_if(stack.rbegin(), stack.rend(),
                              [](const auto &req){ return req.isCached(); });
  if (nearest == stack.rend()) {
    return;
  }

  auto entry = requestReferences.find_as(*nearest);
  if (entry == requestReferences.end()) {
    requestReferences.insert({AnyRequest(*nearest), scratch});
  } else {
    entry->second.insert(scratch.begin(), scratch.end());
  }
}

using namespace swift;

void evaluator::DependencyRecorder::enumerateReferencesInFile(
    const SourceFile *SF, ReferenceEnumerator f) const {
  auto entry = fileReferences.find(SF);
  if (entry == fileReferences.end()) {
    return;
  }

  for (const auto &ref : entry->getSecond()) {
    switch (ref.kind) {
    case DependencyCollector::Reference::Kind::Empty:
    case DependencyCollector::Reference::Kind::Tombstone:
      llvm_unreachable("Cannot enumerate dead reference!");
    case DependencyCollector::Reference::Kind::UsedMember:
    case DependencyCollector::Reference::Kind::PotentialMember:
    case DependencyCollector::Reference::Kind::TopLevel:
    case DependencyCollector::Reference::Kind::Dynamic:
      f(ref);
    }
  }
}
