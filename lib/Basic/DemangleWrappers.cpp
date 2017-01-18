//===--- DemangleWrappers.cpp - Swift Name Demangling ---------------------===//
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

#include "swift/Basic/DemangleWrappers.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;
using namespace demangle_wrappers;

static StringRef getNodeKindString(swift::Demangle::Node::Kind k) {
  switch (k) {
#define NODE(ID)                                                               \
  case Node::Kind::ID:                                                         \
    return #ID;
#include "swift/Basic/DemangleNodes.def"
  }
  llvm_unreachable("bad node kind");
}

static void printNode(llvm::raw_ostream &Out, const Node *node,
                      unsigned depth) {
  // Indent two spaces per depth.
  Out.indent(depth * 2);
  if (!node) {
    Out << "<<NULL>>";
    return;
  }
  Out << "kind=" << getNodeKindString(node->getKind());
  if (node->hasText()) {
    Out << ", text=\"" << node->getText() << '\"';
  }
  if (node->hasIndex()) {
    Out << ", index=" << node->getIndex();
  }
  Out << '\n';
  for (auto &child : *node) {
    printNode(Out, child.get(), depth + 1);
  }
}

void NodeDumper::dump() const { print(llvm::errs()); }

void NodeDumper::print(llvm::raw_ostream &Out) const {
  printNode(Out, Root.get(), 0);
}

void swift::demangle_wrappers::dumpNode(const NodePointer &Root) {
  NodeDumper(Root).dump();
}

namespace {
/// A pretty-stack-trace node for demangling trees.
class PrettyStackTraceNode : public llvm::PrettyStackTraceEntry {
  const char *Action;
  Node *TheNode;
public:
  PrettyStackTraceNode(const char *action, Node *node)
    : Action(action), TheNode(node) {}
  void print(llvm::raw_ostream &out) const override {
    out << "While " << Action << ' ';
    if (!TheNode) {
      out << "<<null demangling node>>\n";
    } else {
      out << "demangling tree:\n";
      printNode(out, TheNode, 4);
    }
  }
};
} // end unnamed namespace

NodePointer
swift::demangle_wrappers::demangleSymbolAsNode(llvm::StringRef MangledName,
                                               const DemangleOptions &Options) {
  PrettyStackTraceStringAction prettyStackTrace("demangling string",
                                                MangledName);
  return swift::Demangle::demangleSymbolAsNode(MangledName.data(),
                                               MangledName.size(), Options);
}

std::string nodeToString(NodePointer Root,
                         const DemangleOptions &Options) {
  PrettyStackTraceNode trace("printing", Root.get());
  return swift::Demangle::nodeToString(Root, Options);
}

std::string swift::demangle_wrappers::demangleSymbolAsString(
    llvm::StringRef MangledName, const DemangleOptions &Options) {
  PrettyStackTraceStringAction prettyStackTrace("demangling string",
                                                MangledName);
  return swift::Demangle::demangleSymbolAsString(MangledName.data(),
                                                 MangledName.size(), Options);
}

std::string swift::demangle_wrappers::demangleTypeAsString(
    llvm::StringRef MangledName, const DemangleOptions &Options) {
  PrettyStackTraceStringAction prettyStackTrace("demangling type string",
                                                MangledName);
  return swift::Demangle::demangleTypeAsString(MangledName.data(),
                                               MangledName.size(), Options);
}

