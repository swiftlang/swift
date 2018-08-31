//===--- swift-demangle-yamldump.cpp --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This tool is similar to the demangler but is intended to /not/ be installed
/// into the OS. This means that it can link against llvm support and friends
/// and thus provide extra functionality. Today it is only used to dump the
/// demangler tree in YAML form for easy processing.
///
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/ManglingMacros.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"

// For std::rand, to work around a bug if main()'s first function call passes
// argv[0].
#if defined(__CYGWIN__)
#include <cstdlib>
#endif

#include <iostream>

using namespace swift;
using namespace swift::Demangle;

//===----------------------------------------------------------------------===//
//                          YAML Dump Implementation
//===----------------------------------------------------------------------===//

namespace {

using llvm::yaml::IO;
using llvm::yaml::MappingTraits;
using llvm::yaml::Output;
using llvm::yaml::ScalarEnumerationTraits;
using llvm::yaml::SequenceTraits;

struct YAMLNode {
  Node::Kind kind;
  std::vector<YAMLNode *> children;
  StringRef name = StringRef();

  YAMLNode(Node::Kind kind) : kind(kind), children() {}
};

} // end anonymous namespace

namespace llvm {
namespace yaml {

template <> struct ScalarEnumerationTraits<swift::Demangle::Node::Kind> {
  static void enumeration(IO &io, swift::Demangle::Node::Kind &value) {
#define NODE(ID) io.enumCase(value, #ID, swift::Demangle::Node::Kind::ID);
#include "swift/Demangling/DemangleNodes.def"
  }
};

template <> struct MappingTraits<YAMLNode *> {
  static void mapping(IO &io, YAMLNode *&node) {
    io.mapOptional("name", node->name);
    io.mapRequired("kind", node->kind);
    io.mapRequired("children", node->children);
  }
};

template <> struct MappingTraits<YAMLNode> {
  static void mapping(IO &io, YAMLNode &node) {
    io.mapOptional("name", node.name);
    io.mapRequired("kind", node.kind);
    io.mapRequired("children", node.children);
  }
};

} // namespace yaml
} // namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(YAMLNode *)

static std::string getNodeTreeAsYAML(llvm::StringRef name,
				     NodePointer root) {
  std::vector<std::unique_ptr<YAMLNode>> nodes;

  std::vector<std::pair<NodePointer, YAMLNode *>> worklist;
  nodes.emplace_back(new YAMLNode(root->getKind()));
  nodes.back()->name = name;
  worklist.emplace_back(root, &*nodes.back());

  while (!worklist.empty()) {
    NodePointer np;
    YAMLNode *node;
    std::tie(np, node) = worklist.back();
    worklist.pop_back();

    for (unsigned i = 0; i < np->getNumChildren(); ++i) {
      nodes.emplace_back(new YAMLNode(np->getChild(i)->getKind()));
      node->children.emplace_back(&*nodes.back());
    }
  }

  std::string output;
  llvm::raw_string_ostream stream(output);
  llvm::yaml::Output yout(stream);
  yout << *nodes.front();
  return stream.str();
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

static llvm::cl::opt<bool>
    DisableSugar("no-sugar",
                 llvm::cl::desc("No sugar mode (disable common language idioms "
                                "such as ? and [] from the output)"));

static llvm::cl::opt<bool> Simplified(
    "simplified",
    llvm::cl::desc("Don't display module names or implicit self types"));

static llvm::cl::list<std::string>
    InputNames(llvm::cl::Positional, llvm::cl::desc("[mangled name...]"),
               llvm::cl::ZeroOrMore);

static llvm::StringRef substrBefore(llvm::StringRef whole,
                                    llvm::StringRef part) {
  return whole.slice(0, part.data() - whole.data());
}

static llvm::StringRef substrAfter(llvm::StringRef whole,
                                   llvm::StringRef part) {
  return whole.substr((part.data() - whole.data()) + part.size());
}

static void demangle(llvm::raw_ostream &os, llvm::StringRef name,
                     swift::Demangle::Context &DCtx,
                     const swift::Demangle::DemangleOptions &options) {
  bool hadLeadingUnderscore = false;
  if (name.startswith("__")) {
    hadLeadingUnderscore = true;
    name = name.substr(1);
  }
  swift::Demangle::NodePointer pointer = DCtx.demangleSymbolAsNode(name);
  // We do not emit a message so that we end up dumping
  llvm::outs() << getNodeTreeAsYAML(name, pointer);
  DCtx.clear();
}

static int demangleSTDIN(const swift::Demangle::DemangleOptions &options) {
  // This doesn't handle Unicode symbols, but maybe that's okay.
  // Also accept the future mangling prefix.
  llvm::Regex maybeSymbol("(_T|_?\\$[Ss])[_a-zA-Z0-9$.]+");

  swift::Demangle::Context DCtx;
  for (std::string mangled; std::getline(std::cin, mangled);) {
    llvm::StringRef inputContents(mangled);

    llvm::SmallVector<llvm::StringRef, 1> matches;
    while (maybeSymbol.match(inputContents, &matches)) {
      llvm::outs() << substrBefore(inputContents, matches.front());
      demangle(llvm::outs(), matches.front(), DCtx, options);
      inputContents = substrAfter(inputContents, matches.front());
    }

    llvm::errs() << "Failed to match: " << inputContents << '\n';
  }

  return EXIT_SUCCESS;
}

int main(int argc, char **argv) {
#if defined(__CYGWIN__)
  // Cygwin clang 3.5.2 with '-O3' generates CRASHING BINARY,
  // if main()'s first function call is passing argv[0].
  std::rand();
#endif
  llvm::cl::ParseCommandLineOptions(argc, argv);

  swift::Demangle::DemangleOptions options;
  options.SynthesizeSugarOnTypes = !DisableSugar;
  if (Simplified)
    options = swift::Demangle::DemangleOptions::SimplifiedUIDemangleOptions();

  if (InputNames.empty()) {
    return demangleSTDIN(options);
  } else {
    swift::Demangle::Context DCtx;
    for (llvm::StringRef name : InputNames) {
      if (name.startswith("S")) {
        std::string correctedName = std::string("$") + name.str();
        demangle(llvm::outs(), correctedName, DCtx, options);
      } else {
        demangle(llvm::outs(), name, DCtx, options);
      }
      llvm::outs() << '\n';
    }

    return EXIT_SUCCESS;
  }
}
