//===-- swift-demangle.cpp - Swift Demangler app ---------------------------===//
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
// This is the entry point.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Demangle.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"

#include <string>
#include <stdio.h>
#include <stdlib.h>

static llvm::cl::opt<bool>
ExpandMode("expand",
               llvm::cl::desc("Expand mode (show node structure of the demangling)"));

static llvm::cl::opt<bool>
CompactMode("compact",
          llvm::cl::desc("Compact mode (only emit the demangled names)"));

static llvm::cl::opt<bool>
TreeOnly("tree-only",
           llvm::cl::desc("Tree-only mode (do not show the demangled string)"));

static llvm::cl::opt<bool>
DisableSugar("no-sugar",
           llvm::cl::desc("No sugar mode (disable common language idioms such as ? and [] from the output)"));

static llvm::cl::list<std::string>
InputNames(llvm::cl::Positional, llvm::cl::desc("[mangled name...]"),
               llvm::cl::ZeroOrMore);

int main(int argc, char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram X(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv);
  swift::Demangle::DemangleOptions options;
  options.SynthesizeSugarOnTypes = !DisableSugar;
  for (llvm::StringRef name : InputNames) {
    if (name.startswith("__"))
      name = name.substr(1);
    swift::Demangle::NodePointer pointer = swift::Demangle::demangleSymbolAsNode(name);
    if (ExpandMode) {
      llvm::outs() << "Demangling for " << name << '\n';
      pointer->print(llvm::outs());
    }
    if (!TreeOnly) {
      std::string string = swift::Demangle::nodeToString(pointer, options);
      if (!CompactMode)
        llvm::outs() << name << " ---> ";
      llvm::outs() << (string.empty() ? name : llvm::StringRef(string)) << '\n';
    }
  }
  return 0;
}
