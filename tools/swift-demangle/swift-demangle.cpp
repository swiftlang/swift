//===--- swift-demangle.cpp - Swift Demangler app -------------------------===//
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
// This is the entry point.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/DemangleWrappers.h"
#include "swift/Basic/ManglingMacros.h"
#include "llvm/ADT/SmallString.h"
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
RemangleMode("test-remangle",
           llvm::cl::desc("Remangle test mode (show the remangled string)"));

static llvm::cl::opt<bool>
RemangleNew("remangle-new",
           llvm::cl::desc("Remangle the symbol with new mangling scheme"));

static llvm::cl::opt<bool>
DisableSugar("no-sugar",
           llvm::cl::desc("No sugar mode (disable common language idioms such as ? and [] from the output)"));

static llvm::cl::opt<bool>
Simplified("simplified",
           llvm::cl::desc("Don't display module names or implicit self types"));

static llvm::cl::opt<bool>
Classify("classify",
           llvm::cl::desc("Display symbol classification characters"));

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
  if (ExpandMode || TreeOnly) {
    llvm::outs() << "Demangling for " << name << '\n';
    swift::demangle_wrappers::NodeDumper(pointer).print(llvm::outs());
  }
  if (RemangleMode) {
    std::string remangled;
    if (!pointer) {
      // Just reprint the original mangled name if it didn't demangle.
      // This makes it easier to share the same database between the
      // mangling and demangling tests.
      remangled = name;
    } else {
      // Also accept the future mangling prefix.
      // TODO: remove the "_S" as soon as MANGLING_PREFIX_STR gets "_S".
      remangled = swift::Demangle::mangleNode(pointer,
                          /*NewMangling*/ name.startswith(MANGLING_PREFIX_STR)
                                          || name.startswith("_S"));
      if (name.startswith("_S")) {
        assert(remangled.find(MANGLING_PREFIX_STR) == 0);
        remangled = "_S" + remangled.substr(3);
      }
      if (name != remangled) {
        llvm::errs() << "\nError: re-mangled name \n  " << remangled
                     << "\ndoes not match original name\n  " << name << '\n';
        exit(1);
      }
    }
    if (hadLeadingUnderscore) llvm::outs() << '_';
    llvm::outs() << remangled;
    return;
  }
  if (!TreeOnly) {
    if (RemangleNew) {
      if (!pointer) {
        llvm::errs() << "Can't de-mangle " << name << '\n';
        exit(1);
      }
      std::string remangled = swift::Demangle::mangleNode(pointer);
      llvm::outs() << remangled;
      return;
    }
    std::string string = swift::Demangle::nodeToString(pointer, options);
    if (!CompactMode)
      llvm::outs() << name << " ---> ";

    if (Classify) {
      std::string Classifications;
      std::string cName = name.str();
      if (!swift::Demangle::isSwiftSymbol(cName.c_str()))
        Classifications += 'N';
      if (DCtx.isThunkSymbol(name)) {
        if (!Classifications.empty())
          Classifications += ',';
        Classifications += "T:";
        Classifications += DCtx.getThunkTarget(name);
      } else {
        assert(DCtx.getThunkTarget(name).empty());
      }
      if (pointer && !DCtx.hasSwiftCallingConvention(name)) {
        if (!Classifications.empty())
          Classifications += ',';
        Classifications += 'C';
      }
      if (!Classifications.empty())
        llvm::outs() << '{' << Classifications << "} ";
    }
    llvm::outs() << (string.empty() ? name : llvm::StringRef(string));
  }
  DCtx.clear();
}

static int demangleSTDIN(const swift::Demangle::DemangleOptions &options) {
  // This doesn't handle Unicode symbols, but maybe that's okay.
  // Also accept the future mangling prefix.
  // TODO: remove the "_S" as soon as MANGLING_PREFIX_STR gets "_S".
  llvm::Regex maybeSymbol("(_T|_S|" MANGLING_PREFIX_STR ")[_a-zA-Z0-9$]+");

  swift::Demangle::Context DCtx;
  for (std::string mangled; std::getline(std::cin, mangled);) {
    llvm::StringRef inputContents(mangled);

    llvm::SmallVector<llvm::StringRef, 1> matches;
    while (maybeSymbol.match(inputContents, &matches)) {
      llvm::outs() << substrBefore(inputContents, matches.front());
      demangle(llvm::outs(), matches.front(), DCtx, options);
      inputContents = substrAfter(inputContents, matches.front());
    }

    llvm::outs() << inputContents << '\n';
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
    CompactMode = true;
    return demangleSTDIN(options);
  } else {
    swift::Demangle::Context DCtx;
    for (llvm::StringRef name : InputNames) {
      demangle(llvm::outs(), name, DCtx, options);
      llvm::outs() << '\n';
    }

    return EXIT_SUCCESS;
  }
}
