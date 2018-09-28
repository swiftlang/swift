//===--- swift-config.cpp - Swift Config app ------------------------------===//
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
//
// This tool encapsulates information about an Swift project configuration for
// use by other project's build environments (to determine installed path,
// available features, required libraries, etc.).//
//
// Note that although this tool *may* be used by some parts of Swift's build
// itself, this tool is primarily designed to support external projects.
//
//===----------------------------------------------------------------------===//

#include "llvm/Config/llvm-config.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/StringRef.h"

// For std::rand, to work around a bug if main()'s first function call passes
// argv[0].
#if defined(__CYGWIN__)
#include <cstdlib>
#endif

#include <iostream>

using namespace llvm;

// Include the build time variables we can report to the user. This is generated
// at build time from the BuildVariables.inc.in file by the build system.
#include "BuildVariables.inc"

static void usage() {
  llvm::errs() << "\
usage: swift-config <OPTION>...\n\
\n\
Get various configuration information needed to compile programs which use\n\
Swift. Typically called from 'configure' scripts. Examples:\n\
  swift-config --cxxflags\n\
  swift-config --ldflags\n\
\n\
Options:\n\
  --version         Print Swift version.\n\
  --prefix          Print the installation prefix.\n\
  --src-root        Print the source root Swift was built from.\n\
  --obj-root        Print the object root used to build Swift.\n\
  --bindir          Directory containing Swift executables.\n\
  --includedir      Directory containing Swift headers.\n\
  --cmark-src-root  Print the source root CMark was built from.\n\
  --cmark-obj-root  Print the object root used to build CMark.\n\
  --libdir          Directory containing Swift libraries.\n\
  --cppflags        C preprocessor flags for files that include Swift headers.\n\
  --cflags          C compiler flags for files that include Swift headers.\n\
  --cxxflags        C++ compiler flags for files that include Swift headers.\n\
  --ldflags         Print Linker flags.\n\
  \n";
  exit(1);
}

/// Compute the path to the main executable.
std::string GetExecutablePath(const char *Argv0) {
  // This just needs to be some symbol in the binary; C++ doesn't
  // allow taking the address of ::main however.
  void *P = (void *)(intptr_t)GetExecutablePath;
  return llvm::sys::fs::getMainExecutable(Argv0, P);
}

int main(int argc, char **argv) {
#if defined(__CYGWIN__)
  // Cygwin clang 3.5.2 with '-O3' generates CRASHING BINARY,
  // if main()'s first function call is passing argv[0].
  std::rand();
#endif
  bool IsInDevelopmentTree;
  std::string ActiveObjRoot;
  llvm::SmallString<256> CurrentPath(GetExecutablePath(argv[0]));
  std::string CurrentExecPrefix;

  // Create an absolute path, and pop up one directory (we expect to be inside a
  // bin dir).
  sys::fs::make_absolute(CurrentPath);
  CurrentExecPrefix = sys::path::parent_path(sys::path::parent_path(CurrentPath)).str();

  StringRef DirSep = "/";
  const Triple HostTriple(Triple::normalize(LLVM_HOST_TRIPLE));
  if (HostTriple.isOSWindows()) {
    DirSep = "\\";
  }

  // Check to see if we are inside a development tree by comparing to possible
  // locations (prefix style or CMake style).
  // Compute various directory locations based on the derived location information.
  std::string ActivePrefix, ActiveBinDir, ActiveIncludeDir, ActiveLibDir, ActiveIncludeOption;
  if ((sys::fs::equivalent(CurrentExecPrefix, SWIFT_OBJ_DIR)) || (sys::fs::equivalent(CurrentExecPrefix, Twine(SWIFT_OBJ_DIR) + DirSep.str() + "bin"))) {
    IsInDevelopmentTree = true;
    ActiveObjRoot = SWIFT_OBJ_DIR;
    ActivePrefix = SWIFT_PREFIX_DIR;
    ActiveIncludeDir = SWIFT_INCLUDE_DIR;
    ActiveLibDir = SWIFT_LIBRARY_DIR;
    ActiveBinDir = SWIFT_BIN_DIR;
    // We need to include files from both the source and object trees.
    ActiveIncludeOption = ("-I" + ActiveIncludeDir + " " + "-I" + ActiveObjRoot + DirSep.str() + "include");
  } else {
    IsInDevelopmentTree = false;
    ActivePrefix = CurrentExecPrefix;
    ActiveIncludeDir = ActivePrefix + DirSep.str() + "include";
    SmallString<256> path(StringRef(CMAKE_INSTALL_PREFIX));
    sys::fs::make_absolute(ActivePrefix, path);
    ActiveBinDir = path.str();
    ActiveLibDir = ActivePrefix + DirSep.str() + "lib" + LLVM_LIBDIR_SUFFIX;
    ActiveIncludeOption = "-I" + ActiveIncludeDir;
  }

  bool HasAnyOption = false;

  raw_ostream &OS = outs();
  for (int i = 1; i != argc; ++i) {
    StringRef Arg = argv[i];

    if (Arg.startswith("-")) {
      HasAnyOption = true;
      if (Arg == "--help") {
        usage();
      } else if (Arg == "--version") {
        OS << SWIFT_VERSION << '\n';
      } else if (Arg == "--prefix") {
        OS << ActivePrefix << '\n';
      } else if (Arg == "--src-root") {
        OS << SWIFT_SOURCE_DIR << '\n';
      } else if (Arg == "--obj-root") {
        OS << ActivePrefix << '\n';
      } else if (Arg == "--bindir") {
        OS << ActiveBinDir << '\n';
      } else if (Arg == "--libdir") {
        OS << SWIFT_LIBRARY_DIR << '\n';
      } else if (Arg == "--includedir") {
        OS << ActiveIncludeDir << '\n';
      } else if (Arg == "--cmark-src-root") {
        OS << SWIFT_PATH_TO_CMARK_SOURCE << '\n';
      } else if (Arg == "--cmark-obj-root") {
        OS << SWIFT_PATH_TO_CMARK_BUILD << '\n';
      } else if (Arg == "--cppflags") {
        OS << SWIFT_CPPFLAGS << '\n';
      } else if (Arg == "--cflags") {
        OS << ActiveIncludeOption << " " << SWIFT_CFLAGS << '\n';
      } else if (Arg == "--cxxflags") {
        OS << ActiveIncludeOption << " " << SWIFT_CXX_FLAGS << '\n';
      } else if (Arg == "--ldflags") {
        OS << ((HostTriple.isWindowsMSVCEnvironment()) ? "-LIBPATH:" : "-L");
        OS << ActiveLibDir << " " << CMAKE_CXX_LINK_FLAGS << '\n';
      }
    }
  }

  if (!HasAnyOption) {
    usage();
  }

  return EXIT_SUCCESS;
}