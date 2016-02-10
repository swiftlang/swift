//===--------=- main.cpp - Tools for analyzing llvm profdata --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/StringSwitch.h"
#include "ProfdataCompare.hpp"

using namespace llvm;
using namespace covcompare;
using namespace std;

typedef function<int (int, const char **)> MainFunction;

/// \brief Top level help.
static int helpMain(int argc, const char *argv[]) {
  errs() << "Usage: cov-compare {compare|yaml} [OPTION]...\n\n"
         << "Compares code coverage information.\n\n"
         << "Subcommands:\n"
         << "  coverage: Compare two YAML files and generate a report.\n"
         << "  yaml: Convert a profdata file and binary into a YAML file.\n";
  return EXIT_FAILURE;
}

int main(int argc, const char **argv) {
  if (argc < 2) {
    return helpMain(argc, argv);
  }
  auto function = StringSwitch<MainFunction>(argv[1])
                    .Case("compare", compareMain)
                    .Case("yaml", yamlMain)
                    .Default(helpMain);
  
  string invocation = string(argv[0]) + " " + argv[1];
  argv[1] = invocation.c_str();
  return function(argc - 1, argv + 1);
}
