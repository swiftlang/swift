//===--- Options.cpp - Option info & table --------------------------------===//
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

#include "swift/Option/Options.h"

#include "llvm/Option/OptTable.h"
#include "llvm/Option/Option.h"

using namespace swift::options;
using namespace llvm::opt;

#define OPTTABLE_CODE
#include "swift/Option/Options.inc"

namespace {

class SwiftOptTable : public llvm::opt::OptTable {
public:
  SwiftOptTable() : OptTable(optionTables()) {}
};

} // end anonymous namespace

std::unique_ptr<OptTable> swift::createSwiftOptTable() {
  return std::unique_ptr<OptTable>(new SwiftOptTable());
}
