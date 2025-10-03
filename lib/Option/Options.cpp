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

#include "llvm/ADT/STLExtras.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Option/Option.h"

#define OPTTABLE_STR_TABLE_CODE
#include "swift/Option/Options.inc"
#undef OPTTABLE_STR_TABLE_CODE

#define OPTTABLE_PREFIXES_TABLE_CODE
#include "swift/Option/Options.inc"
#undef OPTTABLE_PREFIXES_TABLE_CODE

using namespace swift::options;
using namespace llvm::opt;

static const llvm::opt::GenericOptTable::Info InfoTable[] = {
#define OPTION(...) LLVM_CONSTRUCT_OPT_INFO(__VA_ARGS__),
#include "swift/Option/Options.inc"
#undef OPTION
};

namespace {

class SwiftOptTable : public llvm::opt::GenericOptTable {
public:
  SwiftOptTable()
      : GenericOptTable(OptionStrTable, OptionPrefixesTable, InfoTable) {}
};

} // end anonymous namespace

std::unique_ptr<OptTable> swift::createSwiftOptTable() {
  return std::unique_ptr<GenericOptTable>(new SwiftOptTable());
}
