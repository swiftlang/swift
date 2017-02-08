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

using namespace swift::options;
using namespace llvm::opt;

#define PREFIX(NAME, VALUE) static const char *const NAME[] = VALUE;
#include "swift/Option/Options.inc"
#undef PREFIX

static const OptTable::Info InfoTable[] = {
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM, \
HELPTEXT, METAVAR)   \
{ PREFIX, NAME, HELPTEXT, METAVAR, OPT_##ID, Option::KIND##Class, PARAM, \
FLAGS, OPT_##GROUP, OPT_##ALIAS, ALIASARGS },
#include "swift/Option/Options.inc"
#undef OPTION
};

namespace {

class SwiftOptTable : public OptTable {
public:
  SwiftOptTable() : OptTable(InfoTable) {}
};

} // end anonymous namespace

std::unique_ptr<OptTable> swift::createSwiftOptTable() {
  return std::unique_ptr<OptTable>(new SwiftOptTable());
}
