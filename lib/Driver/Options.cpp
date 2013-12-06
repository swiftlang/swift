//===--- Options.cpp - Option info & table --------------------------------===//
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

#include "swift/Driver/Options.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Option/Option.h"

using namespace swift::driver;
using namespace swift::driver::options;
using namespace llvm::opt;

#define PREFIX(NAME, VALUE) static const char *const NAME[] = VALUE;
#include "swift/Driver/Options.inc"
#undef PREFIX

static const OptTable::Info InfoTable[] = {
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM, \
HELPTEXT, METAVAR)   \
{ PREFIX, NAME, HELPTEXT, METAVAR, OPT_##ID, Option::KIND##Class, PARAM, \
FLAGS, OPT_##GROUP, OPT_##ALIAS, ALIASARGS },
#include "swift/Driver/Options.inc"
#undef OPTION
};

namespace {

class DriverOptTable : public OptTable {
public:
  DriverOptTable() : OptTable(InfoTable, llvm::array_lengthof(InfoTable)) {}
};

}

std::unique_ptr<OptTable> swift::driver::createDriverOptTable() {
  return std::unique_ptr<OptTable>(new DriverOptTable());
}
