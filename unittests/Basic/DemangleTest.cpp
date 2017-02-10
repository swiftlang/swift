//===--- DemangleTest.cpp -------------------------------------------------===//
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
#include "gtest/gtest.h"

using namespace swift::demangle_wrappers;

TEST(Demangle, DemangleWrappers) {
  EXPECT_EQ("", demangleSymbolAsString(""));
  std::string MangledName = "_TtV1a1b\\\t\n\r\"\'\x1f\x20\x7e\x7f";
  MangledName += '\0';
  EXPECT_EQ("a.b with unmangled suffix \"\\\\\\t\\n\\r\\\"'\\x1F ~\\x7F\\0\"",
      demangleSymbolAsString(MangledName));
}

