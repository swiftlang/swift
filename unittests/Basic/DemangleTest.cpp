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

#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"
#include "gtest/gtest.h"

using namespace swift::Demangle;

TEST(Demangle, DemangleWrappers) {
  EXPECT_EQ("", demangleSymbolAsString(llvm::StringRef("")));
  std::string MangledName = "_TtV1a1b\\\t\n\r\"\'\x1f\x20\x7e\x7f";
  MangledName += '\0';
  EXPECT_EQ("a.b with unmangled suffix \"\\\\\\t\\n\\r\\\"'\\x1F ~\\x7F\\0\"",
      demangleSymbolAsString(MangledName));
}

TEST(Demangle, IsObjCSymbol) {
  EXPECT_EQ("type metadata accessor for __C.NSNumber",
            demangleSymbolAsString(llvm::StringRef("_$sSo8NSNumberCMa")));
  EXPECT_EQ(true, isObjCSymbol(llvm::StringRef("_$sSo8NSNumberCMa")));
  EXPECT_EQ(false,
            isObjCSymbol(llvm::StringRef("_$s3pat7inlinedSo8NSNumberCvp")));
  EXPECT_EQ(true, isObjCSymbol(llvm::StringRef("_$sSC3fooyS2d_SdtFTO")));
}

TEST(Demangle, CustomGenericParameterNames) {
  std::string SymbolName = "_$s1a1gyq_q__xt_tr0_lF";
  std::string DemangledName = "a.g<Q, U>((U, Q)) -> U";

  DemangleOptions Options;
  Options.GenericParameterName = [](uint64_t depth, uint64_t index) {
    return index ? "U" : "Q";
  };
  std::string Result = demangleSymbolAsString(SymbolName, Options);
  EXPECT_STREQ(DemangledName.c_str(), Result.c_str());
}

TEST(Demangle, DeepEquals) {
  static std::string Symbols[]{
#define SYMBOL(Mangled, Demangled) Mangled,
#include "ManglingTestData.def"
  };
  for (const auto &Symbol : Symbols) {
    Demangler D1;
    Demangler D2;
    auto tree1 = D1.demangleSymbol(Symbol);
    auto tree2 = D2.demangleSymbol(Symbol);
    EXPECT_TRUE(tree1->isDeepEqualTo(tree2)) << "Failing symbol: " << Symbol;
  }
}
