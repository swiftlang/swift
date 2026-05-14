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

// Test that DemangleInitRAII correctly saves and restores the Words[] array
// across nested demangle calls.
//
// When demangleType hits a symbolic reference, the resolver may call
// demangleSymbol on the same Demangler. This is what MetadataReader does
// when resolving context descriptors via buildContextManglingForSymbol.
// The inner call processes identifiers that populate Words[] with StringRefs
// into its own Text buffer. DemangleInitRAII must save and restore Words[]
// so the outer demangling's word substitutions still reference the correct
// strings.
//
// The mangled type encodes something like:
//   SomeModule.SomeType<OtherModule.OtherType>
// where the generic argument's context comes from a symbolic reference.
// "SomeModule" and "SomeType" produce Words[0]="Some", [1]="Module",
// [2]="Some", [3]="Type". The suffix "05OtherD0" reconstructs "OtherType"
// using word substitution 'D' = Words[3] = "Type".
TEST(Demangle, WordsArraySavedAcrossNestedDemangle) {
  Demangler dem;

  // Mangled type: 10SomeModule 8SomeType V y \x01<offset> 05OtherD0 V G
  static const char mangledName[] =
      "10SomeModule"         // module identifier
      "8SomeType"            // type identifier
      "V"                    // struct
      "y"                    // begin generic args
      "\x01\x00\x00\x00\x00" // symbolic reference (0x01) + int32 offset (unused)
      "05OtherD0"            // identifier with word substitution 'D' = Words[3]
      "V"                    // struct
      "G";                   // end generic args

  auto resolver = [&](SymbolicReferenceKind kind, Directness directness,
                      int32_t offset,
                      const void *base) -> NodePointer {
    // Demangle a symbol on the same Demangler instance, triggering
    // DemangleInitRAII. The symbol string is a local std::string whose
    // buffer is freed when the lambda returns — if Words[] is not properly
    // saved/restored, the outer demangling reads stale pointers.
    std::string symbol("$s11OtherModule9OtherTypeVMn");
    auto node = dem.demangleSymbol(symbol);
    if (!node)
      return nullptr;
    // Unwrap Global → NominalTypeDescriptor → Type
    if (node->getKind() == Node::Kind::Global)
      node = node->getChild(0);
    if (node->getKind() == Node::Kind::NominalTypeDescriptor)
      node = node->getChild(0);
    return node;
  };

  auto mangledStr = makeSymbolicMangledNameStringRef(mangledName);
  auto result = dem.demangleType(mangledStr, resolver);

  ASSERT_NE(result, nullptr);

  // The word substitution 'D' in "05OtherD0" must resolve to Words[3]="Type"
  // (from the outer "SomeType"), producing the identifier "OtherType".
  ASSERT_EQ(result->getKind(), Node::Kind::Type);
  auto boundGeneric = result->getChild(0);
  ASSERT_EQ(boundGeneric->getKind(), Node::Kind::BoundGenericStructure);

  auto someType = boundGeneric->getChild(0);
  ASSERT_EQ(someType->getKind(), Node::Kind::Type);
  auto structure = someType->getChild(0);
  ASSERT_EQ(structure->getKind(), Node::Kind::Structure);
  EXPECT_EQ(structure->getChild(0)->getText(), "SomeModule");
  EXPECT_EQ(structure->getChild(1)->getText(), "SomeType");

  auto typeList = boundGeneric->getChild(1);
  ASSERT_EQ(typeList->getKind(), Node::Kind::TypeList);
  auto argType = typeList->getChild(0);
  ASSERT_EQ(argType->getKind(), Node::Kind::Type);
  auto argStruct = argType->getChild(0);
  ASSERT_EQ(argStruct->getKind(), Node::Kind::Structure);
  EXPECT_EQ(argStruct->getChild(1)->getText(), "OtherType");
}
