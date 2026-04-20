//===--- MetadataReaderTest.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Remote/MetadataReader.h"
#include "swift/Remote/MemoryReader.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"
#include "gtest/gtest.h"

#include <cstring>
#include <map>
#include <vector>

using namespace swift;
using namespace swift::remote;
using namespace swift::Demangle;

namespace {

/// A MemoryReader that serves pre-configured memory regions and can return
/// symbols for specific addresses, enabling tests that exercise MetadataReader
/// code paths without requiring a real remote process.
class MockMemoryReader final : public MemoryReader {
public:
  /// Register a block of bytes at the given address.
  void addMemory(uint64_t address, const void *data, size_t size) {
    auto &vec = Memory[address];
    vec.resize(size);
    memcpy(vec.data(), data, size);
  }

  /// Register a symbol for resolvePointerAsSymbol at the given address.
  void addResolveSymbol(uint64_t address, std::string symbol) {
    ResolveSymbols[address] = std::move(symbol);
  }

private:
  std::map<uint64_t, std::vector<uint8_t>> Memory;
  std::map<uint64_t, std::string> ResolveSymbols;

  bool queryDataLayout(DataLayoutQueryType type, void *inBuffer,
                       void *outBuffer) override {
    switch (type) {
    case DLQ_GetPointerSize: {
      *static_cast<uint8_t *>(outBuffer) = 8;
      return true;
    }
    case DLQ_GetSizeSize: {
      *static_cast<uint8_t *>(outBuffer) = 8;
      return true;
    }
    case DLQ_GetPtrAuthMask: {
      *static_cast<uint64_t *>(outBuffer) = ~uint64_t(0);
      return true;
    }
    case DLQ_GetObjCReservedLowBits: {
      *static_cast<uint8_t *>(outBuffer) = 0;
      return true;
    }
    case DLQ_GetLeastValidPointerValue: {
      *static_cast<uint64_t *>(outBuffer) = 0x1000;
      return true;
    }
    case DLQ_GetObjCInteropIsEnabled:
      return false;
    }
    return false;
  }

  RemoteAddress getSymbolAddress(const std::string &name) override {
    return RemoteAddress();
  }

  bool readString(RemoteAddress address, std::string &dest) override {
    auto it = Memory.find(address.getRawAddress());
    if (it == Memory.end())
      return false;
    const char *data = reinterpret_cast<const char *>(it->second.data());
    size_t maxLen = it->second.size();
    size_t len = strnlen(data, maxLen);
    dest.assign(data, len);
    return true;
  }

  ReadBytesResult readBytes(RemoteAddress address, uint64_t size) override {
    auto it = Memory.find(address.getRawAddress());
    if (it == Memory.end())
      return ReadBytesResult();
    if (size > it->second.size())
      return ReadBytesResult();
    void *buf = malloc(size);
    memcpy(buf, it->second.data(), size);
    return ReadBytesResult(buf,
                           [](const void *p) { free(const_cast<void *>(p)); });
  }

  std::optional<RemoteAbsolutePointer>
  resolvePointerAsSymbol(RemoteAddress address) override {
    auto it = ResolveSymbols.find(address.getRawAddress());
    if (it != ResolveSymbols.end())
      return RemoteAbsolutePointer(it->second, 0, address);
    return std::nullopt;
  }
};

/// A minimal builder type that satisfies MetadataReader's template
/// requirements. None of these types are exercised by the code under test.
struct StubBuilder {
  using BuiltType = const void *;
  using BuiltTypeDecl = const void *;
  using BuiltProtocolDecl = const void *;
  using BuiltRequirement = std::monostate;
  using BuiltSubstitution = std::monostate;
  using BuiltSubstitutionMap = std::monostate;
  using BuiltGenericSignature = const void *;
};

using Runtime = External<NoObjCInterop<RuntimeTarget<8>>>;
using TestMetadataReader = MetadataReader<Runtime, StubBuilder>;

} // end anonymous namespace

// Test that DemangleInitRAII correctly saves and restores the Words[] array
// across nested demangle calls.
//
// When the demangler hits a symbolic reference during demangleType, the
// resolver calls buildContextManglingForSymbol → dem.demangleSymbol(symbol)
// on the same Demangler instance. The inner demangleSymbol processes
// identifiers that populate Words[] with StringRefs into its own Text
// buffer. DemangleInitRAII must save and restore Words[] so the outer
// demangling's word substitutions still reference the correct strings.
//
// Consider a Swift program with types whose mangled names use word
// substitutions and symbolic references:
//
//   // SomeModule
//   struct SomeType<T> { var field: T }
//
//   // OtherModule (compiled separately, referenced via symbolic ref)
//   struct OtherType { ... }
//
//   // A field of type SomeModule.SomeType<OtherModule.OtherType>
//   // The field's type reference in the reflection metadata is mangled as:
//   //   10SomeModule 8SomeType V y \x01<symref> 05OtherD0 V G
//   //
//   // The demangler splits identifiers into words at camelCase boundaries
//   // (see docs/ABI/Mangling.rst). "SomeModule" produces Words[0]="Some",
//   // Words[1]="Module". "SomeType" produces Words[2]="Some",
//   // Words[3]="Type". \x01<symref> is a symbolic reference to
//   // OtherModule's context descriptor. "05OtherD0" reconstructs
//   // "OtherType" using word substitution 'D' = Words[3] = "Type".
TEST(MetadataReader, WordsArraySavedAcrossNestedDemangle) {
  auto reader = std::make_shared<MockMemoryReader>();

  // Mangled type (see breakdown above):
  //   10SomeModule 8SomeType V y \x01<offset> 05OtherD0 V G \0
  const uint64_t mangledNameAddr = 0x5000;
  const uint64_t symbolicRefTarget = 0x5100;

  // The \x01 byte is at index 23 ("10SomeModule8SomeTypeVy" = 23 bytes).
  // The resolver computes: remoteAddress = (mangledNameAddr + 24) + offset.
  int32_t offset = symbolicRefTarget - (mangledNameAddr + 24);

  uint8_t mangledName[40];
  const char *prefix = "10SomeModule8SomeTypeVy";
  memcpy(&mangledName[0], prefix, 23);
  mangledName[23] = 0x01; // direct context symbolic reference
  memcpy(&mangledName[24], &offset, 4);
  const char *suffix = "05OtherD0VG";
  memcpy(&mangledName[28], suffix, 12); // includes NUL terminator

  reader->addMemory(mangledNameAddr, mangledName, sizeof(mangledName));

  // getSymbol(symbolicRefTarget) returns the nominal type descriptor for
  // OtherModule.OtherType. The resolver creates a ParentContextDescriptorRef
  // and calls dem.demangleSymbol() on its string, which is the nested
  // demangle call whose Words[] must not leak into the outer context.
  const char *otherModuleDescriptor = "$s11OtherModule9OtherTypeVMn";
  reader->addResolveSymbol(symbolicRefTarget, otherModuleDescriptor);

  TestMetadataReader metadataReader(reader);
  Demangler dem;

  // The word substitution 'D' in "05OtherD0" references Words[3].
  // If Words[] is correctly saved/restored across the nested demangle,
  // Words[3] = "Type" (from the outer "SomeType") and the identifier
  // resolves to "OtherType".
  auto result = metadataReader.demangle(
      RemoteRef<char>(RemoteAddress(mangledNameAddr, 0),
                      reinterpret_cast<const char *>(mangledName)),
      MangledNameKind::Type, dem);

  ASSERT_NE(result, nullptr);

  // Result: SomeModule.SomeType<OtherModule.OtherType.OtherType>
  // Tree: Type → BoundGenericStructure → [Type(Structure), TypeList]
  ASSERT_EQ(result->getKind(), Node::Kind::Type);
  auto boundGeneric = result->getChild(0);
  ASSERT_EQ(boundGeneric->getKind(), Node::Kind::BoundGenericStructure);

  // First child: the generic type SomeModule.SomeType
  auto someType = boundGeneric->getChild(0);
  ASSERT_EQ(someType->getKind(), Node::Kind::Type);
  auto structure = someType->getChild(0);
  ASSERT_EQ(structure->getKind(), Node::Kind::Structure);
  auto module = structure->getChild(0);
  ASSERT_EQ(module->getKind(), Node::Kind::Module);
  EXPECT_EQ(module->getText(), "SomeModule");
  auto ident = structure->getChild(1);
  ASSERT_EQ(ident->getKind(), Node::Kind::Identifier);
  EXPECT_EQ(ident->getText(), "SomeType");

  // Second child: the type argument list containing the generic argument.
  // The word substitution 'D' = Words[3] = "Type" must resolve correctly
  // for this identifier to be "OtherType".
  auto typeList = boundGeneric->getChild(1);
  ASSERT_EQ(typeList->getKind(), Node::Kind::TypeList);
  auto argType = typeList->getChild(0);
  ASSERT_EQ(argType->getKind(), Node::Kind::Type);
  auto argStruct = argType->getChild(0);
  ASSERT_EQ(argStruct->getKind(), Node::Kind::Structure);
  auto argIdent = argStruct->getChild(1);
  ASSERT_EQ(argIdent->getKind(), Node::Kind::Identifier);
  EXPECT_EQ(argIdent->getText(), "OtherType");
}
