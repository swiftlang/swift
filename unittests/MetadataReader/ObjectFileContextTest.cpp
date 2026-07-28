//===--- ObjectFileContextTest.cpp ----------------------------------------===//
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

#include "swift/StaticMirror/ObjectFileContext.h"
#include "swift/Remote/RemoteAddress.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/ObjectYAML/yaml2obj.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::static_mirror;

namespace {

// Minimal ELF64 dynamic library that exposes the two relocations
// Image::scanELFType records:
//   * R_X86_64_RELATIVE — pure-address branch (no symbol name). The recorded
//     value is `HeaderAddress + r_addend`, so a non-zero addend lets us
//     verify the addend was actually folded into the recorded address.
//   * R_X86_64_64       — symbol-named branch (resolved via the LLVM resolver).
//
// Also exposing the `image_local` symbol to verify `getSymbolAddress`.
// `PT_LOAD` covers all sections so `reloc` offsets fall inside a segment
// that `decodeImageIndexAndAddress` can find.
constexpr llvm::StringLiteral kFixtureYaml = R"(--- !ELF
FileHeader:
  Class:   ELFCLASS64
  Data:    ELFDATA2LSB
  Type:    ET_DYN
  Machine: EM_X86_64
Sections:
  - Name:    .text
    Type:    SHT_PROGBITS
    Flags:   [ SHF_ALLOC, SHF_EXECINSTR ]
    Address: 0x1000
    Size:    0x10
  - Name:    .data
    Type:    SHT_PROGBITS
    Flags:   [ SHF_ALLOC, SHF_WRITE ]
    Address: 0x1010
    Size:    0x20
  - Name:    .rela.dyn
    Type:    SHT_RELA
    Flags:   [ SHF_ALLOC ]
    Address: 0x1030
    Link:    .dynsym
    Relocations:
      - Offset: 0x1010
        Type:   R_X86_64_RELATIVE
        Addend: 0x0004
      - Offset: 0x1018
        Type:   R_X86_64_64
        Symbol: external
  - Name:    .dynamic
    Type:    SHT_DYNAMIC
    Flags:   [ SHF_ALLOC ]
    Address: 0x1080
    Link:    .dynstr
    Entries:
      - Tag:   DT_RELA
        Value: 0x1030
      - Tag:   DT_RELASZ
        Value: 0x30
      - Tag:   DT_RELAENT
        Value: 0x18
      - Tag:   DT_NULL
        Value: 0x0
Symbols:
  - Name:    image_local
    Type:    STT_FUNC
    Section: .text
    Value:   0x1000
    Binding: STB_GLOBAL
DynamicSymbols:
  - Name:    external
ProgramHeaders:
  - Type:     PT_LOAD
    Flags:    [ PF_R ]
    VAddr:    0x1000
    FirstSec: .text
    LastSec:  .dynamic
)";

std::unique_ptr<llvm::object::ObjectFile>
buildFixture(llvm::SmallVectorImpl<char> &Storage) {
  return llvm::yaml::yaml2ObjectFile(
      Storage, kFixtureYaml,
      [](const llvm::Twine &Msg) { ADD_FAILURE() << Msg.str(); });
}

class ObjectFileContextTest : public ::testing::Test {
protected:
  llvm::SmallVector<char, 0> Storage0;
  llvm::SmallVector<char, 0> Storage1;
  std::unique_ptr<llvm::object::ObjectFile> Obj0;
  std::unique_ptr<llvm::object::ObjectFile> Obj1;

  void SetUp() override {
    Obj0 = buildFixture(Storage0);
    Obj1 = buildFixture(Storage1);
    ASSERT_TRUE(Obj0);
    ASSERT_TRUE(Obj1);
  }
};

// A two-image setup is required to make the slide non-zero — with a single
// image, ObjectMemoryReader leaves Slide at 0 and the asymmetry fix becomes a
// no-op that the test couldn't observe.
TEST_F(ObjectFileContextTest, SymmetricPointerReaders) {
  std::vector<const llvm::object::ObjectFile *> Objects{Obj0.get(), Obj1.get()};
  ObjectMemoryReader Reader(Objects);

  constexpr uint64_t HeaderAddress = 0x1000;
  constexpr uint64_t LocalSymbolValue = 0x1000;
  constexpr uint64_t RelativeRelocOffset = 0x1010;
  constexpr uint64_t RelativeRelocAddend = 0x0004;
  constexpr uint64_t SymbolNamedRelocOffset = 0x1018;

  auto Image1Start = Reader.getImageStartAddress(1);
  uint64_t Slide1 = Image1Start.getRawAddress() - HeaderAddress;
  ASSERT_NE(Slide1, 0u)
      << "Multi-image setup should produce a non-zero slide for image 1";

  /// Verify resolved symbol address includes image1 slide.
  {
    auto Resolved = Reader.getSymbolAddress(Image1Start, "image_local");
    EXPECT_EQ(Resolved.getRawAddress(), LocalSymbolValue + Slide1);
  }

  // Verify looking up a dynamic symbol by relocation address works and that it
  // includes the addend and image slide.
  {
    swift::remote::RemoteAddress RelocAddr(
        RelativeRelocOffset + Slide1,
        swift::remote::RemoteAddress::DefaultAddressSpace);
    auto Resolved = Reader.getDynamicSymbol(RelocAddr);
    ASSERT_TRUE(static_cast<bool>(Resolved));
    EXPECT_TRUE(Resolved.getSymbol().empty());
    EXPECT_EQ(Resolved.getResolvedAddress().getRawAddress(),
              HeaderAddress + RelativeRelocAddend + Slide1);
  }

  // Verify that looking up a symbol by name preserves the symbol name and
  // resolve address is empty.
  {
    swift::remote::RemoteAddress RelocAddr(
        SymbolNamedRelocOffset + Slide1,
        swift::remote::RemoteAddress::DefaultAddressSpace);
    auto Resolved = Reader.getDynamicSymbol(RelocAddr);
    ASSERT_TRUE(static_cast<bool>(Resolved));
    EXPECT_EQ(Resolved.getSymbol(), "external");
    EXPECT_FALSE(static_cast<bool>(Resolved.getResolvedAddress()));
  }
}

} // namespace
