//===--- swift-reflection-test.cpp - Reflection testing application -------===//
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

#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Reflection/ReflectionContext.h"
#include "swift/Reflection/TypeRef.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/MachOUniversal.h"
#include "llvm/Object/ELF.h"
#include "llvm/Support/CommandLine.h"

#include <iostream>

using llvm::dyn_cast;
using llvm::StringRef;
using namespace llvm::object;

using namespace swift;
using namespace reflection;
using namespace Demangle;

namespace {

enum class ActionType {
  None,
  DumpReflectionSection,
};

} // end anonymous namespace

namespace options {
static llvm::cl::opt<ActionType>
Action(llvm::cl::desc("Mode:"),
       llvm::cl::values(
         clEnumValN(ActionType::DumpReflectionSection,
                    "dump-reflection-section",
                    "Dump the reflection binary section after decoding "
                    "property and case records."),
         clEnumValEnd));

static llvm::cl::opt<std::string>
BinaryFilename("binary-filename", llvm::cl::desc("Filename of the binary file"),
               llvm::cl::Required);

static llvm::cl::opt<std::string>
Architecture("arch", llvm::cl::desc("Architecture to inspect in the binary"),
             llvm::cl::Required);
} // end namespace options


static void guardError(std::error_code error) {
  if (!error) return;
  std::cerr << "swift-reflection-test error: " << error.message() << "\n";
  exit(EXIT_FAILURE);
}

static llvm::object::SectionRef
getReflectionSectionRef(const ObjectFile *objectFile) {
  for (auto section : objectFile->sections()) {
    StringRef sectionName;
    section.getName(sectionName);
    if (sectionName.equals("__swift3_reflect") ||
        sectionName.equals(".swift3_reflect")) {
      return section;
    }
  }
  return llvm::object::SectionRef();
}

static llvm::object::SectionRef
getReflectionSectionRef(const Binary *binaryFile, StringRef arch) {
  if (auto objectFile = dyn_cast<ObjectFile>(binaryFile))
    return getReflectionSectionRef(objectFile);
  if (auto machoUniversal = dyn_cast<MachOUniversalBinary>(binaryFile)) {
    const auto objectOrError = machoUniversal->getObjectForArch(arch);
    guardError(objectOrError.getError());
    return getReflectionSectionRef(objectOrError.get().get());
  }
  return SectionRef();
}

struct FileSectionReader final : public ReflectionReader {
  const ReflectionSection Section;

  FileSectionReader(const ReflectionSection Section) : Section(Section) {}

  size_t read(void *dst, void *src, size_t count) const override {
    memmove(dst, src, count);
    return count;
  }

  std::vector<const ReflectionSection> getSections() const override {
    return { Section };
  }
};

static int doDumpReflectionSection(std::string BinaryFilename, StringRef arch) {
  auto binaryOrError = llvm::object::createBinary(BinaryFilename);
  guardError(binaryOrError.getError());

  const auto binary = binaryOrError.get().getBinary();

  auto reflectionSectionRef = getReflectionSectionRef(binary, arch);

  if (reflectionSectionRef.getObject() == nullptr) {
    std::cerr << BinaryFilename;
    std::cerr << " doesn't have a swift3_reflect section!\n";
    return EXIT_FAILURE;
  }

  StringRef sectionContents;
  reflectionSectionRef.getContents(sectionContents);

  const ReflectionSection section {
    BinaryFilename.c_str(),
    reinterpret_cast<const void *>(sectionContents.begin()),
    reinterpret_cast<const void *>(sectionContents.end())
  };

  FileSectionReader Reader(section);
  ReflectionContext RC(Reader);
  RC.dumpSections(std::cout);

  return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Reflection Test\n");
  switch (options::Action) {
  case ActionType::DumpReflectionSection:
    return doDumpReflectionSection(options::BinaryFilename, options::Architecture);
    break;
  case ActionType::None:
    llvm::cl::PrintHelpMessage();
    return EXIT_FAILURE;
    break;
  }
}
