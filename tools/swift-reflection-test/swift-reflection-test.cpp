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
using llvm::ArrayRef;
using namespace llvm::object;

using namespace swift;
using namespace reflection;
using namespace Demangle;

namespace {

enum class ActionType {
  None,
  DumpReflectionSections
};

} // end anonymous namespace

namespace options {
static llvm::cl::opt<ActionType>
Action(llvm::cl::desc("Mode:"),
       llvm::cl::values(
         clEnumValN(ActionType::DumpReflectionSections,
                    "dump-reflection-sections",
                    "Dump the field reflection section"),
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
getSectionRef(const ObjectFile *objectFile,
              ArrayRef<StringRef> anySectionNames) {
  for (auto section : objectFile->sections()) {
    StringRef sectionName;
    section.getName(sectionName);
    for (auto desiredName : anySectionNames) {
      if (sectionName.equals(desiredName)) {
        return section;
      }
    }
  }
  return llvm::object::SectionRef();
}

static llvm::object::SectionRef
getSectionRef(const Binary *binaryFile, StringRef arch,
              ArrayRef<StringRef> anySectionNames) {
  if (auto objectFile = dyn_cast<ObjectFile>(binaryFile))
    return getSectionRef(objectFile, anySectionNames);
  if (auto machoUniversal = dyn_cast<MachOUniversalBinary>(binaryFile)) {
    const auto objectOrError = machoUniversal->getObjectForArch(arch);
    guardError(objectOrError.getError());
    return getSectionRef(objectOrError.get().get(), anySectionNames);
  }
  return SectionRef();
}

static int doDumpReflectionSections(std::string BinaryFilename,
                                    StringRef arch) {
  auto binaryOrError = llvm::object::createBinary(BinaryFilename);
  guardError(binaryOrError.getError());

  const auto binary = binaryOrError.get().getBinary();

  auto fieldSectionRef = getSectionRef(binary, arch, {
    "__swift3_fieldmd", ".swift3_fieldmd"
  });

  if (fieldSectionRef.getObject() == nullptr) {
    std::cerr << BinaryFilename;
    std::cerr << " doesn't have a field reflection section!\n";
    return EXIT_FAILURE;
  }

  auto associatedTypeSectionRef = getSectionRef(binary, arch, {
    "__swift3_assocty", ".swift3_assocty"
  });

  if (associatedTypeSectionRef.getObject() == nullptr) {
    std::cerr << BinaryFilename;
    std::cerr << " doesn't have an associated type reflection section!\n";
    return EXIT_FAILURE;
  }

  StringRef fieldSectionContents;
  fieldSectionRef.getContents(fieldSectionContents);

  const FieldSection fieldSection {
    BinaryFilename.c_str(),
    reinterpret_cast<const void *>(fieldSectionContents.begin()),
    reinterpret_cast<const void *>(fieldSectionContents.end())
  };

  StringRef associatedTypeSectionContents;
  associatedTypeSectionRef.getContents(associatedTypeSectionContents);

  const AssociatedTypeSection associatedTypeSection {
    BinaryFilename.c_str(),
    reinterpret_cast<const void *>(associatedTypeSectionContents.begin()),
    reinterpret_cast<const void *>(associatedTypeSectionContents.end())
  };

  MemoryReader Reader;
  Reader.addReflectionInfo({fieldSection, associatedTypeSection});
  ReflectionContext RC(Reader);
  RC.dumpAllSections(std::cout);

  return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Reflection Test\n");
  switch (options::Action) {
  case ActionType::DumpReflectionSections:
    return doDumpReflectionSections(options::BinaryFilename,
                                    options::Architecture);
  case ActionType::None:
    llvm::cl::PrintHelpMessage();
    return EXIT_FAILURE;
    break;
  }
}
