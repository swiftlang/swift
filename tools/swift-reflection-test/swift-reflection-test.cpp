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
#include "llvm/Object/ELF.h"
#include "llvm/Support/CommandLine.h"

using llvm::dyn_cast;
using llvm::StringRef;

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
} // end namespace options

static int doDumpReflectionSection(StringRef BinaryFilename) {

  llvm::object::SectionRef reflectionSectionRef;

  auto binaryOrError = llvm::object::createBinary(BinaryFilename);
  if (auto errorCode = binaryOrError.getError()) {
    llvm::errs() << "swift-reflection-test: '" << BinaryFilename << '\'';
    llvm::errs() << ": " << errorCode.message() << "\n";
    return EXIT_FAILURE;
  }
  auto binary = binaryOrError.get().getBinary();

  auto object = cast<llvm::object::ObjectFile>(binary);
  for (auto section : object->sections()) {
    StringRef sectionName;
    section.getName(sectionName);
    if (sectionName.equals("__swift3_reflect") ||
        sectionName.equals(".swift3_reflect")) {
      reflectionSectionRef = section;
      break;
    }
  }

  if (reflectionSectionRef.getObject() == nullptr) {
    llvm::errs() << BinaryFilename;
    llvm::errs() << " doesn't have a swift3_reflect section!\n";
    return EXIT_FAILURE;
  }

  StringRef sectionContents;
  reflectionSectionRef.getContents(sectionContents);

  const ReflectionSection section {
    BinaryFilename,
    reinterpret_cast<const void *>(sectionContents.begin()),
    reinterpret_cast<const void *>(sectionContents.end())
  };

  ReflectionContext RC;

  for (const auto &descriptor : section) {
    auto typeName = demangleTypeAsString(descriptor.getMangledTypeName());
    auto mangledTypeName = descriptor.getMangledTypeName();
    auto demangleTree = demangleTypeAsNode(mangledTypeName);
    auto TR = decodeDemangleNode(RC, demangleTree);
    llvm::errs() << typeName << '\n';
    for (size_t i = 0; i < typeName.size(); ++i)
      llvm::errs() << '=';
    llvm::errs() << '\n';

    TR->dump();
    llvm::errs() << "\n";

    for (auto &field : descriptor.getFieldRecords()) {
      auto fieldDemangleTree = demangleTypeAsNode(field.getMangledTypeName());
      auto fieldTR = decodeDemangleNode(RC, fieldDemangleTree);
      auto fieldName = field.getFieldName();
      llvm::errs() << "- " << fieldName << ":\n";

      fieldTR->dump();
      llvm::errs() << "\n";
    }

    llvm::errs() << '\n';
  }

  return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Reflection Test\n");

  switch (options::Action) {
  case ActionType::DumpReflectionSection:
    return doDumpReflectionSection(options::BinaryFilename);
    break;
  case ActionType::None:
    llvm::cl::PrintHelpMessage();
    return EXIT_FAILURE;
    break;
  }
}
