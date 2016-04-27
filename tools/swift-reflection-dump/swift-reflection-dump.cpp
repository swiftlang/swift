//===--- swift-reflection-dump.cpp - Reflection testing application -------===//
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
// This is a host-side tool to dump remote reflection sections in swift
// binaries.
//===----------------------------------------------------------------------===//

#include "swift/ABI/MetadataValues.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/MachOUniversal.h"
#include "llvm/Object/ELF.h"
#include "llvm/Support/CommandLine.h"

#include <unistd.h>

#include <algorithm>
#include <iostream>
#include <csignal>

using llvm::dyn_cast;
using llvm::StringRef;
using llvm::ArrayRef;
using namespace llvm::object;

using namespace swift;
using namespace swift::reflection;
using namespace swift::remote;
using namespace Demangle;

enum class ActionType {
  DumpReflectionSections,
  DumpTypeLowering
};

namespace options {
static llvm::cl::opt<ActionType>
Action(llvm::cl::desc("Mode:"),
       llvm::cl::values(
         clEnumValN(ActionType::DumpReflectionSections,
                    "dump-reflection-sections",
                    "Dump the field reflection section"),
         clEnumValN(ActionType::DumpTypeLowering,
                    "dump-type-lowering",
                    "Dump the field layout for typeref strings read from stdin"),
         clEnumValEnd),
       llvm::cl::init(ActionType::DumpReflectionSections));

static llvm::cl::list<std::string>
BinaryFilename("binary-filename", llvm::cl::desc("Filenames of the binary files"),
               llvm::cl::OneOrMore);

static llvm::cl::opt<std::string>
Architecture("arch", llvm::cl::desc("Architecture to inspect in the binary"),
             llvm::cl::Required);
} // end namespace options

template<typename T>
static T unwrap(llvm::ErrorOr<T> value) {
  if (!value.getError())
    return std::move(value.get());
  std::cerr << "swift-reflection-test error: " << value.getError().message() << "\n";
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
  return SectionRef();
}

static int doDumpReflectionSections(ArrayRef<std::string> binaryFilenames,
                                    StringRef arch,
                                    ActionType action,
                                    std::ostream &OS) {
  // Note: binaryOrError and objectOrError own the memory for our ObjectFile;
  // once they go out of scope, we can no longer do anything.
  std::vector<OwningBinary<Binary>> binaryOwners;
  std::vector<std::unique_ptr<llvm::object::ObjectFile>> objectOwners;

  // Construct the TypeRefBuilder
  TypeRefBuilder builder;

  for (auto binaryFilename : binaryFilenames) {
    auto binaryOwner = unwrap(llvm::object::createBinary(binaryFilename));
    llvm::object::Binary *binaryFile = binaryOwner.getBinary();

    // The object file we are doing lookups in -- either the binary itself, or
    // a particular slice of a universal binary.
    std::unique_ptr<llvm::object::ObjectFile> objectOwner;
    const ObjectFile *objectFile;

    if (auto o = dyn_cast<ObjectFile>(binaryFile)) {
      objectFile = o;
    } else {
      auto universal = cast<MachOUniversalBinary>(binaryFile);
      objectOwner = unwrap(universal->getObjectForArch(arch));
      objectFile = objectOwner.get();
    }

    // Field descriptor section
    auto fieldSectionRef = getSectionRef(objectFile, {
      "__swift3_fieldmd", ".swift3_fieldmd"
    });

    if (fieldSectionRef.getObject() == nullptr) {
      OS << binaryFilename;
      OS << " doesn't have a field reflection section!\n";
      return EXIT_FAILURE;
    }

    StringRef fieldSectionContents;
    fieldSectionRef.getContents(fieldSectionContents);

    const FieldSection fieldSection {
      reinterpret_cast<const void *>(fieldSectionContents.begin()),
      reinterpret_cast<const void *>(fieldSectionContents.end())
    };

    // Associated type section - optional
    AssociatedTypeSection associatedTypeSection {nullptr, nullptr};

    auto associatedTypeSectionRef = getSectionRef(objectFile, {
      "__swift3_assocty", ".swift3_assocty"
    });

    if (associatedTypeSectionRef.getObject() != nullptr) {
      StringRef associatedTypeSectionContents;
      associatedTypeSectionRef.getContents(associatedTypeSectionContents);
      associatedTypeSection = {
        reinterpret_cast<const void *>(associatedTypeSectionContents.begin()),
        reinterpret_cast<const void *>(associatedTypeSectionContents.end()),
      };
    }

    // Builtin types section
    BuiltinTypeSection builtinTypeSection {nullptr, nullptr};

    auto builtinTypeSectionRef = getSectionRef(objectFile, {
      "__swift3_builtin", ".swift3_builtin"
    });

    if (builtinTypeSectionRef.getObject() != nullptr) {
      StringRef builtinTypeSectionContents;
      builtinTypeSectionRef.getContents(builtinTypeSectionContents);

      builtinTypeSection = {
        reinterpret_cast<const void *>(builtinTypeSectionContents.begin()),
        reinterpret_cast<const void *>(builtinTypeSectionContents.end())
      };
    }

    // Typeref section
    auto typeRefSectionRef = getSectionRef(objectFile, {
      "__swift3_typeref", ".swift3_typeref"
    });

    if (typeRefSectionRef.getObject() == nullptr) {
      OS << binaryFilename;
      OS << " doesn't have an associated typeref section!\n";
      return EXIT_FAILURE;
    }

    StringRef typeRefSectionContents;
    typeRefSectionRef.getContents(typeRefSectionContents);

    const GenericSection typeRefSection {
      reinterpret_cast<const void *>(typeRefSectionContents.begin()),
      reinterpret_cast<const void *>(typeRefSectionContents.end())
    };

    // Reflection strings section - optional
    GenericSection reflectionStringsSection {nullptr, nullptr};

    auto reflectionStringsSectionRef = getSectionRef(objectFile, {
      "__swift3_reflstr", ".swift3_reflstr"
    });

    if (reflectionStringsSectionRef.getObject() != nullptr) {
      StringRef reflectionStringsSectionContents;
      reflectionStringsSectionRef.getContents(reflectionStringsSectionContents);

      reflectionStringsSection = {
        reinterpret_cast<const void *>(reflectionStringsSectionContents.begin()),
        reinterpret_cast<const void *>(reflectionStringsSectionContents.end())
      };
    }

    builder.addReflectionInfo({
      binaryFilename,
      fieldSection,
      associatedTypeSection,
      builtinTypeSection,
      typeRefSection,
      reflectionStringsSection,
    });

    // Retain the objects that own section memory
    binaryOwners.push_back(std::move(binaryOwner));
    objectOwners.push_back(std::move(objectOwner));
  }

  switch (action) {
  case ActionType::DumpReflectionSections:
    // Dump everything
    builder.dumpAllSections(OS);
    break;
  case ActionType::DumpTypeLowering: {
    for (std::string line; std::getline(std::cin, line); ) {
      if (line.empty())
        continue;

      if (StringRef(line).startswith("//"))
        continue;

      auto demangled = Demangle::demangleTypeAsNode(line);
      auto *typeRef = swift::remote::decodeMangledType(builder, demangled);
      if (typeRef == nullptr) {
        OS << "Invalid typeref: " << line << "\n";
        continue;
      }

      typeRef->dump(OS);
      auto *typeInfo = builder.getTypeConverter().getTypeInfo(typeRef);
      if (typeInfo == nullptr) {
        OS << "Invalid lowering\n";
        continue;
      }
      typeInfo->dump(OS);
    }
    break;
  }
  }

  return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Reflection Dump\n");
  return doDumpReflectionSections(options::BinaryFilename,
                                  options::Architecture,
                                  options::Action,
                                  std::cout);
}

