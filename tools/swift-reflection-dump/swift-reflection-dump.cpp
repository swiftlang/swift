//===--- swift-reflection-dump.cpp - Reflection testing application -------===//
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
// This is a host-side tool to dump remote reflection sections in swift
// binaries.
//===----------------------------------------------------------------------===//

#include "swift/ABI/MetadataValues.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Reflection/ReflectionContext.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/MachOUniversal.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Support/CommandLine.h"

#if defined(_WIN32)
#include <io.h>
#else
#include <unistd.h>
#endif

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
                    "Dump the field layout for typeref strings read from stdin")),
       llvm::cl::init(ActionType::DumpReflectionSections));

static llvm::cl::list<std::string>
BinaryFilename("binary-filename", llvm::cl::desc("Filenames of the binary files"),
               llvm::cl::OneOrMore);

static llvm::cl::opt<std::string>
Architecture("arch", llvm::cl::desc("Architecture to inspect in the binary"),
             llvm::cl::Required);
} // end namespace options

template<typename T>
static T unwrap(llvm::Expected<T> value) {
  if (value)
    return std::move(value.get());
  std::cerr << "swift-reflection-test error: " << toString(value.takeError()) << "\n";
  exit(EXIT_FAILURE);
}

using NativeReflectionContext
  = ReflectionContext<External<RuntimeTarget<sizeof(uintptr_t)>>>;

class ObjectMemoryReader : public MemoryReader {
  const std::vector<const ObjectFile *> &ObjectFiles;
public:
  ObjectMemoryReader(const std::vector<const ObjectFile *> &ObjectFiles)
    : ObjectFiles(ObjectFiles)
  {
  }

  bool queryDataLayout(DataLayoutQueryType type, void *inBuffer,
                       void *outBuffer) override {
    switch (type) {
      case DLQ_GetPointerSize: {
        auto result = static_cast<uint8_t *>(outBuffer);
        *result = sizeof(void *);
        return true;
      }
      case DLQ_GetSizeSize: {
        auto result = static_cast<uint8_t *>(outBuffer);
        *result = sizeof(size_t);
        return true;
      }
    }

    return false;
  }

  RemoteAddress getSymbolAddress(const std::string &name) override {
    for (auto &object : ObjectFiles) {
      for (auto &symbol : object->symbols()) {
        if (unwrap(symbol.getName()).equals(name)) {
          // TODO: Account for offset in ELF binaries
          return RemoteAddress(unwrap(symbol.getAddress()));
        }
      }
    }
    return RemoteAddress(nullptr);
  }
  
  bool isAddressValid(RemoteAddress addr, uint64_t size) const {
    // TODO: Account for offset in ELF binaries

    auto src = addr.getAddressData();
    
    // Check that the source is in bounds of one of the object files.
    for (auto &object : ObjectFiles) {
      if ((uint64_t)object->getData().bytes_begin() <= src
          && src + size <= (uint64_t)object->getData().bytes_end()) {
        return true;
      }
    }
    return false;
  }
  
  ReadBytesResult readBytes(RemoteAddress address, uint64_t size) override {
    if (!isAddressValid(address, size))
      return ReadBytesResult(nullptr, [](const void *){});

    // TODO: Account for offset in ELF binaries
    return ReadBytesResult((const void *)address.getAddressData(), [](const void *) {});
  }
  
  bool readString(RemoteAddress address, std::string &dest) override {
    if (!isAddressValid(address, 1))
      return false;
    // TODO: Account for running off the edge of an object, offset in ELF
    // binaries
    auto cString = StringRef((const char*)address.getAddressData());
    dest.append(cString.begin(), cString.end());
    return true;
  }
};

static int doDumpReflectionSections(ArrayRef<std::string> binaryFilenames,
                                    StringRef arch,
                                    ActionType action,
                                    std::ostream &OS) {
  // Note: binaryOrError and objectOrError own the memory for our ObjectFile;
  // once they go out of scope, we can no longer do anything.
  std::vector<OwningBinary<Binary>> binaryOwners;
  std::vector<std::unique_ptr<ObjectFile>> objectOwners;
  std::vector<const ObjectFile *> objectFiles;

  // Construct the ReflectionContext.
  // FIXME: Should pick a Runtime template based on the bitwidth of the target
  // architecture.
  auto reader = std::make_shared<ObjectMemoryReader>(objectFiles);
  NativeReflectionContext context(std::move(reader));

  for (auto binaryFilename : binaryFilenames) {
    auto binaryOwner = unwrap(createBinary(binaryFilename));
    Binary *binaryFile = binaryOwner.getBinary();

    // The object file we are doing lookups in -- either the binary itself, or
    // a particular slice of a universal binary.
    std::unique_ptr<ObjectFile> objectOwner;
    const ObjectFile *objectFile;

    if (auto o = dyn_cast<ObjectFile>(binaryFile)) {
      objectFile = o;
    } else {
      auto universal = cast<MachOUniversalBinary>(binaryFile);
      objectOwner = unwrap(universal->getObjectForArch(arch));
      objectFile = objectOwner.get();
    }

    // Retain the objects that own section memory
    binaryOwners.push_back(std::move(binaryOwner));
    objectOwners.push_back(std::move(objectOwner));
    objectFiles.push_back(objectFile);

    auto startAddress = (uintptr_t)objectFile->getData().begin();
    context.addImage(RemoteAddress(startAddress));
  }

  switch (action) {
  case ActionType::DumpReflectionSections:
    // Dump everything
    context.getBuilder().dumpAllSections(OS);
    break;
  case ActionType::DumpTypeLowering: {
    for (std::string line; std::getline(std::cin, line); ) {
      if (line.empty())
        continue;

      if (StringRef(line).startswith("//"))
        continue;

      Demangle::Demangler Dem;
      auto demangled = Dem.demangleType(line);
      auto *typeRef = swift::Demangle::decodeMangledType(context.getBuilder(),
                                                         demangled);
      if (typeRef == nullptr) {
        OS << "Invalid typeref: " << line << "\n";
        continue;
      }

      typeRef->dump(OS);
      auto *typeInfo =
        context.getBuilder().getTypeConverter().getTypeInfo(typeRef);
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
  PROGRAM_START(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Reflection Dump\n");
  return doDumpReflectionSections(options::BinaryFilename,
                                  options::Architecture,
                                  options::Action,
                                  std::cout);
}

