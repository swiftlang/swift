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

#include "swift/ABI/MetadataValues.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Reflection/ReflectionContext.h"
#include "swift/Reflection/TypeRef.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/MachOUniversal.h"
#include "llvm/Object/ELF.h"
#include "llvm/Support/CommandLine.h"

#include "messages.h"

#include <unistd.h>

#include <algorithm>
#include <iostream>
#include <csignal>

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
  DumpReflectionSections,
  DumpHeapInstance
};

} // end anonymous namespace

namespace options {
static llvm::cl::opt<ActionType>
Action(llvm::cl::desc("Mode:"),
       llvm::cl::values(
         clEnumValN(ActionType::DumpReflectionSections,
                    "dump-reflection-sections",
                    "Dump the field reflection section"),
         clEnumValN(ActionType::DumpHeapInstance,
                    "dump-heap-instance",
                    "Dump the field layout for a heap instance by running "
                    "a Swift executable"),
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

static void errorAndExit(const std::string &message) {
  std::cerr << message << ": " << strerror(errno) << std::endl;
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

  auto FieldSectionRef = getSectionRef(binary, arch, {
    "__swift3_fieldmd", ".swift3_fieldmd"
  });

  if (FieldSectionRef.getObject() == nullptr) {
    std::cerr << BinaryFilename;
    std::cerr << " doesn't have a field reflection section!\n";
    return EXIT_FAILURE;
  }

  auto AssociatedTypeSectionRef = getSectionRef(binary, arch, {
    "__swift3_assocty", ".swift3_assocty"
  });

  if (AssociatedTypeSectionRef.getObject() == nullptr) {
    std::cerr << BinaryFilename;
    std::cerr << " doesn't have an associated type reflection section!\n";
    return EXIT_FAILURE;
  }

  auto ReflectionStringsSectionRef = getSectionRef(binary, arch, {
    "__swift3_reflstr", ".swift3_reflstr"
  });

  if (ReflectionStringsSectionRef.getObject() == nullptr) {
    std::cerr << BinaryFilename;
    std::cerr << " doesn't have an associated reflection strings section!\n";
    return EXIT_FAILURE;
  }

  auto TypeRefSectionRef = getSectionRef(binary, arch, {
    "__swift3_typeref", ".swift3_typeref"
  });

  if (TypeRefSectionRef.getObject() == nullptr) {
    std::cerr << BinaryFilename;
    std::cerr << " doesn't have an associated typeref section!\n";
    return EXIT_FAILURE;
  }

  StringRef FieldSectionContents;
  FieldSectionRef.getContents(FieldSectionContents);

  const FieldSection fieldSection {
    reinterpret_cast<const void *>(FieldSectionContents.begin()),
    reinterpret_cast<const void *>(FieldSectionContents.end())
  };

  StringRef AssociatedTypeSectionContents;
  AssociatedTypeSectionRef.getContents(AssociatedTypeSectionContents);

  const AssociatedTypeSection associatedTypeSection {
    reinterpret_cast<const void *>(AssociatedTypeSectionContents.begin()),
    reinterpret_cast<const void *>(AssociatedTypeSectionContents.end())
  };

  StringRef ReflectionStringsSectionContents;
  ReflectionStringsSectionRef.getContents(ReflectionStringsSectionContents);

  const GenericSection ReflectionStringsSection {
    reinterpret_cast<const void *>(ReflectionStringsSectionContents.begin()),
    reinterpret_cast<const void *>(ReflectionStringsSectionContents.end())
  };

  StringRef TypeRefSectionContents;
  AssociatedTypeSectionRef.getContents(TypeRefSectionContents);

  const GenericSection TypeRefSection {
    reinterpret_cast<const void *>(TypeRefSectionContents.begin()),
    reinterpret_cast<const void *>(TypeRefSectionContents.end())
  };

  InProcessMemoryReader Reader;
  ReflectionContext<External<RuntimeTarget<8>>> RC(Reader);
  RC.addReflectionInfo({
    BinaryFilename,
    fieldSection,
    associatedTypeSection,
    ReflectionStringsSection,
    TypeRefSection,
  });
  RC.dumpAllSections(std::cout);

  return EXIT_SUCCESS;
}

namespace {
struct Section {
  addr_t StartAddress;
  addr_t Size;

  addr_t getEndAddress() const {
    return StartAddress + Size;
  }
};

struct RemoteReflectionInfo {
  const std::string ImageName;
  const Section fieldmd;
  const Section assocty;
  const Section reflstr;
  const Section typeref;
  const addr_t StartAddress;
  const size_t TotalSize;

  RemoteReflectionInfo(std::string ImageName, Section fieldmd, Section assocty,
                       Section reflstr, Section typeref)
    : fieldmd(fieldmd), assocty(assocty), reflstr(reflstr), typeref(typeref),
      StartAddress(std::min({
        fieldmd.StartAddress, typeref.StartAddress,
        reflstr.StartAddress, assocty.StartAddress})),
      TotalSize(std::max({fieldmd.getEndAddress(), assocty.getEndAddress(),
                         reflstr.getEndAddress(), typeref.getEndAddress()}) - StartAddress) {}
};
}

template <typename StoredPointer>
class PipeMemoryReader : public MemoryReader {
  static constexpr size_t ReadEnd = 0;
  static constexpr size_t WriteEnd = 1;
  static constexpr size_t ParentEnd = 1;
  static constexpr size_t ChildEnd = 0;

  int to_child[2];
  int from_child[2];

public:

  int getParentWriteFD() const {
    return to_child[WriteEnd];
  }

  int getChildReadFD() const {
    return to_child[ReadEnd];
  }

  int getParentReadFD() const {
    return from_child[ReadEnd];
  }

  int getChildWriteFD() const {
    return from_child[WriteEnd];
  }

  PipeMemoryReader() {
    if (pipe(to_child))
      errorAndExit("Couldn't create pipes to child process");
    if (pipe(from_child))
      errorAndExit("Couldn't create pipes from child process");
  }

  uint8_t getPointerSize() override {
    // FIXME: Return based on -arch argument to the test tool
    return 8;
  }

  uint8_t getSizeSize() override {
    // FIXME: Return based on -arch argument to the test tool
    return 8;
  }

  bool readBytes(addr_t Address, uint8_t *Dest, uint64_t Size) override {
    write(getParentWriteFD(), REQUEST_READ_BYTES, 2);
    write(getParentWriteFD(), &Address, sizeof(Address));
    write(getParentWriteFD(), &Size, sizeof(Size));
    auto bytesRead = read(getParentReadFD(), Dest, Size);
    return bytesRead == (int64_t)Size;
  }

  uint64_t getParentWriteFD(addr_t Address) {
    write(getParentWriteFD(), REQUEST_STRING_LENGTH, 2);
    StoredPointer Length;
    write(getParentWriteFD(), &Address, sizeof(Address));
    read(getParentReadFD(), &Length, sizeof(Length));
    return static_cast<uint64_t>(Length);
  }

  addr_t getSymbolAddress(const std::string &SymbolName) override {
    StoredPointer Address = 0;
    write(getParentWriteFD(), REQUEST_SYMBOL_ADDRESS, 2);
    write(getParentWriteFD(), SymbolName.c_str(), SymbolName.size());
    write(getParentWriteFD(), "\n", 1);
    read(getParentReadFD(), &Address, sizeof(Address));
    return static_cast<addr_t>(Address);
  }

  addr_t receiveInstanceAddress() {
    write(getParentWriteFD(), REQUEST_INSTANCE_ADDRESS, 2);
    addr_t InstanceAddress = 0;
    read(getParentReadFD(), &InstanceAddress, sizeof(InstanceAddress));
    return InstanceAddress;
  }

  void sendExitMessage() {
    write(getParentWriteFD(), REQUEST_EXIT, 2);
  }

  uint8_t receivePointerSize() {
    write(getParentWriteFD(), REQUEST_POINTER_SIZE, 2);
    uint8_t PointerSize;
    read(getParentReadFD(), &PointerSize, sizeof(PointerSize));
    return PointerSize;
  }

  std::vector<ReflectionInfo> receiveReflectionInfo() {
    write(getParentWriteFD(), REQUEST_REFLECTION_INFO, 2);
    uint64_t NumReflectionInfos = 0;
    read(getParentReadFD(), &NumReflectionInfos, sizeof(NumReflectionInfos));

    std::vector<RemoteReflectionInfo> RemoteInfos;
    for (uint64_t i = 0; i < NumReflectionInfos; ++i) {
      uint64_t ImageNameLength;
      read(getParentReadFD(), &ImageNameLength, sizeof(ImageNameLength));
      char c;
      std::string ImageName;
      for (uint64_t i = 0; i < ImageNameLength; ++i) {
        read(getParentReadFD(), &c, 1);
        ImageName.push_back(c);
      }

      addr_t fieldmd_start;
      addr_t fieldmd_size;
      addr_t typeref_start;
      addr_t typeref_size;
      addr_t reflstr_start;
      addr_t reflstr_size;
      addr_t assocty_start;
      addr_t assocty_size;

      read(getParentReadFD(), &fieldmd_start, sizeof(fieldmd_start));
      read(getParentReadFD(), &fieldmd_size, sizeof(fieldmd_size));
      read(getParentReadFD(), &typeref_start, sizeof(typeref_start));
      read(getParentReadFD(), &typeref_size, sizeof(typeref_size));
      read(getParentReadFD(), &reflstr_start, sizeof(reflstr_start));
      read(getParentReadFD(), &reflstr_size, sizeof(reflstr_size));
      read(getParentReadFD(), &assocty_start, sizeof(assocty_start));
      read(getParentReadFD(), &assocty_size, sizeof(assocty_size));

      RemoteInfos.push_back({
        ImageName,
        {fieldmd_start, fieldmd_size},
        {typeref_start, typeref_size},
        {reflstr_start, reflstr_size},
        {assocty_start, assocty_size},
      });
    }

    std::vector<ReflectionInfo> Infos;
    for (auto &RemoteInfo : RemoteInfos) {

      auto buffer = (uint8_t *)malloc(RemoteInfo.TotalSize);

      readBytes(RemoteInfo.StartAddress, buffer, RemoteInfo.TotalSize);

      auto fieldmd_base = buffer + RemoteInfo.fieldmd.StartAddress - RemoteInfo.StartAddress;
      auto typeref_base = buffer + RemoteInfo.typeref.StartAddress - RemoteInfo.StartAddress;
      auto reflstr_base = buffer + RemoteInfo.reflstr.StartAddress - RemoteInfo.StartAddress;
      auto assocty_base = buffer + RemoteInfo.assocty.StartAddress - RemoteInfo.StartAddress;
      ReflectionInfo Info {
        RemoteInfo.ImageName,
        {fieldmd_base, fieldmd_base + RemoteInfo.fieldmd.Size},
        {typeref_base, typeref_base + RemoteInfo.typeref.Size},
        {reflstr_base, reflstr_base + RemoteInfo.reflstr.Size},
        {assocty_base, assocty_base + RemoteInfo.assocty.Size},
      };
      Infos.push_back(Info);
    }
    return Infos;
  }

  uint64_t getStringLength(addr_t Address) {
    write(getParentWriteFD(), REQUEST_STRING_LENGTH, 2);
    StoredPointer Length;
    write(getParentWriteFD(), &Address, sizeof(Address));
    read(getParentReadFD(), &Length, sizeof(Length));
    return static_cast<uint64_t>(Length);
  }

  std::string readString(addr_t Address) override {
    auto NameSize = getStringLength(Address);
    if (!NameSize)
      return "";

    auto NameBuffer = std::unique_ptr<uint8_t>(new uint8_t[NameSize + 1]);
    if (!readBytes(Address, NameBuffer.get(), NameSize + 1))
      return "";
    return std::string(reinterpret_cast<const char *>(NameBuffer.get()));
  }
};

template <typename Runtime>
static int doDumpHeapInstance(std::string BinaryFilename) {
  using StoredPointer = typename Runtime::StoredPointer;

  PipeMemoryReader<StoredPointer> Pipe;

  pid_t pid = fork();
  switch (pid) {
    case -1:
      errorAndExit("Couldn't fork child process");
      exit(EXIT_FAILURE);
    case 0: { // Child:
      close(Pipe.getParentWriteFD());
      close(Pipe.getParentReadFD());
      dup2(Pipe.getChildReadFD(), STDIN_FILENO);
      dup2(Pipe.getChildWriteFD(), STDOUT_FILENO);
      execv(BinaryFilename.c_str(), NULL);
      exit(EXIT_SUCCESS);
    }
    default: { // Parent
      close(Pipe.getChildReadFD());
      close(Pipe.getChildWriteFD());

      ReflectionContext<External<Runtime>> RC(Pipe);

      uint8_t PointerSize = Pipe.receivePointerSize();
      if (PointerSize != Runtime::PointerSize)
        errorAndExit("Child process had unexpected architecture");

      addr_t instance = Pipe.receiveInstanceAddress();
      assert(instance);
      std::cerr << "Parent: instance pointer in child address space: 0x";
      std::cerr << std::hex << instance << std::endl;

      addr_t isa;
      if (!Pipe.readInteger(instance, &isa))
        errorAndExit("Couldn't get heap object's metadata address");

      for (auto &Info : Pipe.receiveReflectionInfo())
        RC.addReflectionInfo(Info);

      std::cerr << "Parent: metadata pointer in child address space: 0x";
      std::cerr << std::hex << isa << std::endl;

      std::cerr << "Decoding type reference ..." << std::endl;
      auto TR = RC.getTypeRef(isa);
      TR->dump();

      auto Fields = RC.getFieldTypeRefs(isa);
      for (auto &Field : Fields) {
        std::cout << Field.first << ":\n";
        Field.second->dump();
        // TODO: Print field layout here.
        std::cout << std::endl;
      }
    }
  }

  Pipe.sendExitMessage();

  return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Reflection Test\n");
  switch (options::Action) {
  case ActionType::DumpReflectionSections:
    return doDumpReflectionSections(options::BinaryFilename,
                                    options::Architecture);
  case ActionType::DumpHeapInstance: {
    StringRef arch = options::Architecture;
    unsigned PointerSize = 0;
    if (arch == "x86_64")
      PointerSize = 8;
    else if (arch == "i386")
      PointerSize = 4;
    else if (arch == "arm64")
      PointerSize = 8;
    else if (arch == "arm" || arch == "armv7" || arch == "armv7s")
      PointerSize = 4;
    else if (arch == "armv7k")
      PointerSize = 4;
    else
      errorAndExit("Unsupported architecture");

    if (PointerSize == 4)
      return doDumpHeapInstance<External<RuntimeTarget<4>>>(options::BinaryFilename);
    else
      return doDumpHeapInstance<External<RuntimeTarget<8>>>(options::BinaryFilename);
  }
  case ActionType::None:
    llvm::cl::PrintHelpMessage();
    return EXIT_FAILURE;
    break;
  }
}
