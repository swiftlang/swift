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
template <typename Runtime>
struct Section {
  using StoredPointer = typename Runtime::StoredPointer;
  StoredPointer StartAddress;
  StoredPointer Size;
  StoredPointer getEndAddress() const {
    return StartAddress + Size;
  }
};

template <typename Runtime>
struct RemoteReflectionInfo {
  using StoredPointer = typename Runtime::StoredPointer;
  using StoredSize = typename Runtime::StoredSize;

  const std::string ImageName;
  const Section<Runtime> fieldmd;
  const Section<Runtime> assocty;
  const llvm::Optional<Section<Runtime>> reflstr;
  const Section<Runtime> typeref;
  const StoredPointer StartAddress;
  const StoredSize TotalSize;

  RemoteReflectionInfo(std::string ImageName,
                       Section<Runtime> fieldmd,
                       Section<Runtime> assocty,
                       llvm::Optional<Section<Runtime>> reflstr,
                       Section<Runtime> typeref)
    : ImageName(ImageName),
      fieldmd(fieldmd),
      assocty(assocty),
      reflstr(reflstr),
      typeref(typeref),
      StartAddress(std::min({
        fieldmd.StartAddress,
        typeref.StartAddress,
        reflstr.hasValue()
          ? reflstr.getValue().StartAddress
          : fieldmd.StartAddress,
        assocty.StartAddress})),
      TotalSize(std::max({
        fieldmd.getEndAddress(),
        assocty.getEndAddress(),
        reflstr.hasValue()
          ? reflstr.getValue().getEndAddress()
          : fieldmd.getEndAddress(),
        typeref.getEndAddress()
      }) - StartAddress) {}
};
}

template <typename Runtime>
class PipeMemoryReader : public MemoryReader {
  using StoredPointer = typename Runtime::StoredPointer;

  static constexpr size_t ReadEnd = 0;
  static constexpr size_t WriteEnd = 1;

  int to_child[2];
  int from_child[2];

public:

  int getParentReadFD() const {
    return from_child[ReadEnd];
  }

  int getChildWriteFD() const {
    return from_child[WriteEnd];
  }

  int getParentWriteFD() const {
    return to_child[WriteEnd];
  }

  int getChildReadFD() const {
    return to_child[ReadEnd];
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

  template <typename T>
  void collectBytesFromPipe(T *Value, size_t Size) {
    auto Dest = reinterpret_cast<uint8_t *>(&Value);
    while (Size) {
      auto bytesRead = read(getParentReadFD(), Value, Size);
      if (bytesRead <= 0)
        errorAndExit("collectBytesFromPipe");
      Size -= bytesRead;
      Dest += bytesRead;
    }
  }

  bool readBytes(addr_t Address, uint8_t *Dest, uint64_t Size) override {

    StoredPointer TargetAddress = (StoredPointer)Address;
    write(getParentWriteFD(), REQUEST_READ_BYTES, 2);
    write(getParentWriteFD(), &TargetAddress, sizeof(TargetAddress));
    write(getParentWriteFD(), &Size, sizeof(Size));
    collectBytesFromPipe(Dest, Size);
    return true;
  }

  uint64_t getParentWriteFD(StoredPointer Address) {
    write(getParentWriteFD(), REQUEST_STRING_LENGTH, 2);
    StoredPointer Length;
    write(getParentWriteFD(), &Address, sizeof(Address));
    collectBytesFromPipe(&Length, sizeof(Length));
    return static_cast<uint64_t>(Length);
  }

  addr_t getSymbolAddress(const std::string &SymbolName) override {
    StoredPointer Address = 0;
    write(getParentWriteFD(), REQUEST_SYMBOL_ADDRESS, 2);
    write(getParentWriteFD(), SymbolName.c_str(), SymbolName.size());
    write(getParentWriteFD(), "\n", 1);
    collectBytesFromPipe(&Address, sizeof(Address));
    return static_cast<StoredPointer>(Address);
  }

  StoredPointer receiveInstanceAddress() {
    write(getParentWriteFD(), REQUEST_INSTANCE_ADDRESS, 2);
    StoredPointer InstanceAddress = 0;
    collectBytesFromPipe(&InstanceAddress, sizeof(InstanceAddress));
    return InstanceAddress;
  }

  void sendExitMessage() {
    write(getParentWriteFD(), REQUEST_EXIT, 2);
  }

  uint8_t receivePointerSize() {
    write(getParentWriteFD(), REQUEST_POINTER_SIZE, 2);
    uint8_t PointerSize = 0;
    collectBytesFromPipe(&PointerSize, sizeof(PointerSize));
    return PointerSize;
  }

  std::vector<ReflectionInfo> receiveReflectionInfo() {
    write(getParentWriteFD(), REQUEST_REFLECTION_INFO, 2);
    uint64_t NumReflectionInfos = 0;
    collectBytesFromPipe(&NumReflectionInfos, sizeof(NumReflectionInfos));

    std::vector<RemoteReflectionInfo<Runtime>> RemoteInfos;
    for (uint64_t i = 0; i < NumReflectionInfos; ++i) {
      uint64_t ImageNameLength;
      collectBytesFromPipe(&ImageNameLength, sizeof(ImageNameLength));
      char c;
      std::string ImageName;
      for (uint64_t i = 0; i < ImageNameLength; ++i) {
        collectBytesFromPipe(&c, 1);
        ImageName.push_back(c);
      }

      StoredPointer fieldmd_start;
      StoredPointer fieldmd_size;
      StoredPointer typeref_start;
      StoredPointer typeref_size;
      StoredPointer reflstr_start;
      StoredPointer reflstr_size;
      StoredPointer assocty_start;
      StoredPointer assocty_size;

      collectBytesFromPipe(&fieldmd_start, sizeof(fieldmd_start));
      collectBytesFromPipe(&fieldmd_size, sizeof(fieldmd_size));
      collectBytesFromPipe(&typeref_start, sizeof(typeref_start));
      collectBytesFromPipe(&typeref_size, sizeof(typeref_size));
      collectBytesFromPipe(&reflstr_start, sizeof(reflstr_start));
      collectBytesFromPipe(&reflstr_size, sizeof(reflstr_size));
      collectBytesFromPipe(&assocty_start, sizeof(assocty_start));
      collectBytesFromPipe(&assocty_size, sizeof(assocty_size));

      RemoteInfos.push_back({
        ImageName,
        {fieldmd_start, fieldmd_size},
        {typeref_start, typeref_size},
        reflstr_size > 0
          ? llvm::Optional<Section<Runtime>>({reflstr_start, reflstr_size})
          : llvm::None,
        {assocty_start, assocty_size},
      });
    }

    std::vector<ReflectionInfo> Infos;
    for (auto &RemoteInfo : RemoteInfos) {

      auto buffer = (uint8_t *)malloc(RemoteInfo.TotalSize);

      if (!readBytes(RemoteInfo.StartAddress, buffer, RemoteInfo.TotalSize))
        errorAndExit("Couldn't read reflection information");

      auto fieldmd_base
        = buffer + RemoteInfo.fieldmd.StartAddress - RemoteInfo.StartAddress;
      auto typeref_base
        = buffer + RemoteInfo.typeref.StartAddress - RemoteInfo.StartAddress;
      auto reflstr_base
        = RemoteInfo.reflstr.hasValue()
          ? buffer + RemoteInfo.reflstr.getValue().StartAddress
             - RemoteInfo.StartAddress
          : 0;
      auto assocty_base
        = buffer + RemoteInfo.assocty.StartAddress - RemoteInfo.StartAddress;
      ReflectionInfo Info {
        RemoteInfo.ImageName,
        {fieldmd_base, fieldmd_base + RemoteInfo.fieldmd.Size},
        {typeref_base, typeref_base + RemoteInfo.typeref.Size},
        {reflstr_base, reflstr_base + (RemoteInfo.reflstr.hasValue()
          ? RemoteInfo.reflstr.getValue().Size
          : 0)},
        {assocty_base, assocty_base + RemoteInfo.assocty.Size},
      };
      Infos.push_back(Info);
    }
    return Infos;
  }

  uint64_t getStringLength(StoredPointer Address) {
    write(getParentWriteFD(), REQUEST_STRING_LENGTH, 2);
    StoredPointer Length;
    write(getParentWriteFD(), &Address, sizeof(Address));
    collectBytesFromPipe(&Length, sizeof(Length));
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

  PipeMemoryReader<Runtime> Pipe;

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

      StoredPointer instance = Pipe.receiveInstanceAddress();
      assert(instance);
      std::cerr << "Parent: instance pointer in child address space: 0x";
      std::cerr << std::hex << instance << std::endl;

      StoredPointer isa;
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
