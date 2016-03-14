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

#if __APPLE__
#include "mach_messages.h"
#include "mach_helpers.h"
#endif

#include <unistd.h>

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

  MemoryReader Reader;
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

#if __APPLE__

static mach_port_t createReceivePort() {
  kern_return_t error;
  mach_port_t port = MACH_PORT_NULL;

  error = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &port);
  guardMachError(error, "mach_port_allocate");

  error = mach_port_insert_right(mach_task_self(), port, port,
                                 MACH_MSG_TYPE_MAKE_SEND);

  guardMachError(error, "mach_port_insert_right");
  return port;
}

static mach_port_t receivePort(mach_port_t fromPort) {
  kern_return_t error;

  ReceivePortMessage message;

  error = mach_msg (&message.header, MACH_RCV_MSG,
                    MACH_RCV_LARGE, sizeof(message), fromPort,
                    MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);

  guardMachError(error, "mach_msg MACH_RCV_MSG");
  return message.taskPort.name;
}

ReflectionInfo receiveReflectionInfo(MemoryReader &Reader,
                                     mach_port_t childPort,
                                     task_t childTask) {
  ReceiveReflectionInfoMessage message;
  kern_return_t error = mach_msg(&message.header,
                                 MACH_RCV_MSG | MACH_RCV_INTERRUPT,
                                 0,
                                 sizeof(message),
                                 childPort,
                                 MACH_MSG_TIMEOUT_NONE,
                                 MACH_PORT_NULL);

  guardMachError(error, "mach_msg (MACH_RCV_MSG)");
  RemoteReflectionInfo RemoteInfo;
  RemoteInfo.image_name_addr = extractUInt64(message.image_name_addr);

  RemoteInfo.fieldmd.addr = extractUInt64(message.fieldmd_start_addr);
  RemoteInfo.fieldmd.size = extractUInt64(message.fieldmd_size);
  RemoteInfo.typeref.addr = extractUInt64(message.typeref_start_addr);
  RemoteInfo.typeref.size = extractUInt64(message.typeref_size);
  RemoteInfo.reflstr.addr = extractUInt64(message.reflstr_start_addr);
  RemoteInfo.reflstr.size = extractUInt64(message.reflstr_size);
  RemoteInfo.assocty.addr = extractUInt64(message.assocty_start_addr);
  RemoteInfo.assocty.size = extractUInt64(message.assocty_size);

  uint64_t MinAddress = RemoteInfo.fieldmd.addr;
  MinAddress = std::min(MinAddress, RemoteInfo.typeref.addr);
  MinAddress = std::min(MinAddress, RemoteInfo.reflstr.addr);
  MinAddress = std::min(MinAddress, RemoteInfo.assocty.addr);
  auto totalSizeOfReflectionSections = RemoteInfo.fieldmd.size +
    RemoteInfo.typeref.size +
    RemoteInfo.reflstr.size +
    RemoteInfo.assocty.size;

  vm_address_t LocalAddress = 0;
  vm_prot_t CurrentProtection;
  vm_prot_t MaxProtection;
  error = vm_remap(mach_task_self(),
                   &LocalAddress,
                   totalSizeOfReflectionSections,
                   /*mask*/ 0,
                   VM_FLAGS_ANYWHERE | VM_FLAGS_RETURN_DATA_ADDR | VM_FLAGS_RESILIENT_CODESIGN,
                   childTask,
                   MinAddress,
                   /*copy*/ true,
                   &CurrentProtection,
                   &MaxProtection,
                   VM_INHERIT_DEFAULT);
  guardMachError(error, "vm_map reflection sections");

  auto Base_fieldmd = LocalAddress + (RemoteInfo.fieldmd.addr - MinAddress);
  FieldSection Fields(Base_fieldmd, Base_fieldmd + RemoteInfo.fieldmd.size);

  auto Base_assocty = LocalAddress + (RemoteInfo.assocty.addr - MinAddress);
  AssociatedTypeSection AssociatedTypes(Base_assocty,
                                        Base_assocty + RemoteInfo.assocty.size);

  auto Base_typeref = LocalAddress + (RemoteInfo.typeref.addr - MinAddress);
  GenericSection Typerefs(Base_typeref, Base_typeref + RemoteInfo.typeref.size);

  auto Base_reflstr = LocalAddress + (RemoteInfo.reflstr.addr - MinAddress);
  GenericSection Reflstr(Base_reflstr, Base_reflstr + RemoteInfo.reflstr.size);

  auto ImageName = Reader.readString(RemoteInfo.image_name_addr);
  if (ImageName.empty())
    llvm_unreachable("No image name for reflection info!");

  return { ImageName, Fields, AssociatedTypes, Typerefs, Reflstr };
}

static task_t childTask;

static uint8_t machGetPointerSize() {
  return sizeof(uintptr_t);
};

static uint8_t machGetSizeSize() {
  return sizeof(size_t);
};

static bool machReadBytes(const addr_t Address, uint8_t *Dest, uint64_t Size) {
  mach_msg_type_number_t SizeRead = 0;
  vm_offset_t Data = 0;
  auto Result = vm_read(childTask, Address, Size, &Data, &SizeRead);
  guardMachError(Result, "vm_read");
  if (Result == KERN_SUCCESS && SizeRead == Size) {
    memmove(reinterpret_cast<void *>(Dest),
            reinterpret_cast<void *>(Data), Size);
    vm_deallocate(childTask, Data, SizeRead);
    return true;
  }
  return false;
}

static bool machReadInteger(const addr_t Address, uint64_t *Value, uint8_t Size) {
  return machReadBytes(Address, reinterpret_cast<uint8_t *>(Value), Size);
};

uint64_t machGetStringLength(const addr_t BaseAddress) {
  addr_t Limit = BaseAddress + 1024 * 1024;
  addr_t Address = BaseAddress;
  uint64_t Length = 0;
  std::vector<uint8_t> Bytes;
  mach_msg_type_number_t SizeRead = 0;
  vm_offset_t DataOffset = 0;
  const size_t ChunkSize = 8;
  while (Address < Limit) {
    auto Result = vm_read(childTask, Address, ChunkSize, &DataOffset,
                          &SizeRead);
    guardMachError(Result, "vm_read");
    auto *Data = reinterpret_cast<uint8_t *>(DataOffset);
    for (size_t i = 0; i < 8; ++i) {
      if (Data[i] == 0) {
        vm_deallocate(childTask, DataOffset, SizeRead);
        return Length;
      }
      ++Length;
    }
    Address += ChunkSize;
    vm_deallocate(childTask, DataOffset, SizeRead);
  }
  return 0;
};

static std::unique_ptr<MemoryReaderImpl> getMachMemoryReaderImpl() {
  auto Impl = std::unique_ptr<MemoryReaderImpl>(new MemoryReaderImpl());
  Impl->getPointerSize = machGetPointerSize;
  Impl->getSizeSize = machGetSizeSize;
  Impl->readBytes = machReadBytes;
  Impl->readInteger = machReadInteger;
  Impl->getStringLength = machGetStringLength;
  return Impl;
}

static int doDumpHeapInstance(std::string BinaryFilename) {
  auto childPort = createReceivePort();
  mach_error_t error = task_set_bootstrap_port(mach_task_self(), childPort);
  guardMachError(error, "task_set_bootstrap_port");

  pid_t pid = fork();
  switch (pid) {
    case -1:
      error = mach_port_deallocate(mach_task_self(), childPort);
      guardMachError(error, "mach_port_deallocate");
      exit(EXIT_FAILURE);
    case 0: { // Child:
      execv(BinaryFilename.c_str(), NULL);
      exit(EXIT_SUCCESS);
    }
    default: { // Parent
      error = task_get_bootstrap_port(mach_task_self(), &bootstrap_port);
      guardMachError(error, "reset task_get_bootstrap_port");
      std::cerr << "Parent: Getting child's Mach port ..." << std::endl;
      childTask = receivePort(childPort);
      std::cerr << "Parent: Mach child task is: " << childTask << std::endl;

      uint64_t numReflectionInfos = receive_uint64_t(childPort);
      std::cout << "Parent: " << numReflectionInfos;
      std::cout << " reflection info bundles" << std::endl;

      MemoryReader Reader(getMachMemoryReaderImpl());
      ReflectionContext<External<RuntimeTarget<8>>> RC(Reader);

      for (uint64_t i = 0; i < numReflectionInfos; ++i) {
        auto info = receiveReflectionInfo(Reader, childPort, childTask);
        RC.addReflectionInfo(info);
      }

      addr_t isa = receive_uint64_t(childPort);

      std::cerr << "Parent: isa pointer in child address space: 0x";
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

      vm_offset_t address;
      mach_msg_type_number_t size_read;
      error = vm_read(childTask, isa, 256, &address, &size_read);

      struct task_basic_info childTaskBasicInfo;
      mach_msg_type_number_t size = TASK_BASIC_INFO_COUNT;
      error = task_info(childTask, TASK_BASIC_INFO,
                        (task_info_t)&childTaskBasicInfo, &size);

      error = mach_port_deallocate(mach_task_self(), childPort);
      guardMachError(error, "mach_port_deallocate");
    }
  }

  kill(pid, SIGTERM);

  error = task_set_bootstrap_port(mach_task_self(), bootstrap_port);
  guardMachError(error, "reset task_set_bootstrap_port");

  error = mach_port_deallocate(mach_task_self(), childPort);
  guardMachError(error, "mach_port_deallocate");

  return EXIT_SUCCESS;
}

#endif // __APPLE__

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Reflection Test\n");
  switch (options::Action) {
  case ActionType::DumpReflectionSections:
    return doDumpReflectionSections(options::BinaryFilename,
                                    options::Architecture);
  case ActionType::DumpHeapInstance:
#if __APPLE__
    return doDumpHeapInstance(options::BinaryFilename);
#else
    std::cerr << "Dumping heap instances not available on this platform";
    std::cerr << std::endl;
    return EXIT_FAILURE;
#endif
  case ActionType::None:
    llvm::cl::PrintHelpMessage();
    return EXIT_FAILURE;
    break;
  }
}
