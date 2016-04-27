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
// This file supports performing target-specific remote reflection tests
// on live swift executables.
//===----------------------------------------------------------------------===//

#include "swift/Reflection/ReflectionContext.h"
#include "swift/Reflection/TypeRef.h"
#include "llvm/ADT/Optional.h"
#include "messages.h"
#include "overrides.h"

#include <unistd.h>

using namespace swift;
using namespace swift::reflection;
using namespace swift::remote;
using namespace Demangle;

static void errorAndExit(const std::string &message) {
  std::cerr << message << ": " << strerror(errno) << std::endl;
  abort();
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
  const Section<Runtime> builtin;
  const Section<Runtime> typeref;
  const Section<Runtime> reflstr;
  const StoredPointer StartAddress;
  const StoredSize TotalSize;

  static StoredPointer getStartAddress(std::vector<Section<Runtime>> &&elts) {
    StoredPointer Start = 0;
    for (auto elt : elts) {
      if (elt.StartAddress != 0) {
        if (Start != 0)
          Start = std::min(Start, elt.StartAddress);
        else
          Start = elt.StartAddress;
      }
    }
    return Start;
  }

  static StoredPointer getEndAddress(std::vector<Section<Runtime>> &&elts) {
    StoredPointer End = 0;
    for (auto elt : elts) {
      if (elt.StartAddress != 0)
        End = std::max(End, elt.getEndAddress());
    }
    return End;
  }

  RemoteReflectionInfo(std::string ImageName,
                       Section<Runtime> fieldmd,
                       Section<Runtime> assocty,
                       Section<Runtime> builtin,
                       Section<Runtime> typeref,
                       Section<Runtime> reflstr)
    : ImageName(ImageName),
      fieldmd(fieldmd),
      assocty(assocty),
      builtin(builtin),
      typeref(typeref),
      reflstr(reflstr),
      StartAddress(getStartAddress({
                     fieldmd,
                     assocty,
                     builtin,
                     typeref,
                     reflstr})),
      TotalSize(getEndAddress({
                     fieldmd,
                     assocty,
                     builtin,
                     typeref,
                     reflstr}) - StartAddress) {}
};
}


template <typename Runtime>
class PipeMemoryReader : public MemoryReader {
  using StoredPointer = typename Runtime::StoredPointer;
  using StoredSize = typename Runtime::StoredSize;

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
    auto Dest = reinterpret_cast<uint8_t *>(Value);
    while (Size) {
      auto bytesRead = read(getParentReadFD(), Dest, Size);
      if (bytesRead == -EINTR)
        continue;
      if (bytesRead <= 0)
        errorAndExit("collectBytesFromPipe");
      Size -= bytesRead;
      Dest += bytesRead;
    }
  }

  bool readBytes(RemoteAddress Address, uint8_t *Dest, uint64_t Size) override {

    StoredPointer TargetAddress = (StoredPointer)Address.getAddressData();
    write(getParentWriteFD(), REQUEST_READ_BYTES, 2);
    write(getParentWriteFD(), &TargetAddress, sizeof(TargetAddress));
    write(getParentWriteFD(), &Size, sizeof(StoredSize));
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

  RemoteAddress getSymbolAddress(const std::string &SymbolName) override {
    StoredPointer Address = 0;
    write(getParentWriteFD(), REQUEST_SYMBOL_ADDRESS, 2);
    write(getParentWriteFD(), SymbolName.c_str(), SymbolName.size());
    write(getParentWriteFD(), "\n", 1);
    collectBytesFromPipe(&Address, sizeof(Address));
    return RemoteAddress(static_cast<StoredPointer>(Address));
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
    StoredSize NumReflectionInfos = 0;
    collectBytesFromPipe(&NumReflectionInfos, sizeof(NumReflectionInfos));

    std::vector<RemoteReflectionInfo<Runtime>> RemoteInfos;
    for (StoredSize i = 0; i < NumReflectionInfos; ++i) {
      StoredSize ImageNameLength;
      collectBytesFromPipe(&ImageNameLength, sizeof(ImageNameLength));
      char c;
      std::string ImageName;
      for (StoredSize i = 0; i < ImageNameLength; ++i) {
        collectBytesFromPipe(&c, 1);
        ImageName.push_back(c);
      }

      StoredPointer fieldmd_start;
      StoredPointer fieldmd_size;
      StoredPointer assocty_start;
      StoredPointer assocty_size;
      StoredPointer builtin_start;
      StoredPointer builtin_size;
      StoredPointer typeref_start;
      StoredPointer typeref_size;
      StoredPointer reflstr_start;
      StoredPointer reflstr_size;

      collectBytesFromPipe(&fieldmd_start, sizeof(fieldmd_start));
      collectBytesFromPipe(&fieldmd_size, sizeof(fieldmd_size));
      collectBytesFromPipe(&assocty_start, sizeof(assocty_start));
      collectBytesFromPipe(&assocty_size, sizeof(assocty_size));
      collectBytesFromPipe(&builtin_start, sizeof(builtin_start));
      collectBytesFromPipe(&builtin_size, sizeof(builtin_size));
      collectBytesFromPipe(&typeref_start, sizeof(typeref_start));
      collectBytesFromPipe(&typeref_size, sizeof(typeref_size));
      collectBytesFromPipe(&reflstr_start, sizeof(reflstr_start));
      collectBytesFromPipe(&reflstr_size, sizeof(reflstr_size));

      RemoteInfos.push_back({
        ImageName,
        {fieldmd_start, fieldmd_size},
        {assocty_start, assocty_size},
        {builtin_start, builtin_size},
        {typeref_start, typeref_size},
        {reflstr_start, reflstr_size},
      });
    }

    std::vector<ReflectionInfo> Infos;
    for (auto &RemoteInfo : RemoteInfos) {

      std::cerr << "Fetching reflection info for " << RemoteInfo.ImageName
                << "\n";

      auto buffer = (uint8_t *)malloc(RemoteInfo.TotalSize);

      if (!readBytes(RemoteAddress(RemoteInfo.StartAddress), buffer,
                     RemoteInfo.TotalSize))
        errorAndExit("Couldn't read reflection information");

      auto fieldmd_base
        = buffer + RemoteInfo.fieldmd.StartAddress - RemoteInfo.StartAddress;
      auto assocty_base
        = buffer + RemoteInfo.assocty.StartAddress - RemoteInfo.StartAddress;
      auto builtin_base
        = buffer + RemoteInfo.builtin.StartAddress - RemoteInfo.StartAddress;
      auto typeref_base
        = buffer + RemoteInfo.typeref.StartAddress - RemoteInfo.StartAddress;
      auto reflstr_base
        = buffer + RemoteInfo.reflstr.StartAddress - RemoteInfo.StartAddress;
      ReflectionInfo Info {
        RemoteInfo.ImageName,
        {fieldmd_base, fieldmd_base + RemoteInfo.fieldmd.Size},
        {assocty_base, assocty_base + RemoteInfo.assocty.Size},
        {builtin_base, builtin_base + RemoteInfo.builtin.Size},
        {typeref_base, typeref_base + RemoteInfo.typeref.Size},
        {reflstr_base, reflstr_base + RemoteInfo.reflstr.Size},
      };
      Infos.push_back(Info);
    }
    return Infos;
  }

  uint64_t getStringLength(RemoteAddress addr) {
    write(getParentWriteFD(), REQUEST_STRING_LENGTH, 2);
    StoredPointer Address = addr.getAddressData();
    write(getParentWriteFD(), &Address, sizeof(Address));
    StoredPointer Length;
    collectBytesFromPipe(&Length, sizeof(Length));
    return static_cast<uint64_t>(Length);
  }

  bool readString(RemoteAddress Address, std::string &Dest) override {
    auto NameSize = getStringLength(Address);
    if (!NameSize)
      return false;

    auto NameBuffer = std::unique_ptr<uint8_t>(new uint8_t[NameSize + 1]);
    if (!readBytes(Address, NameBuffer.get(), NameSize + 1))
      return false;

    Dest = reinterpret_cast<const char *>(NameBuffer.get());
    return true;
  }
};


template <typename Runtime>
static int doDumpHeapInstance(std::string BinaryFilename) {
  using StoredPointer = typename Runtime::StoredPointer;

  auto Pipe = std::make_shared<PipeMemoryReader<Runtime>>();

  pid_t pid = _fork();
  switch (pid) {
    case -1:
      errorAndExit("Couldn't fork child process");
      exit(EXIT_FAILURE);
    case 0: { // Child:
      close(Pipe->getParentWriteFD());
      close(Pipe->getParentReadFD());
      dup2(Pipe->getChildReadFD(), STDIN_FILENO);
      dup2(Pipe->getChildWriteFD(), STDOUT_FILENO);
      _execv(BinaryFilename.c_str(), NULL);
      exit(EXIT_SUCCESS);
    }
    default: { // Parent
      close(Pipe->getChildReadFD());
      close(Pipe->getChildWriteFD());

      ReflectionContext<External<Runtime>> RC(Pipe);

      uint8_t PointerSize = Pipe->receivePointerSize();
      if (PointerSize != Runtime::PointerSize)
        errorAndExit("Child process had unexpected architecture");

      StoredPointer instance = Pipe->receiveInstanceAddress();
      assert(instance);
      std::cout << "Parent: instance pointer in child address space: 0x";
      std::cout << std::hex << instance << "\n";

      StoredPointer isa;
      if (!Pipe->readInteger(RemoteAddress(instance), &isa))
        errorAndExit("Couldn't get heap object's metadata address");

      for (auto &Info : Pipe->receiveReflectionInfo())
        RC.addReflectionInfo(Info);

      std::cout << "Parent: metadata pointer in child address space: 0x";
      std::cout << std::hex << isa << "\n";

      std::cout << "Decoding type reference ...\n";
      auto TR = RC.readTypeFromMetadata(isa);
      TR->dump(std::cout, 0);

      auto TI = RC.getTypeInfo(isa);
      if (TI != nullptr)
        TI->dump();
    }
  }

  Pipe->sendExitMessage();
  return EXIT_SUCCESS;
}

void printUsageAndExit() {
  std::cerr << "swift-reflection-test <arch> <binary filename>\n";
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {

  if (argc != 3)
    printUsageAndExit();

  std::string arch(argv[1]);
  std::string BinaryFilename(argv[2]);

  // FIXME: get this from LLVM
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
    return doDumpHeapInstance<External<RuntimeTarget<4>>>(BinaryFilename);
  else
    return doDumpHeapInstance<External<RuntimeTarget<8>>>(BinaryFilename);
}
