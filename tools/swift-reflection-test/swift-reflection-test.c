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

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#include "swift/SwiftRemoteMirror/SwiftRemoteMirror.h"
#include "messages.h"
#include "overrides.h"

#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct RemoteSection {
  uintptr_t StartAddress;
  uintptr_t Size;
  uintptr_t EndAddress;
} RemoteSection;

typedef struct PipeMemoryReader {
  int to_child[2];
  int from_child[2];
} PipeMemoryReader;

typedef struct RemoteReflectionInfo {
  const char *ImageName;
  RemoteSection fieldmd;
  RemoteSection assocty;
  RemoteSection builtin;
  RemoteSection typeref;
  RemoteSection reflstr;
  uintptr_t StartAddress;
  size_t TotalSize;
} RemoteReflectionInfo;

typedef struct LocalReflectionInfo {
  const char *ImageName;
  swift_reflection_section_t fieldmd;
  swift_reflection_section_t assocty;
  swift_reflection_section_t builtin;
  swift_reflection_section_t typeref;
  swift_reflection_section_t reflstr;
} LocalReflectionInfo;

static void errorAndExit(const char *message) {
  fprintf(stderr, "%s: %s\n", message, strerror(errno));
  abort();
}

RemoteSection makeRemoteSection(uintptr_t StartAddress, uintptr_t Size) {
  RemoteSection RS = {
    StartAddress,
    Size,
    StartAddress + Size
  };
  return RS;
}

swift_reflection_section_t
makeLocalSection(uintptr_t StartAddress, size_t Size) {
  swift_reflection_section_t LS = {
    (void *)StartAddress,
    (void *)(StartAddress + Size)
  };
  return LS;
}

uintptr_t getStartAddress(const RemoteSection Sections[], size_t Count) {
  uintptr_t Start = 0;
  for (size_t i = 0; i < Count; ++i) {
    if (Sections[i].StartAddress != 0) {
      if (Start != 0)
        Start = MIN(Start, Sections[i].StartAddress);
      else
        Start = Sections[i].StartAddress;
    }
  }
  return Start;
}

uintptr_t getEndAddress(const RemoteSection Sections[], size_t Count) {
  uintptr_t End = 0;
  for (size_t i = 0; i < Count; ++i) {
    if (Sections[i].StartAddress != 0)
      End = MAX(End, Sections[i].EndAddress);
  }
  return End;
}

RemoteReflectionInfo makeRemoteReflectionInfo(const char *ImageName,
                                              RemoteSection fieldmd,
                                              RemoteSection assocty,
                                              RemoteSection builtin,
                                              RemoteSection typeref,
                                              RemoteSection reflstr) {
  RemoteReflectionInfo Info = {
    ImageName,
    fieldmd,
    assocty,
    builtin,
    typeref,
    reflstr,
    0,
    0
  };

  const RemoteSection Sections[5] = {
    fieldmd, assocty, builtin, typeref, reflstr
  };

  Info.StartAddress = getStartAddress(Sections, 5);

  uintptr_t EndAddress = getEndAddress(Sections, 5);
  Info.TotalSize = EndAddress - Info.StartAddress;

  return Info;
}

swift_reflection_section_t
makeSwiftReflectionSection(uintptr_t Base, size_t Size) {
  swift_reflection_section_t section;
  section.Begin = (void*)Base;
  section.End = (void*)(Base + Size);
  return section;
}

static const size_t ReadEnd = 0;
static const size_t WriteEnd = 1;

int PipeMemoryReader_getParentReadFD(const PipeMemoryReader *Reader) {
  return Reader->from_child[ReadEnd];
}

int PipeMemoryReader_getChildWriteFD(const PipeMemoryReader *Reader) {
  return Reader->from_child[WriteEnd];
}

int PipeMemoryReader_getParentWriteFD(const PipeMemoryReader *Reader) {
    return Reader->to_child[WriteEnd];
}

int PipeMemoryReader_getChildReadFD(const PipeMemoryReader *Reader) {
    return Reader->to_child[ReadEnd];
}

uint8_t PipeMemoryReader_getPointerSize(void *Context) {
  return sizeof(uintptr_t);
}

uint8_t PipeMemoryReader_getSizeSize(void *Context) {
  return sizeof(size_t);
}

void PipeMemoryReader_collectBytesFromPipe(const PipeMemoryReader *Reader,
                                           uint8_t *Dest, size_t Size) {
  int ReadFD = PipeMemoryReader_getParentReadFD(Reader);
  while (Size) {
    int bytesRead = read(ReadFD, Dest, Size);
    if (bytesRead == -EINTR)
      continue;
    if (bytesRead <= 0)
      errorAndExit("collectBytesFromPipe");
    Size -= bytesRead;
    Dest += bytesRead;
  }
}

int PipeMemoryReader_readBytes(void *Context,
                               addr_t Address, uint8_t *Dest, uint64_t Size) {
  const PipeMemoryReader *Reader = (const PipeMemoryReader *)Context;
  uintptr_t TargetAddress = Address;
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_READ_BYTES, 2);
  write(WriteFD, &TargetAddress, sizeof(TargetAddress));
  write(WriteFD, &Size, sizeof(size_t));
  PipeMemoryReader_collectBytesFromPipe(Reader, Dest, Size);
  return 1;
}

addr_t PipeMemoryReader_getSymbolAddress(void *Context, const char *SymbolName,
                                         uint64_t Length) {
  const PipeMemoryReader *Reader = (const PipeMemoryReader *)Context;
  uintptr_t Address = 0;
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_SYMBOL_ADDRESS, 2);
  write(WriteFD, SymbolName, Length);
  write(WriteFD, "\n", 1);
  PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&Address,
                                        sizeof(Address));
  return (uintptr_t)Address;
}

uintptr_t
PipeMemoryReader_receiveInstanceAddress(const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_INSTANCE_ADDRESS, 2);
  uintptr_t InstanceAddress = 0;
  PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t *)&InstanceAddress,
                                        sizeof(InstanceAddress));
  return InstanceAddress;
}

void PipeMemoryReader_sendExitMessage(const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_EXIT, 2);
}

uint8_t PipeMemoryReader_receivePointerSize(const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_POINTER_SIZE, 2);
  uint8_t PointerSize = 0;
  PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&PointerSize,
                                        sizeof(PointerSize));
  return PointerSize;
}

PipeMemoryReader createPipeMemoryReader() {
  PipeMemoryReader Reader;
  if (pipe(Reader.to_child))
    errorAndExit("Couldn't create pipes to child process");
  if (pipe(Reader.from_child))
    errorAndExit("Couldn't create pipes from child process");
  return Reader;
}

const LocalReflectionInfo *
PipeMemoryReader_receiveReflectionInfo(const PipeMemoryReader *Reader,
                                       size_t *NumReflectionInfos) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_REFLECTION_INFO, 2);
  PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)NumReflectionInfos,
                                        sizeof(NumReflectionInfos));

  if (*NumReflectionInfos == 0)
    return NULL;

  size_t Size = sizeof(RemoteReflectionInfo) * *NumReflectionInfos;
  RemoteReflectionInfo *RemoteInfos = (RemoteReflectionInfo*)malloc(Size);

  for (size_t i = 0; i < *NumReflectionInfos; ++i) {
    size_t ImageNameLength;
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&ImageNameLength,
                                          sizeof(ImageNameLength));
    const char *ImageName = (const char *)malloc(ImageNameLength);
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)ImageName,
                                          ImageNameLength);
    uintptr_t fieldmd_start;
    size_t fieldmd_size;
    uintptr_t assocty_start;
    size_t assocty_size;
    uintptr_t builtin_start;
    size_t builtin_size;
    uintptr_t typeref_start;
    size_t typeref_size;
    uintptr_t reflstr_start;
    size_t reflstr_size;

    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&fieldmd_start,
                                          sizeof(fieldmd_start));
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&fieldmd_size,
                                          sizeof(fieldmd_size));
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&assocty_start,
                                          sizeof(assocty_start));
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&assocty_size,
                                          sizeof(assocty_size));
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&builtin_start,
                                          sizeof(builtin_start));
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&builtin_size,
                                          sizeof(builtin_size));
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&typeref_start,
                                          sizeof(typeref_start));
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&typeref_size,
                                          sizeof(typeref_size));
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&reflstr_start,
                                          sizeof(reflstr_start));
    PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&reflstr_size,
                                          sizeof(reflstr_size));

    RemoteInfos[i] = makeRemoteReflectionInfo(
      ImageName,
      makeRemoteSection(fieldmd_start, fieldmd_size),
      makeRemoteSection(assocty_start, assocty_size),
      makeRemoteSection(builtin_start, builtin_size),
      makeRemoteSection(typeref_start, typeref_size),
      makeRemoteSection(reflstr_start, reflstr_size));
  }

  // Now pull in the remote sections into our address space.

  LocalReflectionInfo *Infos
    = malloc(sizeof(LocalReflectionInfo) * *NumReflectionInfos);

  for (size_t i = 0; i < *NumReflectionInfos; ++i) {
    RemoteReflectionInfo RemoteInfo = RemoteInfos[i];

    uintptr_t buffer = (uintptr_t)malloc(RemoteInfo.TotalSize);

    int Success = PipeMemoryReader_readBytes((void*)Reader,
                                             RemoteInfo.StartAddress,
                                             (uint8_t*)buffer,
                                             RemoteInfo.TotalSize);
    if (!Success)
      errorAndExit("Couldn't read reflection information");

    uintptr_t fieldmd_base
      = buffer + RemoteInfo.fieldmd.StartAddress - RemoteInfo.StartAddress;
    uintptr_t assocty_base
      = buffer + RemoteInfo.assocty.StartAddress - RemoteInfo.StartAddress;
    uintptr_t builtin_base
      = buffer + RemoteInfo.builtin.StartAddress - RemoteInfo.StartAddress;
    uintptr_t typeref_base
      = buffer + RemoteInfo.typeref.StartAddress - RemoteInfo.StartAddress;
    uintptr_t reflstr_base
      = buffer + RemoteInfo.reflstr.StartAddress - RemoteInfo.StartAddress;

    LocalReflectionInfo Info = {
      RemoteInfo.ImageName,
      makeLocalSection(fieldmd_base, RemoteInfo.fieldmd.Size),
      makeLocalSection(assocty_base, RemoteInfo.assocty.Size),
      makeLocalSection(builtin_base, RemoteInfo.builtin.Size),
      makeLocalSection(typeref_base, RemoteInfo.typeref.Size),
      makeLocalSection(reflstr_base, RemoteInfo.reflstr.Size)
    };
    Infos[i] = Info;
  }
  return Infos;
}

uint64_t PipeMemoryReader_getStringLength(void *Context, addr_t Address) {
  const PipeMemoryReader *Reader = (const PipeMemoryReader *)Context;
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_STRING_LENGTH, 2);
  write(WriteFD, &Address, sizeof(Address));
  uintptr_t Length = 0;
  PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&Length,
                                         sizeof(Length));
  return Length;
}

int doDumpHeapInstance(const char *BinaryFilename) {
  PipeMemoryReader Pipe = createPipeMemoryReader();

  pid_t pid = _fork();
  switch (pid) {
    case -1:
      errorAndExit("Couldn't fork child process");
      exit(EXIT_FAILURE);
    case 0: { // Child:
      close(PipeMemoryReader_getParentWriteFD(&Pipe));
      close(PipeMemoryReader_getParentReadFD(&Pipe));
      dup2(PipeMemoryReader_getChildReadFD(&Pipe), STDIN_FILENO);
      dup2(PipeMemoryReader_getChildWriteFD(&Pipe), STDOUT_FILENO);
      _execv(BinaryFilename, NULL);
      exit(EXIT_SUCCESS);
    }
    default: { // Parent
      close(PipeMemoryReader_getChildReadFD(&Pipe));
      close(PipeMemoryReader_getChildWriteFD(&Pipe));

      SwiftReflectionContextRef RC = swift_reflection_createReflectionContext(
        (void*)&Pipe,
        PipeMemoryReader_getPointerSize,
        PipeMemoryReader_getSizeSize,
        PipeMemoryReader_readBytes,
        PipeMemoryReader_getStringLength,
        PipeMemoryReader_getSymbolAddress);

      uint8_t PointerSize = PipeMemoryReader_getPointerSize((void*)&Pipe);
      if (PointerSize != sizeof(uintptr_t))
        errorAndExit("Child process had unexpected architecture");

      uintptr_t instance = PipeMemoryReader_receiveInstanceAddress(&Pipe);
      assert(instance);
      printf("Parent: instance pointer in child address space: 0x%lx\n",
        instance);

      uintptr_t isa;
      if (!PipeMemoryReader_readBytes(&Pipe, instance, (uint8_t*)&isa,
                                      sizeof(isa)))
        errorAndExit("Couldn't get heap object's metadata address");

      size_t NumReflectionInfos = 0;
      const LocalReflectionInfo *Infos
        = PipeMemoryReader_receiveReflectionInfo(&Pipe, &NumReflectionInfos);

      for (size_t i = 0; i < NumReflectionInfos; ++i) {
        const LocalReflectionInfo Info = Infos[i];
        swift_reflection_addReflectionInfo(RC, Info.ImageName,
                                           Info.fieldmd,
                                           Info.assocty,
                                           Info.builtin,
                                           Info.typeref,
                                           Info.reflstr);
      }

      uintptr_t isaMask;
      if (!swift_reflection_readIsaMask(RC, &isaMask))
        errorAndExit("Couldn't read isa mask");
      printf("Parent: isa mask in child address space: 0x%lx\n", isaMask);

      isa &= isaMask;

      printf("Parent: metadata pointer in child address space: 0x%lx\n", isa);
      printf("Decoding type reference ...\n");

      swift_typeref_t TR = swift_reflection_typeRefForMetadata(RC, isa);
      swift_reflection_dumpTypeRef(TR);
      swift_reflection_dumpInfoForMetadata(RC, isa);
    }
  }

  PipeMemoryReader_sendExitMessage(&Pipe);
  return EXIT_SUCCESS;
}

void printUsageAndExit() {
  fprintf(stderr, "swift-reflection-test <binary filename>\n");
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
  if (argc != 2)
    printUsageAndExit();

  const char *BinaryFilename = argv[1];

  return doDumpHeapInstance(BinaryFilename);
}
