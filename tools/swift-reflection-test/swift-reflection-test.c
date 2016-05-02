//===--- swift-reflection-test.c - Reflection testing application ---------===//
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
  RemoteSection fieldmd;
  RemoteSection assocty;
  RemoteSection builtin;
  RemoteSection capture;
  RemoteSection typeref;
  RemoteSection reflstr;
  uintptr_t StartAddress;
  size_t TotalSize;
} RemoteReflectionInfo;

static void errorAndExit(const char *message) {
  fprintf(stderr, "%s: %s\n", message, strerror(errno));
  abort();
}

swift_reflection_section_t
makeLocalSection(void *Buffer, RemoteSection Section,
                 RemoteReflectionInfo Info) {
  uintptr_t Base
    = (uintptr_t)Buffer + Section.StartAddress - Info.StartAddress;
  swift_reflection_section_t LS = {
    (void *)Base,
    (void *)(Base + Section.Size)
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

RemoteReflectionInfo makeRemoteReflectionInfo(RemoteSection fieldmd,
                                              RemoteSection assocty,
                                              RemoteSection builtin,
                                              RemoteSection capture,
                                              RemoteSection typeref,
                                              RemoteSection reflstr) {
  RemoteReflectionInfo Info = {
    fieldmd,
    assocty,
    builtin,
    capture,
    typeref,
    reflstr,
    0,
    0
  };

  const RemoteSection Sections[6] = {
    fieldmd, assocty, builtin, capture, typeref, reflstr
  };

  Info.StartAddress = getStartAddress(Sections, 6);

  uintptr_t EndAddress = getEndAddress(Sections, 6);
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
                                           void *Dest, size_t Size) {
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
                               addr_t Address, void *Dest, uint64_t Size) {
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

void PipeMemoryReader_sendDoneMessage(const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_DONE, 2);
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

RemoteSection makeRemoteSection(const PipeMemoryReader *Reader) {
  uintptr_t Start;
  size_t Size;

  PipeMemoryReader_collectBytesFromPipe(Reader, &Start, sizeof(Start));
  PipeMemoryReader_collectBytesFromPipe(Reader, &Size, sizeof(Size));

  RemoteSection RS = {Start, Size, Start + Size};
  return RS;
}

void
PipeMemoryReader_receiveReflectionInfo(SwiftReflectionContextRef RC,
                                       const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_REFLECTION_INFO, 2);
  size_t NumReflectionInfos;
  PipeMemoryReader_collectBytesFromPipe(Reader, &NumReflectionInfos,
                                        sizeof(NumReflectionInfos));

  if (NumReflectionInfos == 0)
    return;

  RemoteReflectionInfo *RemoteInfos = calloc(NumReflectionInfos,
                                             sizeof(RemoteReflectionInfo));
  if (RemoteInfos == NULL)
    errorAndExit("malloc failed");

  for (size_t i = 0; i < NumReflectionInfos; ++i) {
    RemoteInfos[i] = makeRemoteReflectionInfo(
      makeRemoteSection(Reader),
      makeRemoteSection(Reader),
      makeRemoteSection(Reader),
      makeRemoteSection(Reader),
      makeRemoteSection(Reader),
      makeRemoteSection(Reader));
  }

  // Now pull in the remote sections into our address space.

  for (size_t i = 0; i < NumReflectionInfos; ++i) {
    RemoteReflectionInfo RemoteInfo = RemoteInfos[i];

    void *Buffer = malloc(RemoteInfo.TotalSize);

    int Success = PipeMemoryReader_readBytes((void *)Reader,
                                             RemoteInfo.StartAddress,
                                             Buffer,
                                             RemoteInfo.TotalSize);
    if (!Success)
      errorAndExit("Couldn't read reflection information");

    swift_reflection_info_t Info = {
      makeLocalSection(Buffer, RemoteInfo.fieldmd, RemoteInfo),
      makeLocalSection(Buffer, RemoteInfo.assocty, RemoteInfo),
      makeLocalSection(Buffer, RemoteInfo.builtin, RemoteInfo),
      makeLocalSection(Buffer, RemoteInfo.capture, RemoteInfo),
      makeLocalSection(Buffer, RemoteInfo.typeref, RemoteInfo),
      makeLocalSection(Buffer, RemoteInfo.reflstr, RemoteInfo)
    };
    swift_reflection_addReflectionInfo(RC, Info);
  }

  free(RemoteInfos);
}

uint64_t PipeMemoryReader_getStringLength(void *Context, addr_t Address) {
  const PipeMemoryReader *Reader = (const PipeMemoryReader *)Context;
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_STRING_LENGTH, 2);
  write(WriteFD, &Address, sizeof(Address));
  uintptr_t Length = 0;
  PipeMemoryReader_collectBytesFromPipe(Reader, &Length, sizeof(Length));
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
      
      PipeMemoryReader_receiveReflectionInfo(RC, &Pipe);

      while (1) {
        uintptr_t instance = PipeMemoryReader_receiveInstanceAddress(&Pipe);
        if (instance == 0) {
          // Child has no more instances to examine
          break;
        }
        printf("Instance pointer in child address space: 0x%lx\n",
               instance);

        printf("Type reference:\n");

        swift_typeref_t TR = swift_reflection_typeRefForInstance(RC, instance);
        swift_reflection_dumpTypeRef(TR);
        printf("\n");

        printf("Type info:\n");
        swift_reflection_dumpInfoForInstance(RC, instance);

        printf("\n");

        PipeMemoryReader_sendDoneMessage(&Pipe);
      }
    }
  }

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
