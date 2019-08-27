//===--- swift-reflection-test.c - Reflection testing application ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This file supports performing target-specific remote reflection tests
// on live swift executables.
//===----------------------------------------------------------------------===//

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#include "swift/SwiftRemoteMirror/SwiftRemoteMirror.h"
#include "swift/Demangling/ManglingMacros.h"
#include "messages.h"
#include "overrides.h"

#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h>
#elif defined(_WIN32)
#include <io.h>
#include <fcntl.h>
#endif

#if defined(__clang__) || defined(__GNUC__)
#define NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
#define NORETURN __declspec(noreturn)
#else
#define NORETURN
#endif

typedef struct PipeMemoryReader {
  int to_child[2];
  int from_child[2];
} PipeMemoryReader;

typedef struct RemoteSection {
  uintptr_t StartAddress;
  uintptr_t Size;
  uintptr_t EndAddress;
} RemoteSection;

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

NORETURN
static void errorAndExit(const char *message) {
  fprintf(stderr, "%s\n", message);
  abort();
}

NORETURN
static void errnoAndExit(const char *message) {
  fprintf(stderr, "%s: %s\n", message, strerror(errno));
  abort();
}

#if 0
#include <inttypes.h>
#define DEBUG_LOG(fmt, ...) fprintf(stderr, "%s: " fmt "\n",\
                                    __func__, __VA_ARGS__)
#else
#define DEBUG_LOG(fmt, ...) (void)0
#endif

static const size_t ReadEnd = 0;
static const size_t WriteEnd = 1;

static
int PipeMemoryReader_getParentReadFD(const PipeMemoryReader *Reader) {
  return Reader->from_child[ReadEnd];
}

static
int PipeMemoryReader_getChildWriteFD(const PipeMemoryReader *Reader) {
  return Reader->from_child[WriteEnd];
}

static
int PipeMemoryReader_getParentWriteFD(const PipeMemoryReader *Reader) {
    return Reader->to_child[WriteEnd];
}

static
int PipeMemoryReader_getChildReadFD(const PipeMemoryReader *Reader) {
    return Reader->to_child[ReadEnd];
}

static
uint8_t PipeMemoryReader_getPointerSize(void *Context) {
  return sizeof(uintptr_t);
}

static
void PipeMemoryReader_collectBytesFromPipe(const PipeMemoryReader *Reader,
                                           void *Dest, size_t Size) {
  int ReadFD = PipeMemoryReader_getParentReadFD(Reader);
  while (Size) {
    int bytesRead = read(ReadFD, Dest, Size);
    if (bytesRead < 0)
      if (errno == EINTR)
        continue;
      else
        errnoAndExit("collectBytesFromPipe");
    else if (bytesRead == 0)
      errorAndExit("collectBytesFromPipe: Unexpected end of file");
    Size -= bytesRead;
    // Arithmetic on a void pointer is a GNU extension.
    Dest = (char*)(Dest) + bytesRead;
  }
}

static int PipeMemoryReader_queryDataLayout(void *Context,
                                             DataLayoutQueryType type,
                                             void *inBuffer, void *outBuffer) {
  switch (type) {
    case DLQ_GetPointerSize: {
      uint8_t *result = (uint8_t *)outBuffer;
      *result = sizeof(void *);
      return 1;
    }
    case DLQ_GetSizeSize: {
      uint8_t *result = (uint8_t *)outBuffer;
      *result = sizeof(size_t);
      return 1;
    }
  }

  return 0;
}

static void PipeMemoryReader_freeBytes(void *reader_context, const void *bytes,
                                       void *context) {
  free((void *)bytes);
}

static
const void *PipeMemoryReader_readBytes(void *Context, swift_addr_t Address,
                                       uint64_t Size, void **outFreeContext) {
  const PipeMemoryReader *Reader = (const PipeMemoryReader *)Context;
  uintptr_t TargetAddress = Address;
  size_t TargetSize = (size_t)Size;
  DEBUG_LOG("Requesting read of %zu bytes from 0x%" PRIxPTR,
            TargetSize, TargetAddress);
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_READ_BYTES, 2);
  write(WriteFD, &TargetAddress, sizeof(TargetAddress));
  write(WriteFD, &TargetSize, sizeof(size_t));
  
  void *Buf = malloc(Size);
  PipeMemoryReader_collectBytesFromPipe(Reader, Buf, Size);
  
  *outFreeContext = NULL;
  
  return Buf;
}

static
swift_addr_t PipeMemoryReader_getSymbolAddress(void *Context,
                                               const char *SymbolName,
                                               uint64_t Length) {
  const PipeMemoryReader *Reader = (const PipeMemoryReader *)Context;
  uintptr_t Address = 0;
  DEBUG_LOG("Requesting address of symbol %s", SymbolName);
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_SYMBOL_ADDRESS, 2);
  write(WriteFD, SymbolName, Length);
  write(WriteFD, "\n", 1);
  PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t*)&Address,
                                        sizeof(Address));
  DEBUG_LOG("Address of %s is 0x%" PRIxPTR, SymbolName, Address);
  return (uintptr_t)Address;
}

static InstanceKind
PipeMemoryReader_receiveInstanceKind(const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_INSTANCE_KIND, 2);
  uint8_t KindValue = 0;
  PipeMemoryReader_collectBytesFromPipe(Reader, &KindValue, sizeof(KindValue));
  DEBUG_LOG("Requested instance kind is %u", KindValue);
  return KindValue;
}

static uintptr_t
PipeMemoryReader_receiveInstanceAddress(const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_INSTANCE_ADDRESS, 2);
  uintptr_t InstanceAddress = 0;
  PipeMemoryReader_collectBytesFromPipe(Reader, (uint8_t *)&InstanceAddress,
                                        sizeof(InstanceAddress));
  DEBUG_LOG("Requested instance address is 0x%" PRIxPTR, InstanceAddress);
  return InstanceAddress;
}

static
void PipeMemoryReader_sendDoneMessage(const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_DONE, 2);
}

static
PipeMemoryReader createPipeMemoryReader() {
  PipeMemoryReader Reader;
#if defined(_WIN32)
  if (_pipe(Reader.to_child, 256, _O_BINARY))
    errnoAndExit("Couldn't create pipes to child process");
  if (_pipe(Reader.from_child, 256, _O_BINARY))
    errnoAndExit("Couldn't create pipes from child process");
#else
  if (pipe(Reader.to_child))
    errnoAndExit("Couldn't create pipes to child process");
  if (pipe(Reader.from_child))
    errnoAndExit("Couldn't create pipes from child process");
#endif
  return Reader;
}

#if defined(__APPLE__) && defined(__MACH__)
static void
PipeMemoryReader_receiveImages(SwiftReflectionContextRef RC,
                                       const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_IMAGES, 2);
  size_t NumReflectionInfos;
  PipeMemoryReader_collectBytesFromPipe(Reader, &NumReflectionInfos,
                                        sizeof(NumReflectionInfos));
  DEBUG_LOG("Receiving %z images from child", NumReflectionInfos);

  if (NumReflectionInfos == 0)
    return;
  
  struct { uintptr_t Start, Size; } *Images;
  Images = calloc(NumReflectionInfos, sizeof(*Images));
  PipeMemoryReader_collectBytesFromPipe(Reader, Images,
                                        NumReflectionInfos * sizeof(*Images));
  
  for (size_t i = 0; i < NumReflectionInfos; ++i) {
    DEBUG_LOG("Adding image at 0x%" PRIxPTR, Images[i].Start);
    swift_reflection_addImage(RC, Images[i].Start);
  }
  
  free(Images);
}

#else

static swift_reflection_section_t
makeLocalSection(const void *Buffer, RemoteSection Section,
                 RemoteReflectionInfo Info) {
  if (Section.Size == 0) {
    swift_reflection_section_t LS = {NULL, NULL};
    return LS;
  }

  uintptr_t Base
    = (uintptr_t)Buffer + Section.StartAddress - Info.StartAddress;
  swift_reflection_section_t LS = {
    (void *)Base,
    (void *)(Base + Section.Size)
  };
  return LS;
}

static
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

static
uintptr_t getEndAddress(const RemoteSection Sections[], size_t Count) {
  uintptr_t End = 0;
  for (size_t i = 0; i < Count; ++i) {
    if (Sections[i].StartAddress != 0)
      End = MAX(End, Sections[i].EndAddress);
  }
  return End;
}

static
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

static
RemoteSection makeRemoteSection(const PipeMemoryReader *Reader) {
  uintptr_t Start;
  size_t Size;

  PipeMemoryReader_collectBytesFromPipe(Reader, &Start, sizeof(Start));
  PipeMemoryReader_collectBytesFromPipe(Reader, &Size, sizeof(Size));

  RemoteSection RS = {Start, Size, Start + Size};
  return RS;
}

static void
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
    errnoAndExit("malloc failed");

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

    void *outFreeContext = NULL;
    const void *Buffer = PipeMemoryReader_readBytes((void *)Reader,
                                                    RemoteInfo.StartAddress,
                                                    RemoteInfo.TotalSize,
                                                    &outFreeContext);
    if (!Buffer)
      errorAndExit("Couldn't read reflection information");

    swift_reflection_info_t Info = {
      {makeLocalSection(Buffer, RemoteInfo.fieldmd, RemoteInfo), 0},
      {makeLocalSection(Buffer, RemoteInfo.assocty, RemoteInfo), 0},
      {makeLocalSection(Buffer, RemoteInfo.builtin, RemoteInfo), 0},
      {makeLocalSection(Buffer, RemoteInfo.capture, RemoteInfo), 0},
      {makeLocalSection(Buffer, RemoteInfo.typeref, RemoteInfo), 0},
      {makeLocalSection(Buffer, RemoteInfo.reflstr, RemoteInfo), 0},
      /*LocalStartAddress*/ (uintptr_t) Buffer,
      /*RemoteStartAddress*/ RemoteInfo.StartAddress,
    };
    swift_reflection_addReflectionInfo(RC, Info);
  }

  free(RemoteInfos);
}
#endif

uint64_t PipeMemoryReader_getStringLength(void *Context, swift_addr_t Address) {
  const PipeMemoryReader *Reader = (const PipeMemoryReader *)Context;
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  uintptr_t TargetAddress = (uintptr_t)Address;
  write(WriteFD, REQUEST_STRING_LENGTH, 2);
  write(WriteFD, &TargetAddress, sizeof(TargetAddress));
  uintptr_t Length = 0;
  PipeMemoryReader_collectBytesFromPipe(Reader, &Length, sizeof(Length));
  return Length;
}

int reflectHeapObject(SwiftReflectionContextRef RC,
                       const PipeMemoryReader Pipe) {
  uintptr_t instance = PipeMemoryReader_receiveInstanceAddress(&Pipe);
  if (instance == 0) {
    // Child has no more instances to examine
    PipeMemoryReader_sendDoneMessage(&Pipe);
    return 0;
  }
  printf("Instance pointer in child address space: 0x%lx\n",
         instance);

  swift_typeref_t TR = swift_reflection_typeRefForInstance(RC, instance);

  printf("Type reference:\n");
  swift_reflection_dumpTypeRef(TR);
  printf("\n");

  printf("Type info:\n");
  swift_reflection_dumpInfoForInstance(RC, instance);

  printf("\n");

  PipeMemoryReader_sendDoneMessage(&Pipe);
  return 1;
}

int reflectExistential(SwiftReflectionContextRef RC,
                       const PipeMemoryReader Pipe,
                       swift_typeref_t MockExistentialTR) {
  uintptr_t instance = PipeMemoryReader_receiveInstanceAddress(&Pipe);
  if (instance == 0) {
    // Child has no more instances to examine
    PipeMemoryReader_sendDoneMessage(&Pipe);
    return 0;
  }
  printf("Instance pointer in child address space: 0x%lx\n",
         instance);

  swift_typeref_t InstanceTypeRef;
  swift_addr_t StartOfInstanceData = 0;

  if (!swift_reflection_projectExistential(RC, instance, MockExistentialTR,
                                           &InstanceTypeRef,
                                           &StartOfInstanceData)) {
    printf("swift_reflection_projectExistential failed.\n");
    PipeMemoryReader_sendDoneMessage(&Pipe);
    return 0;
  }

  printf("Type reference:\n");
  swift_reflection_dumpTypeRef(InstanceTypeRef);
  printf("\n");

  printf("Type info:\n");
  swift_reflection_dumpInfoForTypeRef(RC, InstanceTypeRef);
  printf("\n");

  PipeMemoryReader_sendDoneMessage(&Pipe);
  return 1;
}

int doDumpHeapInstance(const char *BinaryFilename) {
  PipeMemoryReader Pipe = createPipeMemoryReader();

#if defined(_WIN32)
#else
  pid_t pid = _fork();
  switch (pid) {
    case -1:
      errnoAndExit("Couldn't fork child process");
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
      SwiftReflectionContextRef RC =
          swift_reflection_createReflectionContextWithDataLayout(
              (void *)&Pipe, PipeMemoryReader_queryDataLayout,
              PipeMemoryReader_freeBytes, PipeMemoryReader_readBytes,
              PipeMemoryReader_getStringLength,
              PipeMemoryReader_getSymbolAddress);

      uint8_t PointerSize = PipeMemoryReader_getPointerSize((void*)&Pipe);
      if (PointerSize != sizeof(uintptr_t))
        errorAndExit("Child process had unexpected architecture");

#if defined(__APPLE__) && defined(__MACH__)
      PipeMemoryReader_receiveImages(RC, &Pipe);
#else
      PipeMemoryReader_receiveReflectionInfo(RC, &Pipe);
#endif

      while (1) {
        InstanceKind Kind = PipeMemoryReader_receiveInstanceKind(&Pipe);
        switch (Kind) {
        case Object:
          printf("Reflecting an object.\n");
          if (!reflectHeapObject(RC, Pipe))
            return EXIT_SUCCESS;
          break;
        case Existential: {
          static const char Name[] = MANGLING_PREFIX_STR "ypD";
          swift_typeref_t AnyTR
            = swift_reflection_typeRefForMangledTypeName(RC,
              Name, sizeof(Name)-1);

          printf("Reflecting an existential.\n");
          if (!reflectExistential(RC, Pipe, AnyTR))
            return EXIT_SUCCESS;
          break;
        }
        case ErrorExistential: {
          static const char ErrorName[] = MANGLING_PREFIX_STR "s5Error_pD";
          swift_typeref_t ErrorTR
            = swift_reflection_typeRefForMangledTypeName(RC,
              ErrorName, sizeof(ErrorName)-1);
          printf("Reflecting an error existential.\n");
          if (!reflectExistential(RC, Pipe, ErrorTR))
            return EXIT_SUCCESS;
          break;
        }
        case Closure:
          printf("Reflecting a closure.\n");
          if (!reflectHeapObject(RC, Pipe))
            return EXIT_SUCCESS;
          break;
        case None:
          swift_reflection_destroyReflectionContext(RC);
          printf("Done.\n");
          return EXIT_SUCCESS;
        }
      }
    }
  }
#endif
  return EXIT_SUCCESS;
}

#if defined(__APPLE__) && defined(__MACH__)
#include <dlfcn.h>
static unsigned long long computeClassIsSwiftMask(void) {
  uintptr_t *objc_debug_swift_stable_abi_bit_ptr =
    (uintptr_t *)dlsym(RTLD_DEFAULT, "objc_debug_swift_stable_abi_bit");
  return objc_debug_swift_stable_abi_bit_ptr ?
           *objc_debug_swift_stable_abi_bit_ptr : 1;
}
#else
static unsigned long long computeClassIsSwiftMask(void) {
  return 1;
}
#endif

void printUsageAndExit() {
  fprintf(stderr, "swift-reflection-test <binary filename>\n");
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
  if (argc != 2)
    printUsageAndExit();

  const char *BinaryFilename = argv[1];

#if defined(_WIN32)
  // FIXME(compnerd) weak linking is not permitted on PE/COFF, we should fall
  // back to GetProcAddress to see if the symbol is present.
#else
  // swift_reflection_classIsSwiftMask is weak linked so we can work
  // with older Remote Mirror dylibs.
  if (&swift_reflection_classIsSwiftMask != NULL)
    swift_reflection_classIsSwiftMask = computeClassIsSwiftMask();
#endif

  uint16_t Version = swift_reflection_getSupportedMetadataVersion();
  printf("Metadata version: %u\n", Version);

  return doDumpHeapInstance(BinaryFilename);
}
