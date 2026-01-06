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

#define SECTIONS_PER_INFO 6

#include "swift/SwiftRemoteMirror/SwiftRemoteMirror.h"
#include "swift/Demangling/ManglingMacros.h"
#include "messages.h"
#include "overrides.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__)) || defined(__wasi__)
#include <unistd.h>
#elif defined(_WIN32)
#include <io.h>
#include <fcntl.h>
#endif

#if defined(__APPLE__) && defined(__MACH__)
#include <TargetConditionals.h>
#endif

#if __has_feature(ptrauth_calls)
#include <ptrauth.h>
#endif

#if defined(__clang__) || defined(__GNUC__)
#define NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
#define NORETURN __declspec(noreturn)
#else
#define NORETURN
#endif

typedef struct PipeMemoryReaderPage {
  struct PipeMemoryReaderPage *Next;
  swift_addr_t BaseAddress;
  uint64_t Size;
  char *Data;
} PipeMemoryReaderPage;


typedef struct PipeMemoryReader {
  int to_child[2];
  int from_child[2];
  PipeMemoryReaderPage *Pages;
} PipeMemoryReader;


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
#define DEBUG_LOG(fmt, ...) fprintf(stderr, "%s: " fmt "\n",\
                                    __func__, __VA_ARGS__)
#else
#define DEBUG_LOG(fmt, ...) (void)0
#endif

#ifdef __clang__
__attribute((__format__(__printf__, 2, 3)))
#endif
static void
indented_printf(unsigned indentLevel, const char *fmt, ...) {
  for (unsigned i = 0; i < indentLevel; i++)
    fputs("  ", stdout);

  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}

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
#if defined(__APPLE__) && __APPLE__
  int applePlatform = 1;
#else
  int applePlatform = 0;
#endif
#if defined(__APPLE__) && __APPLE__ && ((defined(TARGET_OS_IOS) && TARGET_OS_IOS) || (defined(TARGET_OS_IOS) && TARGET_OS_WATCH) || (defined(TARGET_OS_TV) && TARGET_OS_TV) || defined(__arm64__))
  int iosDerivedPlatform = 1;
#else
  int iosDerivedPlatform = 0;
#endif

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
    case DLQ_GetPtrAuthMask: {
      uintptr_t *result = (uintptr_t *)outBuffer;
#if __has_feature(ptrauth_calls)
      *result = (uintptr_t)ptrauth_strip((void*)0x0007ffffffffffff, 0);
#else
      *result = (uintptr_t)~0ull;
#endif
      return 1;
    }
    case DLQ_GetObjCReservedLowBits: {
      uint8_t *result = (uint8_t *)outBuffer;
      if (applePlatform && !iosDerivedPlatform && (sizeof(void *) == 8)) {
        // Only for 64-bit macOS (not iOS, not even when simulated on x86_64)
        *result = 1;
      } else {
        *result = 0;
      }
      return 1;
    }
    case DLQ_GetLeastValidPointerValue: {
      uint64_t *result = (uint64_t *)outBuffer;
      if (applePlatform && (sizeof(void *) == 8)) {
        // Swift reserves the first 4GiB on Apple 64-bit platforms
        *result = 0x100000000;
        return 1;
      } else {
        // Swift reserves the first 4KiB everywhere else
        *result = 0x1000;
      }
      return 1;
    }
    case DLQ_GetObjCInteropIsEnabled:
      break;
  }

  return 0;
}

static void PipeMemoryReader_freeBytes(void *reader_context, const void *bytes,
                                       void *context) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wcast-qual"
  free((void *)bytes);
#pragma clang diagnostic pop
}

static
const void *PipeMemoryReader_readBytes(void *Context, swift_addr_t Address,
                                       uint64_t Size, void **outFreeContext) {
  PipeMemoryReader *Reader = (PipeMemoryReader *)Context;

#if __has_feature(address_sanitizer)
  // ASAN dislikes reading arbitrary pages of memory, so
  // be more conservative and only read exactly the requested bytes.
  uintptr_t TargetAddress = Address;
  size_t TargetSize = Size;
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_READ_BYTES, 2);
  write(WriteFD, &TargetAddress, sizeof(TargetAddress));
  write(WriteFD, &TargetSize, sizeof(size_t));

  void *Buf = malloc(TargetSize);
  PipeMemoryReader_collectBytesFromPipe(Reader, Buf, TargetSize);
  *outFreeContext = NULL;
  return Buf;

#else
  PipeMemoryReaderPage *Page = Reader->Pages;

  // Try to find an existing page with the requested bytes
  while (Page != NULL) {
    if (Page->BaseAddress <= Address
	&& (Page->BaseAddress + Page->Size >= Address + Size)) {
      break;
    }
    Page = Page->Next;
  }

  // If none, fetch page(s) from the target
  if (Page == NULL) {
    static uint64_t PageSize = 4 * 1024;
    uintptr_t TargetAddress = Address - (Address % PageSize);
    uintptr_t TargetEnd = ((Address + Size + PageSize - 1) / PageSize) * PageSize;
    size_t TargetSize = TargetEnd - TargetAddress;
    DEBUG_LOG("Requesting read of %zu bytes from 0x%" PRIxPTR,
	      TargetSize, TargetAddress);
    int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
    write(WriteFD, REQUEST_READ_BYTES, 2);
    write(WriteFD, &TargetAddress, sizeof(TargetAddress));
    write(WriteFD, &TargetSize, sizeof(size_t));

    void *Data = malloc(TargetSize);
    PipeMemoryReader_collectBytesFromPipe(Reader, Data, TargetSize);
    PipeMemoryReaderPage *NewPage = malloc(sizeof(PipeMemoryReaderPage));
    NewPage->BaseAddress = TargetAddress;
    NewPage->Size = TargetSize;
    NewPage->Data = Data;
    NewPage->Next = Reader->Pages;
    Reader->Pages = NewPage;
    Page = NewPage;
  }

  // We have a page:  Copy bytes from it to satisfy the request
  assert(Page->BaseAddress <= Address);
  assert(Page->BaseAddress + Page->Size >= Address + Size);
  void *Buf = malloc(Size);
  memcpy(Buf, Page->Data + Address - Page->BaseAddress, Size);
  *outFreeContext = NULL;
  return Buf;
#endif
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

static uint8_t PipeMemoryReader_receiveShouldUnwrapExistential(
    const PipeMemoryReader *Reader) {
  int WriteFD = PipeMemoryReader_getParentWriteFD(Reader);
  write(WriteFD, REQUEST_SHOULD_UNWRAP_CLASS_EXISTENTIAL, 2);
  uint8_t ShouldUnwrap = 0;
  PipeMemoryReader_collectBytesFromPipe(Reader, &ShouldUnwrap,
                                        sizeof(ShouldUnwrap));
  DEBUG_LOG("Requested if should unwrap class existential is", KindValue);
  return ShouldUnwrap;
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
  Reader.Pages = NULL;
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

static
void flushPipeMemoryReader(PipeMemoryReader *Reader) {
  while (Reader->Pages != NULL) {
    PipeMemoryReaderPage *this = Reader->Pages;
    Reader->Pages = this->Next;
    free(this->Data);
    free(this);
  }
}

static
void destroyPipeMemoryReader(PipeMemoryReader *Reader) {
  flushPipeMemoryReader(Reader);
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
  DEBUG_LOG("Receiving %zu images from child", NumReflectionInfos);

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
makeLocalSection(const void *Buffer,
                 swift_remote_reflection_section_t Section) {
  if (Section.Size == 0) {
    swift_reflection_section_t LS = {NULL, NULL};
    return LS;
  }

  swift_reflection_section_t LS = {(void *)Buffer,
                                   (void *)((uint8_t *)Buffer + Section.Size)};
  return LS;
}

static swift_reflection_mapping_info_t makeNonContiguousReflectionInfo(
    swift_remote_reflection_section_t *remote_sections,
    swift_reflection_section_t *local_sections) {
  swift_reflection_section_mapping_t sections[SECTIONS_PER_INFO];
  for (size_t i = 0; i < SECTIONS_PER_INFO; ++i) {
    swift_reflection_section_mapping_t section = {local_sections[i],
                                                           remote_sections[i]};
    sections[i] = section;
  }

  swift_reflection_mapping_info_t ReflectionMappingInfo = {
      sections[0], sections[1], sections[2],
      sections[3], sections[4], sections[5]};

  return ReflectionMappingInfo;
}

static swift_remote_reflection_section_t
makeRemoteSection(const PipeMemoryReader *Reader) {
  uintptr_t Start;
  size_t Size;

  PipeMemoryReader_collectBytesFromPipe(Reader, &Start, sizeof(Start));
  PipeMemoryReader_collectBytesFromPipe(Reader, &Size, sizeof(Size));

  swift_remote_reflection_section_t RS = {Start, Size};
  DEBUG_LOG("Making remote section with Start = 0x%" PRIxPTR
            " End = 0x%" PRIxPTR " and  Size = %lu",
            RS.StartAddress, RS.StartAddress + RS.Size, RS.Size);

  return RS;
}

static const void *PipeMemoryReader_readRemoteSection(
    const PipeMemoryReader *Reader,
    swift_remote_reflection_section_t *RemoteSection, void **outFreeContext) {

  const void *Buffer =
      PipeMemoryReader_readBytes((void *)Reader, RemoteSection->StartAddress,
                                 RemoteSection->Size, outFreeContext);
  if (!Buffer)
    errorAndExit("Couldn't read reflection information");

  return Buffer;
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

  swift_remote_reflection_section_t *RemoteSections =
      calloc(NumReflectionInfos * SECTIONS_PER_INFO,
             sizeof(swift_remote_reflection_section_t));
  if (RemoteSections == NULL)
    errnoAndExit("malloc failed");

  // We first read all remote reflection sections, there are 6 for every
  // complete reflection info.
  // They come ordered as: fieldmd, assocty, builtin, capture, typeref, reflstr.
  for (size_t i = 0; i < NumReflectionInfos * SECTIONS_PER_INFO; ++i) {
    RemoteSections[i] = makeRemoteSection(Reader);
  }
  // Now pull in the remote sections into our address space.
  swift_reflection_section_t *LocalSections =
      calloc(NumReflectionInfos * SECTIONS_PER_INFO,
             sizeof(swift_reflection_section_t));
  for (size_t i = 0; i < NumReflectionInfos * SECTIONS_PER_INFO; ++i) {
    void *outFreeContext = NULL;
    const void *Buffer = PipeMemoryReader_readRemoteSection(
        (void *)Reader, &RemoteSections[i], &outFreeContext);
    LocalSections[i] = makeLocalSection(Buffer, RemoteSections[i]);
  }

  // Finally, we zip them in a complete reflection info, with a stride of 6.
  for (size_t i = 0; i < NumReflectionInfos * SECTIONS_PER_INFO;
       i += SECTIONS_PER_INFO) {
    swift_reflection_mapping_info_t Info =
        makeNonContiguousReflectionInfo(&RemoteSections[i], &LocalSections[i]);
    swift_reflection_addReflectionMappingInfo(RC, Info);
  }

  free(RemoteSections);
  free(LocalSections);
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
                       const PipeMemoryReader *Reader) {
  uintptr_t instance = PipeMemoryReader_receiveInstanceAddress(Reader);
  if (instance == 0) {
    // Child has no more instances to examine
    PipeMemoryReader_sendDoneMessage(Reader);
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

  PipeMemoryReader_sendDoneMessage(Reader);
  return 1;
}

int reflectExistentialImpl(
    SwiftReflectionContextRef RC, const PipeMemoryReader *Reader,
    swift_typeref_t MockExistentialTR,
    int (*ProjectExistentialFn)(SwiftReflectionContextRef, swift_addr_t,
                                swift_typeref_t, swift_typeref_t *,
                                swift_addr_t *)) {
  uintptr_t instance = PipeMemoryReader_receiveInstanceAddress(Reader);
  if (instance == 0) {
    // Child has no more instances to examine
    PipeMemoryReader_sendDoneMessage(Reader);
    return 0;
  }
  printf("Instance pointer in child address space: 0x%lx\n", instance);

  swift_typeref_t InstanceTypeRef;
  swift_addr_t StartOfInstanceData = 0;

  if (!ProjectExistentialFn(RC, instance, MockExistentialTR, &InstanceTypeRef,
                            &StartOfInstanceData)) {
    printf("swift_reflection_projectExistential failed.\n");
    PipeMemoryReader_sendDoneMessage(Reader);
    return 0;
  }

  printf("Type reference:\n");
  swift_reflection_dumpTypeRef(InstanceTypeRef);
  printf("\n");

  printf("Type info:\n");
  swift_reflection_dumpInfoForTypeRef(RC, InstanceTypeRef);
  printf("\n");

  printf("Start of instance data: 0x%" PRIx64 "\n", StartOfInstanceData);
  printf("\n");

  PipeMemoryReader_sendDoneMessage(Reader);
  return 1;
}

int reflectExistential(SwiftReflectionContextRef RC,
                       const PipeMemoryReader *Reader,
                       swift_typeref_t MockExistentialTR) {
  return reflectExistentialImpl(RC, Reader, MockExistentialTR,
                                swift_reflection_projectExistential);
}

int reflectExistentialAndUnwrapClass(SwiftReflectionContextRef RC,
                                     const PipeMemoryReader *Reader,
                                     swift_typeref_t MockExistentialTR) {
  return reflectExistentialImpl(
      RC, Reader, MockExistentialTR,
      swift_reflection_projectExistentialAndUnwrapClass);
}

int reflectEnum(SwiftReflectionContextRef RC,
                const PipeMemoryReader *Reader) {
  static const char Name[] = MANGLING_PREFIX_STR "ypD";
  swift_typeref_t AnyTR
    = swift_reflection_typeRefForMangledTypeName(
      RC, Name, sizeof(Name)-1);

  uintptr_t AnyInstance = PipeMemoryReader_receiveInstanceAddress(Reader);
  if (AnyInstance == 0) {
    // Child has no more instances to examine
    PipeMemoryReader_sendDoneMessage(Reader);
    return 0;
  }
  swift_typeref_t EnumTypeRef;
  swift_addr_t EnumInstance = 0;
  if (!swift_reflection_projectExistential(RC, AnyInstance, AnyTR,
                                           &EnumTypeRef,
                                           &EnumInstance)) {
    printf("swift_reflection_projectExistential failed.\n");
    PipeMemoryReader_sendDoneMessage(Reader);
    return 0;
  }

  printf("Instance pointer in child address space: 0x%lx\n",
         (uintptr_t)EnumInstance);

  printf("Type reference:\n");
  swift_reflection_dumpTypeRef(EnumTypeRef);
  printf("\n");

  printf("Type info:\n");
  swift_reflection_dumpInfoForTypeRef(RC, EnumTypeRef);
  printf("\n");

  printf("Enum value:\n");
  swift_typeinfo_t InstanceTypeInfo = swift_reflection_infoForTypeRef(RC, EnumTypeRef);
  if (InstanceTypeInfo.Kind != SWIFT_NO_PAYLOAD_ENUM
      && InstanceTypeInfo.Kind != SWIFT_SINGLE_PAYLOAD_ENUM
      && InstanceTypeInfo.Kind != SWIFT_MULTI_PAYLOAD_ENUM) {
    // Enums with a single payload case and no non-payload cases
    // can get rewritten by the compiler to just the payload
    // type.
    swift_reflection_dumpInfoForTypeRef(RC, EnumTypeRef);
    PipeMemoryReader_sendDoneMessage(Reader);
    return 1;
  }

  int CaseIndex;
  if (!swift_reflection_projectEnumValue(RC, EnumInstance, EnumTypeRef, &CaseIndex)) {
    printf("swift_reflection_projectEnumValue failed.\n\n");
    PipeMemoryReader_sendDoneMessage(Reader);
    return 1; // <<< Test cases also verify failures, so this must "succeed"
  }
  if ((unsigned)CaseIndex > InstanceTypeInfo.NumFields) {
    printf("swift_reflection_projectEnumValue returned invalid case.\n\n");
    PipeMemoryReader_sendDoneMessage(Reader);
    return 0;
  }

  swift_childinfo_t CaseInfo
    = swift_reflection_childOfTypeRef(RC, EnumTypeRef, CaseIndex);

  if (CaseInfo.TR == 0) {
    // Enum case has no payload
    printf("(enum_value name=%s index=%llu)\n",
           CaseInfo.Name, (unsigned long long)CaseIndex);
  } else {
    printf("(enum_value name=%s index=%llu\n",
           CaseInfo.Name, (unsigned long long)CaseIndex);
    swift_reflection_dumpTypeRef(CaseInfo.TR);
    printf(")\n");
  }
  printf("\n");
  PipeMemoryReader_sendDoneMessage(Reader);
  return 1;
}

int reflectEnumValue(SwiftReflectionContextRef RC,
                     const PipeMemoryReader *Reader) {
  static const char Name[] = MANGLING_PREFIX_STR "ypD";
  swift_typeref_t AnyTR
    = swift_reflection_typeRefForMangledTypeName(
      RC, Name, sizeof(Name)-1);

  uintptr_t AnyInstance = PipeMemoryReader_receiveInstanceAddress(Reader);
  if (AnyInstance == 0) {
    // Child has no more instances to examine
    PipeMemoryReader_sendDoneMessage(Reader);
    return 0;
  }
  swift_typeref_t EnumTypeRef;
  swift_addr_t EnumInstance = 0;
  if (!swift_reflection_projectExistential(RC, AnyInstance, AnyTR,
                                           &EnumTypeRef,
                                           &EnumInstance)) {
    printf("swift_reflection_projectExistential failed.\n");
    PipeMemoryReader_sendDoneMessage(Reader);
    return 0;
  }

  printf("Type reference:\n");
  swift_reflection_dumpTypeRef(EnumTypeRef);

  printf("Value: ");
  int parens = 0;
  // Walk into successively nested enum types...
  while (EnumTypeRef != 0) {
    swift_typeinfo_t EnumTypeInfo = swift_reflection_infoForTypeRef(RC, EnumTypeRef);
    switch (EnumTypeInfo.Kind) {
    case SWIFT_NO_PAYLOAD_ENUM:
    case SWIFT_SINGLE_PAYLOAD_ENUM:
    case SWIFT_MULTI_PAYLOAD_ENUM:
    {
      int CaseIndex;
      if (!swift_reflection_projectEnumValue(RC, EnumInstance, EnumTypeRef, &CaseIndex)) {
        printf("swift_reflection_projectEnumValue failed.\n\n");
        PipeMemoryReader_sendDoneMessage(Reader);
        return 1; // <<< Test cases rely on detecting this, so must "succeed"
      }
      if ((unsigned)CaseIndex > EnumTypeInfo.NumFields) {
        printf("swift_reflection_projectEnumValue returned invalid case.\n\n");
        PipeMemoryReader_sendDoneMessage(Reader);
        return 0;
      }
      swift_childinfo_t CaseInfo
        = swift_reflection_childOfTypeRef(RC, EnumTypeRef, CaseIndex);
      printf(".%s", CaseInfo.Name);

      if (EnumTypeInfo.Kind == SWIFT_NO_PAYLOAD_ENUM || CaseInfo.TR == 0) {
        // No payload here, so end the walk
        EnumTypeRef = 0;
      } else {
        // There's a payload!
        printf("(");
        parens += 1;
        EnumTypeRef = CaseInfo.TR; // Walk into payload to see if it's an enum

        if (CaseInfo.Kind == SWIFT_STRONG_REFERENCE) { // Maybe an indirect enum?
          // Get the pointer value from the target
          void *outFreeContext = NULL;
          // !!! FIXME !!! obtain the pointer by properly projecting the enum value
          // Next lines are a hack to prove the concept.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wcast-qual"
          const void *rawPtr
            = PipeMemoryReader_readBytes((void *)Reader, EnumInstance, 8, &outFreeContext);
          uintptr_t instance = *(uintptr_t *)rawPtr & 0xffffffffffffff8ULL;
          PipeMemoryReader_freeBytes((void *)Reader, rawPtr, outFreeContext);
#pragma clang diagnostic pop

          // Indirect enum stores the payload as the first field of a closure context
          swift_typeinfo_t TI = swift_reflection_infoForInstance(RC, instance);
          if (TI.Kind == SWIFT_CLOSURE_CONTEXT) {
            // Yep, it's an indirect enum.  Let's follow the pointer...
            // TODO: Could we get here if we have an enum whose payload is a closure?
            swift_childinfo_t CaseInfo
              = swift_reflection_childOfInstance(RC, instance, 0);
            if (CaseInfo.Kind == SWIFT_NO_PAYLOAD_ENUM
                || CaseInfo.Kind == SWIFT_SINGLE_PAYLOAD_ENUM
                || CaseInfo.Kind == SWIFT_MULTI_PAYLOAD_ENUM) {
              // Found the indirect enum storage, loop to print it out.
              EnumTypeRef = CaseInfo.TR;
              EnumInstance = instance + CaseInfo.Offset;
              break;
            }
          }
        }
      }
      break;
    }
    default:
    {
      EnumTypeRef = 0;
      if (parens == 0) {
        printf(".??"); // Enum was optimized away, print "something"
      } else {
        printf("_");
      }
      break;
    }
    }
  }
  for (int i = 0; i < parens; ++i) {
    printf(")");
  }
  printf("\n\n");
  PipeMemoryReader_sendDoneMessage(Reader);
  return 1;

}

static int reflectAsyncTaskInstance(SwiftReflectionContextRef RC,
                                    uintptr_t AsyncTaskInstance,
                                    const PipeMemoryReader *Reader,
                                    unsigned indentLevel) {
  indented_printf(indentLevel, "Async task %#" PRIx64 "\n",
                  (uint64_t)AsyncTaskInstance);

  swift_async_task_info_t TaskInfo =
      swift_reflection_asyncTaskInfo(RC, AsyncTaskInstance);
  if (TaskInfo.Error) {
    printf("swift_reflection_asyncTaskInfo failed: %s\n", TaskInfo.Error);
  } else {
    indented_printf(indentLevel, "id %" PRIu64 "\n", TaskInfo.Id);
    indented_printf(indentLevel, "enqueuePriority %u\n",
                    TaskInfo.EnqueuePriority);
    if (TaskInfo.ChildTaskCount > 0) {
      indented_printf(indentLevel, "children = {\n");

      // The memory for ChildTasks is only valid until the next Remote Mirror
      // call, so we need to copy it.
      swift_reflection_ptr_t *ChildTasks =
          calloc(TaskInfo.ChildTaskCount, sizeof(swift_reflection_ptr_t));
      memcpy(ChildTasks, TaskInfo.ChildTasks,
             TaskInfo.ChildTaskCount * sizeof(swift_reflection_ptr_t));

      for (unsigned i = 0; i < TaskInfo.ChildTaskCount; i++)
        reflectAsyncTaskInstance(RC, ChildTasks[i], Reader, indentLevel + 1);

      free(ChildTasks);
      indented_printf(indentLevel, "}\n");
    } else {
      indented_printf(indentLevel, "children = {}\n");
    }
  }

  swift_async_task_slab_return_t SlabPtrResult =
      swift_reflection_asyncTaskSlabPointer(RC, AsyncTaskInstance);
  if (SlabPtrResult.Error) {
    printf("swift_reflection_asyncTaskSlabPointer failed: %s\n",
           SlabPtrResult.Error);
  } else {
    swift_reflection_ptr_t SlabPtr = SlabPtrResult.SlabPtr;
    while (SlabPtr) {
      indented_printf(indentLevel, "  Slab pointer %#" PRIx64 "\n",
                      (uint64_t)SlabPtr);
      swift_async_task_slab_allocations_return_t AllocationsResult =
          swift_reflection_asyncTaskSlabAllocations(RC, SlabPtr);
      if (AllocationsResult.Error) {
        indented_printf(
            indentLevel,
            "swift_reflection_asyncTaskSlabAllocations failed: %s\n",
            AllocationsResult.Error);
        SlabPtr = 0;
      } else {
        indented_printf(indentLevel, "    Slab size %" PRIu64 "\n",
                        (uint64_t)AllocationsResult.SlabSize);
        for (unsigned i = 0; i < AllocationsResult.ChunkCount; i++) {
          swift_async_task_allocation_chunk_t Chunk =
              AllocationsResult.Chunks[i];
          indented_printf(indentLevel,
                          "    Chunk at %#" PRIx64 " length %u kind %u\n",
                          (uint64_t)Chunk.Start, Chunk.Length, Chunk.Kind);
        }
        SlabPtr = AllocationsResult.NextSlab;
      }
    }
  }

  if (indentLevel == 0) {
    printf("\n\n");
  }
  fflush(stdout);
  return 1;
}

int reflectAsyncTask(SwiftReflectionContextRef RC,
                     const PipeMemoryReader *Reader) {
  uintptr_t AsyncTaskInstance = PipeMemoryReader_receiveInstanceAddress(Reader);
  int result = reflectAsyncTaskInstance(RC, AsyncTaskInstance, Reader, 0);
  PipeMemoryReader_sendDoneMessage(Reader);
  return result;
}

int logString(SwiftReflectionContextRef RC, const PipeMemoryReader *Reader) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wcast-qual"
  void *Context = (void *)Reader;
#pragma clang diagnostic pop

  swift_addr_t StringPointer = PipeMemoryReader_receiveInstanceAddress(Context);
  uint64_t StringLength =
      PipeMemoryReader_getStringLength(Context, StringPointer);

  void *FreeContext;
  // Read length+1 bytes to get the NUL terminator too.
  const void *String = PipeMemoryReader_readBytes(
      Context, StringPointer, StringLength + 1, &FreeContext);

  printf("%s\n", (const char *)String);
  PipeMemoryReader_freeBytes(Context, String, FreeContext);

  PipeMemoryReader_sendDoneMessage(Context);
  return 1;
}

int doDumpHeapInstance(const char *BinaryFilename, PipeMemoryReader *Reader) {
#if defined(_WIN32)
#else
  pid_t pid = _fork();
  switch (pid) {
    case -1:
      errnoAndExit("Couldn't fork child process");
    case 0: { // Child:
      close(PipeMemoryReader_getParentWriteFD(Reader));
      close(PipeMemoryReader_getParentReadFD(Reader));
      dup2(PipeMemoryReader_getChildReadFD(Reader), STDIN_FILENO);
      dup2(PipeMemoryReader_getChildWriteFD(Reader), STDOUT_FILENO);

      char *const argv[] = {strdup(BinaryFilename), NULL};
      int r = _execv(BinaryFilename, argv);
      int status = EXIT_SUCCESS;
      if (r == -1) {
        perror("child process");
        status = EXIT_FAILURE;
      }
      exit(status);
    }
    default: { // Parent
      close(PipeMemoryReader_getChildReadFD(Reader));
      close(PipeMemoryReader_getChildWriteFD(Reader));
      SwiftReflectionContextRef RC =
          swift_reflection_createReflectionContextWithDataLayout(
              (void *)Reader, PipeMemoryReader_queryDataLayout,
              PipeMemoryReader_freeBytes, PipeMemoryReader_readBytes,
              PipeMemoryReader_getStringLength,
              PipeMemoryReader_getSymbolAddress);

      uint8_t PointerSize = PipeMemoryReader_getPointerSize((void*)Reader);
      if (PointerSize != sizeof(uintptr_t))
        errorAndExit("Child process had unexpected architecture");

#if defined(__APPLE__) && defined(__MACH__)
      PipeMemoryReader_receiveImages(RC, Reader);
#else
      PipeMemoryReader_receiveReflectionInfo(RC, Reader);
#endif

      while (1) {
	// Flush the cache between every reflection operation
	flushPipeMemoryReader(Reader);
        InstanceKind Kind = PipeMemoryReader_receiveInstanceKind(Reader);
        switch (Kind) {
        case Object:
          printf("Reflecting an object.\n");
          if (!reflectHeapObject(RC, Reader))
            return EXIT_SUCCESS;
          break;
        case Existential: {
          static const char Name[] = MANGLING_PREFIX_STR "ypD";
          swift_typeref_t AnyTR = swift_reflection_typeRefForMangledTypeName(
              RC, Name, sizeof(Name) - 1);
          uint8_t ShouldUnwrap =
              PipeMemoryReader_receiveShouldUnwrapExistential(Reader);

          if (ShouldUnwrap) {
            printf("Reflecting an existential and unwrapping class.\n");
            if (!reflectExistentialAndUnwrapClass(RC, Reader, AnyTR))
              return EXIT_SUCCESS;
          } else {
            printf("Reflecting an existential.\n");
            if (!reflectExistential(RC, Reader, AnyTR))
              return EXIT_SUCCESS;
          }
          break;
        }
        case ErrorExistential: {
          static const char ErrorName[] = MANGLING_PREFIX_STR "s5Error_pD";
          swift_typeref_t ErrorTR = swift_reflection_typeRefForMangledTypeName(
              RC, ErrorName, sizeof(ErrorName) - 1);
          uint8_t ShouldUnwrap =
              PipeMemoryReader_receiveShouldUnwrapExistential(Reader);

          if (ShouldUnwrap) {
            printf("Reflecting an error existential and unwrapping class.\n");
            if (!reflectExistentialAndUnwrapClass(RC, Reader, ErrorTR))
              return EXIT_SUCCESS;
          } else {
            printf("Reflecting an error existential.\n");
            if (!reflectExistential(RC, Reader, ErrorTR))
              return EXIT_SUCCESS;
          }
          break;
        }
        case Closure:
          printf("Reflecting a closure.\n");
          if (!reflectHeapObject(RC, Reader))
            return EXIT_SUCCESS;
          break;
        case Enum: {
          printf("Reflecting an enum.\n");
          if (!reflectEnum(RC, Reader))
            return EXIT_SUCCESS;
          break;
        }
        case EnumValue: {
          printf("Reflecting an enum value.\n");
          if (!reflectEnumValue(RC, Reader))
            return EXIT_SUCCESS;
          break;
        }
        case AsyncTask: {
          printf("Reflecting an async task.\n");
          if (!reflectAsyncTask(RC, Reader))
            return EXIT_SUCCESS;
          break;
        }
        case LogString: {
          if (!logString(RC, Reader))
            return EXIT_SUCCESS;
          break;
        }
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

  PipeMemoryReader Pipe = createPipeMemoryReader();
  int ret = doDumpHeapInstance(BinaryFilename, &Pipe);
  destroyPipeMemoryReader(&Pipe);

  return ret;
}
