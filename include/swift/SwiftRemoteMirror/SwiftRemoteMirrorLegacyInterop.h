//===--- SwiftRemoteMirrorLegacyInterop.h - Interop with legacy libs. -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------------===//
///
/// \file
/// This header provides an interface for using multiple versions of the Swift remote
/// mirror library to inspect processes built with different versions of Swift, or
/// processes where different libraries are built with different versions of Swift.
///
//===----------------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_MIRROR_LEGACY_INTEROP_H
#define SWIFT_REMOTE_MIRROR_LEGACY_INTEROP_H

#ifdef __APPLE__

#include "SwiftRemoteMirror.h"

#include <dlfcn.h>
#include <mach-o/getsect.h>

typedef struct swift_reflection_legacy_info {
  swift_reflection_section_t fieldmd;
  swift_reflection_section_t assocty;
  swift_reflection_section_t builtin;
  swift_reflection_section_t capture;
  swift_reflection_section_t typeref;
  swift_reflection_section_t reflstr;
  uintptr_t LocalStartAddress;
  uintptr_t RemoteStartAddress;
} swift_reflection_legacy_info_t;

typedef int (*ReadBytesFunctionLegacy)(void *reader_context, swift_addr_t address,
                                       void *dest, uint64_t size);

struct SwiftReflectionFunctions {
  SwiftReflectionContextRef (*createReflectionContext)(
    void *ReaderContext,
    PointerSizeFunction getPointerSize,
    SizeSizeFunction getSizeSize,
    ReadBytesFunction readBytes,
    GetStringLengthFunction getStringLength,
    GetSymbolAddressFunction getSymbolAddress);
  
  SwiftReflectionContextRef (*createReflectionContextLegacy)(
    void *ReaderContext,
    PointerSizeFunction getPointerSize,
    SizeSizeFunction getSizeSize,
    ReadBytesFunctionLegacy readBytes,
    GetStringLengthFunction getStringLength,
    GetSymbolAddressFunction getSymbolAddress);
  
  void (*destroyReflectionContext)(SwiftReflectionContextRef Context);
  
  void (*addReflectionInfo)(SwiftReflectionContextRef Context,
                            swift_reflection_info_t Info);

  void (*addReflectionInfoLegacy)(SwiftReflectionContextRef Context,
                                  swift_reflection_legacy_info_t Info);

  int (*addImage)(SwiftReflectionContextRef ContextRef,
                  swift_addr_t imageStart, uint64_t imageLength);

  int (*readIsaMask)(SwiftReflectionContextRef ContextRef, uintptr_t *outIsaMask);
  
  swift_typeref_t (*typeRefForMetadata)(SwiftReflectionContextRef ContextRef,
                                        uintptr_t Metadata);

  swift_typeref_t (*typeRefForInstance)(SwiftReflectionContextRef ContextRef,
                                        uintptr_t Object);

  swift_typeref_t (*typeRefForMangledTypeName)(SwiftReflectionContextRef ContextRef,
                                           const char *MangledName,
                                           uint64_t Length);

  swift_typeinfo_t (*infoForTypeRef)(SwiftReflectionContextRef ContextRef,
                                      swift_typeref_t OpaqueTypeRef);

  swift_childinfo_t (*childOfTypeRef)(SwiftReflectionContextRef ContextRef,
                                      swift_typeref_t OpaqueTypeRef,
                                      unsigned Index);

  swift_typeinfo_t (*infoForMetadata)(SwiftReflectionContextRef ContextRef,
                                      uintptr_t Metadata);

  swift_childinfo_t (*childOfMetadata)(SwiftReflectionContextRef ContextRef,
                                       uintptr_t Metadata,
                                       unsigned Index);
  swift_typeinfo_t (*infoForInstance)(SwiftReflectionContextRef ContextRef,
                                      uintptr_t Object);

  swift_childinfo_t (*childOfInstance)(SwiftReflectionContextRef ContextRef,
                                       uintptr_t Object,
                                       unsigned Index);

  unsigned (*genericArgumentCountOfTypeRef)(swift_typeref_t OpaqueTypeRef);

  swift_typeref_t (*genericArgumentOfTypeRef)(swift_typeref_t OpaqueTypeRef,
                                              unsigned Index);

  int (*projectExistential)(SwiftReflectionContextRef ContextRef,
                            swift_addr_t ExistentialAddress,
                            swift_typeref_t ExistentialTypeRef,
                            swift_typeref_t *OutInstanceTypeRef,
                            swift_addr_t *OutStartOfInstanceData);

  void (*dumpTypeRef)(swift_typeref_t OpaqueTypeRef);

  void (*dumpInfoForTypeRef)(SwiftReflectionContextRef ContextRef,
                             swift_typeref_t OpaqueTypeRef);

  void (*dumpInfoForMetadata)(SwiftReflectionContextRef ContextRef,
                              uintptr_t Metadata);

  void (*dumpInfoForInstance)(SwiftReflectionContextRef ContextRef,
                              uintptr_t Object);

  size_t (*demangle)(const char *MangledName,
                     size_t Length,
                     char *OutDemangledName,
                     size_t MaxLength);
};

struct SwiftReflectionInteropContext {
  SwiftReflectionContextRef Context;
  struct SwiftReflectionFunctions Functions;
  int HasFunctions;
  
  SwiftReflectionContextRef LegacyContext;
  struct SwiftReflectionFunctions LegacyFunctions;
  int HasLegacyFunctions;
  
  void *ReaderContext;
  ReadBytesFunction ReadBytes;
};

typedef SwiftReflectionInteropContext *SwiftReflectionInteropContextRef;

static inline int swift_reflection_interop_loadFunctions(
  struct SwiftReflectionFunctions *functions, void *Handle, int IsLegacy) {
#ifndef __cplusplus
#define decltype(x) void *
#endif

#define LOAD_NAMED(field, symbol) do { \
    functions->field = (decltype(functions->field))dlsym(Handle, symbol); \
    if (functions->field == NULL) return 0; \
  } while (0)
#define LOAD(name) LOAD_NAMED(name, "swift_reflection_" #name)
  
  if (IsLegacy)
    LOAD_NAMED(createReflectionContextLegacy, "swift_reflection_createReflectionContext");
  else
    LOAD(createReflectionContext);
  
  LOAD(destroyReflectionContext);
  if (!IsLegacy) {
    LOAD_NAMED(addReflectionInfoLegacy, "swift_reflection_addReflectionInfo");
  } else {
    LOAD(addReflectionInfo);
    LOAD(addImage);
  }
  LOAD(readIsaMask);
  LOAD(typeRefForMetadata);
  LOAD(typeRefForInstance);
  LOAD(typeRefForMangledTypeName);
  LOAD(infoForTypeRef);
  LOAD(childOfTypeRef);
  LOAD(infoForMetadata);
  LOAD(childOfMetadata);
  LOAD(infoForInstance);
  LOAD(childOfInstance);
  LOAD(genericArgumentCountOfTypeRef);
  LOAD(genericArgumentOfTypeRef);
  LOAD(projectExistential);
  LOAD(dumpTypeRef);
  LOAD(dumpInfoForTypeRef);
  
  return 1;
  
#undef LOAD
#undef LOAD_NAMED
#ifndef __cplusplus
#undef decltype
#endif
}

static int swift_reflection_interop_readBytesAdapter(void *reader_context,
  swift_addr_t address, void *dest, uint64_t size) {
  SwiftReflectionInteropContextRef Context =
    (SwiftReflectionInteropContextRef)reader_context;

  FreeBytesFunction FreeFunction;
  void *FreeContext;
  const void *ptr = Context->ReadBytes(Context->ReaderContext, address, size,
                                       &FreeFunction, &FreeContext);
  if (ptr == NULL)
    return 0;
  
  memcpy(dest, ptr, size);
  FreeFunction(ptr, FreeContext);
  return 1;
}

static inline SwiftReflectionInteropContextRef
swift_reflection_interop_createReflectionContext(
    void *ReaderContext,
    void *LibraryHandle,
    void *LegacyLibraryHandle,
    PointerSizeFunction getPointerSize,
    SizeSizeFunction getSizeSize,
    ReadBytesFunction readBytes,
    GetStringLengthFunction getStringLength,
    GetSymbolAddressFunction getSymbolAddress) {
  
  SwiftReflectionInteropContextRef Context =
    (SwiftReflectionInteropContextRef)calloc(sizeof(*Context), 1);
  
  Context->HasFunctions = swift_reflection_interop_loadFunctions(
    &Context->Functions, LibraryHandle, 0);
  Context->HasLegacyFunctions = swift_reflection_interop_loadFunctions(
    &Context->LegacyFunctions, LegacyLibraryHandle, 1);
  
  if (Context->HasFunctions) {
    Context->Context = Context->Functions.createReflectionContext(ReaderContext,
      getPointerSize, getSizeSize, readBytes, getStringLength, getSymbolAddress);
  }
  
  if (Context->HasLegacyFunctions) {
    Context->LegacyContext = Context->LegacyFunctions.createReflectionContextLegacy(
      Context, getPointerSize, getSizeSize, swift_reflection_interop_readBytesAdapter,
      getStringLength, getSymbolAddress);
  }
  
  Context->ReaderContext = ReaderContext;
  Context->ReadBytes = readBytes;
  
  return Context;
}

#ifndef __LP64__
typedef const struct mach_header MachHeader;
#else
typedef const struct mach_header_64 MachHeader;
#endif

static inline int
swift_reflection_interop_findSection(MachHeader *Header, const char *Name,
                                     swift_reflection_section_t *Sect) {
  unsigned long Size;
  void *Address = getsectiondata(Header, "__TEXT", Name, &Size);
  if (!Address)
    return 0;
  
  Sect->Begin = Address;
  Sect->End = (void *)((uintptr_t)Address + Size);
  return 1;
}

static inline int
swift_reflection_interop_addImageLegacy(SwiftReflectionInteropContextRef ContextRef,
                                        swift_addr_t imageStart, uint64_t imageLength) {
  if (imageLength < sizeof(MachHeader)) {
    return 0;
  }
  
  FreeBytesFunction FreeFunction;
  void *FreeContext;
  const void *Buf = ContextRef->ReadBytes(ContextRef->ReaderContext,
                                          imageStart,
                                          imageLength,
                                          &FreeFunction,
                                          &FreeContext);
  // TODO: free the stuff when we're done
  
  auto Header = (MachHeader *)Buf;
  if (Header->magic != MH_MAGIC && Header->magic != MH_MAGIC_64) {
    return 0;
  }
  
  swift_reflection_legacy_info_t info = {};
  
  int success = 0;
  success = swift_reflection_interop_findSection(Header,
                                                 "__swift3_fieldmd",
                                                 &info.fieldmd) || success;
  success = swift_reflection_interop_findSection(Header,
                                                 "__swift3_assocty",
                                                 &info.assocty) || success;
  success = swift_reflection_interop_findSection(Header,
                                                 "__swift3_builtin",
                                                 &info.builtin) || success;
  success = swift_reflection_interop_findSection(Header,
                                                 "__swift3_capture",
                                                 &info.capture) || success;
  success = swift_reflection_interop_findSection(Header,
                                                 "__swift3_typeref",
                                                 &info.typeref) || success;
  success = swift_reflection_interop_findSection(Header,
                                                 "__swift3_reflstr",
                                                 &info.reflstr) || success;
  
  if (!success)
    return 0;
  
  info.LocalStartAddress = (uintptr_t)Buf;
  info.RemoteStartAddress = imageStart;
  
  ContextRef->LegacyFunctions.addReflectionInfoLegacy(ContextRef->LegacyContext, info);
  return 1;
}

/// Add reflection information from a loaded Swift image.
/// Returns true on success, false if the image's memory couldn't be read.
static inline int
swift_reflection_interop_addImage(SwiftReflectionInteropContextRef ContextRef,
                                  swift_addr_t imageStart, uint64_t imageLength) {
  if (ContextRef->HasFunctions) {
    int Success = ContextRef->Functions.addImage(ContextRef->Context,
                                                 imageStart,
                                                 imageLength);
    if (Success)
      return 1;
  }
  
  if (ContextRef->HasLegacyFunctions) {
    int Success = swift_reflection_interop_addImageLegacy(ContextRef,
                                                          imageStart,
                                                          imageLength);
    if (Success)
      return 1;
  }
  
  return 0;
}

#endif // __APPLE__

#endif // SWIFT_REMOTE_MIRROR_LEGACY_INTEROP_H

