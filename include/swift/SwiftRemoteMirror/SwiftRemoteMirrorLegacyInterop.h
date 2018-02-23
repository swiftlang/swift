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

typedef struct swift_metadata_interop {
  uintptr_t Metadata;
  int Library;
} swift_metadata_interop_t;

typedef struct swift_typeref_interop {
  swift_typeref_t Typeref;
  int Library;
} swift_typeref_interop_t;

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
  
  int (*ownsObject)(SwiftReflectionContextRef ContextRef, uintptr_t Object);
  
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

struct SwiftReflectionInteropContextLibrary {
  SwiftReflectionContextRef Context;
  struct SwiftReflectionFunctions Functions;
  int IsLegacy;
};

struct SwiftReflectionInteropContext {
  void *ReaderContext;
  ReadBytesFunction ReadBytes;
  
  // Currently we support at most two libraries.
  struct SwiftReflectionInteropContextLibrary Libraries[2];
  int LibraryCount;
};

typedef SwiftReflectionInteropContext *SwiftReflectionInteropContextRef;

/// A stub for 'ownsObject' that always returns true. Used for the legacy library.
static inline int swift_reflection_interop_ownsObjectStub(
  SwiftReflectionContextRef ContextRef, uintptr_t Object) {
  return 1;
}

static inline void swift_reflection_interop_loadFunctions(
  struct SwiftReflectionInteropContext *Context, void *Handle, int IsLegacy) {
#ifndef __cplusplus
#define decltype(x) void *
#endif

  if (Handle == NULL)
    return;

  struct SwiftReflectionFunctions *Functions = &Context
    ->Libraries[Context->LibraryCount].Functions;

#define LOAD_NAMED(field, symbol) do { \
    Functions->field = (decltype(Functions->field))dlsym(Handle, symbol); \
    if (Functions->field == NULL) return; \
  } while (0)
#define LOAD(name) LOAD_NAMED(name, "swift_reflection_" #name)
  
  if (IsLegacy) {
    LOAD_NAMED(createReflectionContextLegacy, "swift_reflection_createReflectionContext");
    LOAD_NAMED(addReflectionInfoLegacy, "swift_reflection_addReflectionInfo");
    Functions->ownsObject = swift_reflection_interop_ownsObjectStub;
  } else {
    LOAD(createReflectionContext);
    LOAD(addReflectionInfo);
    LOAD(addImage);
    LOAD(ownsObject);
  }
  
  LOAD(destroyReflectionContext);
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
  
  Context->LibraryCount++;
  
  return;
  
#undef LOAD
#undef LOAD_NAMED
#ifndef __cplusplus
#undef decltype
#endif
}

#define FOREACH_LIBRARY \
  for (struct SwiftReflectionInteropContextLibrary *Library = &ContextRef->Libraries[0]; \
       Library < &ContextRef->Libraries[ContextRef->LibraryCount]; \
       ++Library)
#define LIBRARY_INDEX (Library - ContextRef->Libraries)
#define DECLARE_LIBRARY(index) \
  struct SwiftReflectionInteropContextLibrary *Library = &ContextRef->Libraries[index]

static int swift_reflection_interop_readBytesAdapter(void *reader_context,
                                                     swift_addr_t address,
                                                     void *dest,
                                                     uint64_t size) {
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
  
  SwiftReflectionInteropContextRef ContextRef =
    (SwiftReflectionInteropContextRef)calloc(sizeof(*ContextRef), 1);
  
  swift_reflection_interop_loadFunctions(ContextRef, LibraryHandle, 0);
  swift_reflection_interop_loadFunctions(ContextRef, LegacyLibraryHandle, 1);
  
  if (ContextRef->LibraryCount == 0) {
    free(ContextRef);
    return NULL;
  }
  
  FOREACH_LIBRARY {
    if (Library->IsLegacy) {
      Library->Context = Library->Functions.createReflectionContextLegacy(
        ContextRef, getPointerSize, getSizeSize,
        swift_reflection_interop_readBytesAdapter, getStringLength, getSymbolAddress);
    } else {
      Library->Context = Library->Functions.createReflectionContext(ReaderContext,
      getPointerSize, getSizeSize, readBytes, getStringLength, getSymbolAddress);
    }
  }
  
  ContextRef->ReaderContext = ReaderContext;
  ContextRef->ReadBytes = readBytes;
  
  return ContextRef;
}

static inline void
swift_reflection_interop_destroyReflectionContext(
  SwiftReflectionInteropContextRef ContextRef) {
  FOREACH_LIBRARY {
    Library->Functions.destroyReflectionContext(Library->Context);
  }
  free(ContextRef);
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
swift_reflection_interop_addImageLegacy(
  SwiftReflectionInteropContextRef ContextRef,
  struct SwiftReflectionInteropContextLibrary *Library,
  swift_addr_t imageStart,
  uint64_t imageLength) {
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
  
  Library->Functions.addReflectionInfoLegacy(Library->Context, info);
  return 1;
}

/// Add reflection information from a loaded Swift image.
/// Returns true on success, false if the image's memory couldn't be read.
static inline int
swift_reflection_interop_addImage(SwiftReflectionInteropContextRef ContextRef,
                                  swift_addr_t imageStart, uint64_t imageLength) {
  FOREACH_LIBRARY {
    int Success;
    if (Library->IsLegacy) {
      Success = swift_reflection_interop_addImageLegacy(ContextRef,
                                                        Library,
                                                        imageStart,
                                                        imageLength);
    } else {
      Success = Library->Functions.addImage(Library->Context, imageStart, imageLength);
    }
    if (Success) {
      return 1;
    }
  }
  return 0;
}

static inline int
swift_reflection_interop_readIsaMask(SwiftReflectionInteropContextRef ContextRef,
                                     uintptr_t *outIsaMask) {
  FOREACH_LIBRARY {
    int Success = Library->Functions.readIsaMask(Library->Context, outIsaMask);
    if (Success)
      return 1;
  }
  return 0;
}

static inline swift_typeref_interop_t
swift_reflection_interop_typeRefForMetadata(SwiftReflectionInteropContextRef ContextRef,
                                            swift_metadata_interop_t Metadata) {
  DECLARE_LIBRARY(Metadata.Library);
  swift_typeref_interop_t Result;
  Result.Typeref = Library->Functions.
    typeRefForMetadata(Library->Context, Metadata.Metadata);
  Result.Library = Metadata.Library;
  return Result;
}

static inline swift_typeref_interop_t
swift_reflection_interop_typeRefForInstance(SwiftReflectionInteropContextRef ContextRef,
                                            uintptr_t Object) {
  swift_typeref_interop_t Result;
  FOREACH_LIBRARY {
    if (!Library->Functions.ownsObject(Library->Context, Object))
      continue;
    swift_typeref_t Typeref = Library->Functions.typeRefForInstance(Library->Context,
                                                                     Object);
    if (Typeref == 0)
      continue;
    
    Result.Typeref = Typeref;
    Result.Library = LIBRARY_INDEX;
    return Result;
  }
  
  Result.Typeref = 0;
  Result.Library = 0;
  return Result;
}

swift_typeref_interop_t
swift_reflection_interop_typeRefForMangledTypeName(
  SwiftReflectionInteropContextRef ContextRef,
  const char *MangledName,
  uint64_t Length) {
  swift_typeref_interop_t Result;
  FOREACH_LIBRARY {
    swift_typeref_t Typeref = Library->Functions.typeRefForMangledTypeName(
      Library->Context, MangledName, Length);
    if (Typeref == 0)
      continue;
    
    Result.Typeref = Typeref;
    Result.Library = LIBRARY_INDEX;
    return Result;
  }
  
  Result.Typeref = 0;
  Result.Library = 0;
  return Result;
}

swift_typeinfo_t
swift_reflection_interop_infoForTypeRef(SwiftReflectionInteropContextRef ContextRef,
                                        swift_typeref_interop_t OpaqueTypeRef) {
  DECLARE_LIBRARY(OpaqueTypeRef.Library);
  return Library->Functions.infoForTypeRef(Library->Context, OpaqueTypeRef.Typeref);
}

swift_childinfo_t
swift_reflection_interop_childOfTypeRef(SwiftReflectionInteropContextRef ContextRef,
                                       swift_typeref_interop_t OpaqueTypeRef,
                                        unsigned Index) {
  DECLARE_LIBRARY(OpaqueTypeRef.Library);
  return Library->Functions.childOfTypeRef(Library->Context,
                                           OpaqueTypeRef.Typeref,
                                           Index);
}

swift_typeinfo_t
swift_reflection_interop_infoForMetadata(SwiftReflectionInteropContextRef ContextRef,
                                        swift_metadata_interop_t Metadata) {
  DECLARE_LIBRARY(Metadata.Library);
  return Library->Functions.infoForMetadata(Library->Context, Metadata.Metadata);
}

swift_childinfo_t
swift_reflection_interop_childOfMetadata(SwiftReflectionInteropContextRef ContextRef,
                                        swift_metadata_interop_t Metadata,
                                        unsigned Index) {
  DECLARE_LIBRARY(Metadata.Library);
  return Library->Functions.childOfMetadata(Library->Context, Metadata.Metadata, Index);
}

swift_typeinfo_t
swift_reflection_interop_infoForInstance(SwiftReflectionInteropContextRef ContextRef,
                                         uintptr_t Object) {
  swift_typeinfo_t Result = {};
  FOREACH_LIBRARY {
    if (!Library->Functions.ownsObject(Library->Context, Object))
      continue;
    
    Result = Library->Functions.infoForInstance(Library->Context, Object);
    if (Result.Kind == SWIFT_UNKNOWN)
      continue;
    
    return Result;
  }
  
  Result.Kind = SWIFT_UNKNOWN;
  return Result;
}

swift_childinfo_t
swift_reflection_interop_childOfInstance(SwiftReflectionInteropContextRef ContextRef,
                                         uintptr_t Object,
                                         unsigned Index) {
  FOREACH_LIBRARY {
    if (!Library->Functions.ownsObject(Library->Context, Object))
      continue;
    
    return Library->Functions.childOfInstance(Library->Context, Object, Index);
  }
  
  swift_childinfo_t Result = {};
  Result.Kind = SWIFT_UNKNOWN;
  return Result;
}

unsigned
swift_reflection_interop_genericArgumentCountOfTypeRef(
  SwiftReflectionInteropContextRef ContextRef, swift_typeref_interop_t OpaqueTypeRef) {
  DECLARE_LIBRARY(OpaqueTypeRef.Library);
  return Library->Functions.genericArgumentCountOfTypeRef(OpaqueTypeRef.Typeref);
}

swift_typeref_interop_t
swift_reflection_interop_genericArgumentOfTypeRef(
  SwiftReflectionInteropContextRef ContextRef, swift_typeref_interop_t OpaqueTypeRef,
  unsigned Index) {
  DECLARE_LIBRARY(OpaqueTypeRef.Library);
  swift_typeref_interop_t Result;
  Result.Typeref = Library->Functions.genericArgumentOfTypeRef(OpaqueTypeRef.Typeref,
                                                               Index);
  Result.Library = OpaqueTypeRef.Library;
  return Result;
}

static inline int
swift_reflection_interop_projectExistential(SwiftReflectionInteropContextRef ContextRef,
                                            swift_addr_t ExistentialAddress,
                                            swift_typeref_interop_t ExistentialTypeRef,
                                            swift_typeref_interop_t *OutInstanceTypeRef,
                                            swift_addr_t *OutStartOfInstanceData) {
  DECLARE_LIBRARY(ExistentialTypeRef.Library);
  int Success = Library->Functions.projectExistential(Library->Context,
                                                      ExistentialAddress,
                                                      ExistentialTypeRef.Typeref,
                                                      &OutInstanceTypeRef->Typeref,
                                                      OutStartOfInstanceData);
  if (!Success)
    return 0;
  
  OutInstanceTypeRef->Library = ExistentialTypeRef.Library;
  return 1;
}

static inline void
swift_reflection_interop_dumpTypeRef(SwiftReflectionInteropContextRef ContextRef,
                                     swift_typeref_interop_t OpaqueTypeRef) {
  DECLARE_LIBRARY(OpaqueTypeRef.Library);
  Library->Functions.dumpTypeRef(OpaqueTypeRef.Typeref);
}

void swift_reflection_interop_dumpInfoForTypeRef(
  SwiftReflectionInteropContextRef ContextRef, swift_typeref_interop_t OpaqueTypeRef) {
  DECLARE_LIBRARY(OpaqueTypeRef.Library);
  Library->Functions.dumpInfoForTypeRef(Library->Context, OpaqueTypeRef.Typeref);
}

void swift_reflection_interop_dumpInfoForMetadata(SwiftReflectionInteropContextRef ContextRef,
                                                  swift_metadata_interop_t Metadata) {
  DECLARE_LIBRARY(Metadata.Library);
  Library->Functions.dumpInfoForMetadata(Library->Context, Metadata.Metadata);
}

void swift_reflection_interop_dumpInfoForInstance(SwiftReflectionInteropContextRef ContextRef,
                                                  uintptr_t Object) {
  FOREACH_LIBRARY {
    if (!Library->Functions.ownsObject(Library->Context, Object))
      continue;
    
    Library->Functions.dumpInfoForInstance(Library->Context, Object);
    return;
  }
}

size_t swift_reflection_interop_demangle(SwiftReflectionInteropContextRef ContextRef,
                                         const char *MangledName,
                                         size_t Length,
                                         char *OutDemangledName,
                                         size_t MaxLength) {
  FOREACH_LIBRARY {
    return Library->Functions.demangle(MangledName, Length, OutDemangledName, MaxLength);
  }
  return 0;
}

#undef FOREACH_LIBRARY
#undef LIBRARY_INDEX
#undef DECLARE_LIBRARY

#endif // __APPLE__

#endif // SWIFT_REMOTE_MIRROR_LEGACY_INTEROP_H

