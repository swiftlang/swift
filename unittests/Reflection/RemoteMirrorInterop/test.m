//===--- test.m - SwiftRemoteMirrorLegacyInterop test program. ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------------===//
///
/// \file
/// This is a test program that exercises the SwiftRemoteMirrorLegacyInterop header.
///
//===----------------------------------------------------------------------------===//

#import <dlfcn.h>
#import <Foundation/Foundation.h>
#import <mach/mach.h>
#import <mach-o/dyld.h>

#import "SwiftRemoteMirrorLegacyInterop.h"


void *Load(char *path) {
  void *Handle = dlopen(path, RTLD_LOCAL);
  if (Handle == NULL) {
    fprintf(stderr, "loading %s: %s\n", path, dlerror());
    exit(1);
  }
  return Handle;
}

void Free(void *reader_context, const void *bytes, void *context) {
  assert(reader_context == (void *)0xdeadbeef);
  assert(context == (void *)0xfeedface);
  free((void *)bytes);
}

const void *ReadBytes(void *context, swift_addr_t address, uint64_t size,
                      void **outFreeContext) {
  assert(context == (void *)0xdeadbeef);
  *outFreeContext = (void *)0xfeedface;

  void *Buffer = malloc(size);
  vm_size_t InOutSize = size;
  kern_return_t result = vm_read_overwrite(mach_task_self(), address, size, (vm_address_t)Buffer, &InOutSize);
  if (result != KERN_SUCCESS) abort();
  if (InOutSize != size) abort();
  return Buffer;
}

uint64_t GetStringLength(void *context, swift_addr_t address) {
  assert(context == (void *)0xdeadbeef);
  return strlen((char *)address);
}

swift_addr_t GetSymbolAddress(void *context, const char *name, uint64_t name_length) {
  assert(context == (void *)0xdeadbeef);
  return (swift_addr_t)dlsym(RTLD_DEFAULT, name);
}

int main(int argc, char **argv) {
  if (argc != 4) {
    fprintf(stderr, "usage: %s <libtestswift.dylib> <libswiftRemoteMirror4.dylib> "
                    "<libswiftRemoteMirror5.dylib>\n",
                    argv[0]);
    exit(1);
  }
  
  char *TestLibPath = argv[1];
  char *Mirror4Path = argv[2];
  char *Mirror5Path = argv[3];
  
  void *TestHandle = Load(TestLibPath);
  intptr_t (*Test)(void) = dlsym(TestHandle, "test");
  
  uintptr_t Obj = Test();
  
  void *Mirror4Handle = Mirror4Path[0] == '-' ? NULL : Load(Mirror4Path);
  void *Mirror5Handle = Mirror5Path[0] == '-' ? NULL : Load(Mirror5Path);
  SwiftReflectionInteropContextRef Context =
    swift_reflection_interop_createReflectionContext(
      (void *)0xdeadbeef,
      Mirror5Handle,
      Mirror4Handle,
      sizeof(void *),
      Free,
      ReadBytes,
      GetStringLength,
      GetSymbolAddress);
  if (Context == NULL) {
    fprintf(stderr, "Unable to create a reflection context!\n");
    exit(1);
  }
  
  uint32_t ImageCount = _dyld_image_count();
  for (uint32_t i = 0; i < ImageCount; i++) {
    swift_addr_t Image = (swift_addr_t)_dyld_get_image_header(i);
    swift_reflection_interop_addImage(Context, Image);
  }
  
  swift_typeref_interop_t Type = swift_reflection_interop_typeRefForInstance(Context, Obj);
  if (Type.Typeref != 0) {
    swift_typeinfo_t TypeInfo = swift_reflection_interop_infoForTypeRef(Context, Type);
    printf("Kind:%u Size:%u Alignment:%u Stride:%u NumFields:%u\n",
           TypeInfo.Kind, TypeInfo.Size, TypeInfo.Alignment, TypeInfo.Stride,
           TypeInfo.NumFields);
  } else {
    printf("Unknown typeref!\n");
  }
  
  uintptr_t Metadata = *(uintptr_t *)Obj;
  swift_metadata_interop_t LookedUp =
    swift_reflection_interop_lookupMetadata(Context, Metadata);
  printf("Original metadata: %p\n", (void *)Metadata);
  printf("Looked up metadata: Metadata=%p Library=%d\n",
         (void *)LookedUp.Metadata, LookedUp.Library);
  
  swift_typeinfo_t TypeInfo = swift_reflection_interop_infoForInstance(Context, Obj);
  if (TypeInfo.Kind != SWIFT_UNKNOWN) {
    printf("Kind:%u Size:%u Alignment:%u Stride:%u NumFields:%u\n",
           TypeInfo.Kind, TypeInfo.Size, TypeInfo.Alignment, TypeInfo.Stride,
           TypeInfo.NumFields);
  
    for (unsigned i = 0; i < TypeInfo.NumFields; ++i) {
      swift_childinfo_interop_t ChildInfo = swift_reflection_interop_childOfInstance(
        Context, Obj, i);
      printf("  [%u]: %s Offset:%u Kind:%u\n", i,
             ChildInfo.Name, ChildInfo.Offset, ChildInfo.Kind);
    }
  } else {
    printf("Unknown typeinfo!\n");
  }
  
  swift_reflection_interop_destroyReflectionContext(Context);
}
