//===--- Darwin.h - Darwin specifics ----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Darwin specifics.
//
//  WARNING: Some of the things in this file are SPI.  If you use them in
//  your own code, we will mercilessly break your program for you and hand
//  you the pieces :-)
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BACKTRACING_DARWIN_H
#define SWIFT_BACKTRACING_DARWIN_H
#ifdef __APPLE__

#ifdef __cplusplus
extern "C" {
#endif

#include <mach/mach.h>

#include <libproc.h>
#include <stdint.h>

#include <CoreFoundation/CoreFoundation.h>

// .. Mach fixes ...............................................................

// Use an inline function for mach_task_self() or it won't import into Swift
#undef mach_task_self
static inline task_t mach_task_self() { return mach_task_self_; }

// .. Thread states ............................................................

/* We can't import these from the system header, because it uses all kinds of
   macros and the Swift importer can't cope with that.  So declare them here
   in a form it can understand. */
#define ARM_THREAD_STATE64 6
struct darwin_arm64_thread_state {
  uint64_t _x[29];
  uint64_t fp;
  uint64_t lr;
  uint64_t sp;
  uint64_t pc;
  uint32_t cpsr;
  uint32_t __pad;
};

struct darwin_arm64_exception_state {
  uint64_t far;
  uint32_t esr;
  uint32_t exception;
};

struct darwin_arm64_mcontext {
  struct darwin_arm64_exception_state es;
  struct darwin_arm64_thread_state    ss;
  // followed by NEON state (which we don't care about)
};

#define x86_THREAD_STATE64 4
struct darwin_x86_64_thread_state {
  uint64_t rax;
  uint64_t rbx;
  uint64_t rcx;
  uint64_t rdx;
  uint64_t rdi;
  uint64_t rsi;
  uint64_t rbp;
  uint64_t rsp;
  uint64_t r8;
  uint64_t r9;
  uint64_t r10;
  uint64_t r11;
  uint64_t r12;
  uint64_t r13;
  uint64_t r14;
  uint64_t r15;
  uint64_t rip;
  uint64_t rflags;
  uint64_t cs;
  uint64_t fs;
  uint64_t gs;
};

struct darwin_x86_64_exception_state {
  uint16_t trapno;
  uint16_t cpu;
  uint32_t err;
  uint64_t faultvaddr;
};

struct darwin_x86_64_mcontext {
  struct darwin_x86_64_exception_state es;
  struct darwin_x86_64_thread_state    ss;
  // followed by FP/AVX/AVX512 state (which we don't care about)
};

// .. dyld SPI .................................................................

struct dyld_process_cache_info {
  __swift_uuid_t cacheUUID;
  __swift_uint64_t       cacheBaseAddress;
  __swift_bool   noCache;
  __swift_bool   privateCache;
};
typedef struct dyld_process_cache_info dyld_process_cache_info;
typedef const struct dyld_process_info_base* dyld_process_info;

extern dyld_process_info _dyld_process_info_create(__swift_task_t task, __swift_uint64_t timestamp, __swift_kern_return_t* kernelError);
extern void  _dyld_process_info_release(dyld_process_info info);
extern void  _dyld_process_info_retain(dyld_process_info info);
extern void  _dyld_process_info_get_cache(dyld_process_info info, dyld_process_cache_info* cacheInfo);
extern void _dyld_process_info_for_each_image(dyld_process_info info, void (^callback)(__swift_uint64_t machHeaderAddress, const __swift_uuid_t uuid, const char* path));
extern void _dyld_process_info_for_each_segment(dyld_process_info info, __swift_uint64_t machHeaderAddress, void (^callback)(__swift_uint64_t segmentAddress, __swift_uint64_t segmentSize, const char* segmentName));

// .. CoreSymbolication SPI ....................................................

typedef int32_t cpu_type_t;
typedef int32_t cpu_subtype_t;

struct _CSArchitecture {
  cpu_type_t	cpu_type;
  cpu_subtype_t	cpu_subtype;
};

typedef struct _CSArchitecture CSArchitecture;

#define CPU_ARCH_ABI64          0x01000000      /* 64 bit ABI */
#define CPU_ARCH_ABI64_32       0x02000000      /* ABI for 64-bit hardware with 32-bit types; LP32 */

#define CPU_TYPE_X86            ((cpu_type_t) 7)
#define CPU_TYPE_I386           CPU_TYPE_X86            /* compatibility */

#define CPU_SUBTYPE_INTEL(f, m) ((cpu_subtype_t) (f) + ((m) << 4))
#define CPU_SUBTYPE_I386_ALL    CPU_SUBTYPE_INTEL(3, 0)

#define CPU_TYPE_ARM            ((cpu_type_t) 12)

#define CPU_SUBTYPE_ARM64_ALL   ((cpu_subtype_t) 0)
#define CPU_SUBTYPE_ARM_V7K     ((cpu_subtype_t) 12)

static const CSArchitecture kCSArchitectureI386 = { CPU_TYPE_I386, CPU_SUBTYPE_I386_ALL };
static const CSArchitecture kCSArchitectureX86_64 = { CPU_TYPE_I386|CPU_ARCH_ABI64, CPU_SUBTYPE_I386_ALL };
static const CSArchitecture kCSArchitectureArm64 =   { CPU_TYPE_ARM | CPU_ARCH_ABI64, CPU_SUBTYPE_ARM64_ALL };
static const CSArchitecture kCSArchitectureArm64_32 =   { CPU_TYPE_ARM | CPU_ARCH_ABI64_32, CPU_SUBTYPE_ARM64_ALL };
static const CSArchitecture kCSArchitectureArmV7K =  { CPU_TYPE_ARM, CPU_SUBTYPE_ARM_V7K };

typedef struct _CSBinaryRelocationInformation {
  vm_address_t base;
  vm_address_t extent;
  char name[17];
} CSBinaryRelocationInformation;

typedef struct _CSBinaryImageInformation {
  vm_address_t base;
  vm_address_t extent;
  backtrace_CFUUIDBytes uuid;
  CSArchitecture arch;
  const char *path;
  CSBinaryRelocationInformation *relocations;
  uint32_t relocationCount;
  uint32_t flags;
} CSBinaryImageInformation;

typedef uint64_t CSMachineTime;

static const CSMachineTime kCSBeginningOfTime = 0;
static const CSMachineTime kCSEndOfTime = (1ull<<63) - 1;
static const CSMachineTime kCSNow = (1ull<<63);
static const CSMachineTime kCSAllTimes = (1ull<<63) + 1;

struct _CSTypeRef {
  uintptr_t _opaque_1;
  uintptr_t _opaque_2;
};

typedef struct _CSTypeRef CSTypeRef;

typedef CSTypeRef CSNullRef;
typedef CSTypeRef CSSymbolicatorRef;
typedef CSTypeRef CSSymbolOwnerRef;
typedef CSTypeRef CSSymbolRef;
typedef CSTypeRef CSSourceInfoRef;

static const CSNullRef kCSNull = { 0, 0 };

typedef void (^CSSymbolOwnerIterator)(CSSymbolOwnerRef owner);
typedef void (^CSStackFrameIterator)(CSSymbolRef symbol, CSSourceInfoRef info);

typedef struct _CSNotificationData {
    CSSymbolicatorRef symbolicator;
    union {
        struct Ping {
            uint32_t value;
        } ping;

        struct DyldLoad {
            CSSymbolOwnerRef symbolOwner;
        } dyldLoad;

        struct DyldUnload {
            CSSymbolOwnerRef symbolOwner;
        } dyldUnload;
    } u;
} CSNotificationData;

typedef void (^CSNotificationBlock)(uint32_t type, CSNotificationData data);

struct _CSRange {
  vm_address_t	location;
  vm_size_t	length;
};

typedef struct _CSRange CSRange;

enum {
  kCRSanitizePathGlobLocalHomeDirectories = 1,
  kCRSanitizePathGlobLocalVolumes = 2,
  kCRSanitizePathGlobAllTypes = 0xff,

  kCRSanitizePathNormalize = 0x100 << 0,
  kCRSanitizePathKeepFile = 0x100 << 1,
};

#ifdef __cplusplus
} // extern "C"
#endif

#endif // __APPLE__
#endif // SWIFT_BACKTRACING_DARWIN_H

