//===--- _SwiftBacktracing.h - Swift Backtracing Support --------*- C++ -*-===//
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
//  Defines types and support functions for the Swift backtracing code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BACKTRACING_H
#define SWIFT_BACKTRACING_H

#include <inttypes.h>

#ifdef __APPLE__
#include <TargetConditionals.h>
#endif

#if TARGET_OS_OSX
#include <mach/machine.h>
#include <mach/task.h>

#include <CoreFoundation/CFUUID.h>
#include <CoreFoundation/CFString.h>

#include <libproc.h>
#endif

#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

struct CrashInfo {
  uint64_t crashing_thread;
  uint64_t signal;
  uint64_t fault_address;
  uint64_t mctx;
};

// .. Processor specifics ......................................................

struct x86_64_gprs {
  uint64_t _r[16];
  uint64_t rflags;
  uint16_t cs, fs, gs, _pad0;
  uint64_t rip;
  uint64_t valid;
};

struct i386_gprs {
  uint32_t _r[8];
  uint32_t eflags;
  uint16_t segreg[6];
  uint32_t eip;
  uint32_t valid;
};

struct arm64_gprs {
  uint64_t _x[32];
  uint64_t pc;
  uint64_t valid;
};

struct arm_gprs {
  uint32_t _r[16];
  uint32_t valid;
};

// .. Darwin specifics .........................................................

#if TARGET_OS_OSX

/* Darwin thread states.  We can't import these from the system header because
   it uses all kinds of macros and the Swift importer can't cope with that.
   So declare them here in a form it can understand. */
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

/* DANGER!  These are SPI.  They may change (or vanish) at short notice, may
   not work how you expect, and are generally dangerous to use. */
// ###FIXME: Remove the 0
#if 0 && __has_include(<mach-o/dyld_process_info.h>)
  #include <mach-o/dyld_process_info.h>
#else
struct dyld_process_cache_info {
  uuid_t      cacheUUID;
  uint64_t    cacheBaseAddress;
  bool        noCache;
  bool        privateCache;
};
typedef struct dyld_process_cache_info dyld_process_cache_info;
typedef const struct dyld_process_info_base* dyld_process_info;

extern dyld_process_info _dyld_process_info_create(task_t task, uint64_t timestamp, kern_return_t* kernelError);
extern void  _dyld_process_info_release(dyld_process_info info);
extern void  _dyld_process_info_retain(dyld_process_info info);
extern void  _dyld_process_info_get_cache(dyld_process_info info, dyld_process_cache_info* cacheInfo);
extern void _dyld_process_info_for_each_image(dyld_process_info info, void (^callback)(uint64_t machHeaderAddress, const uuid_t uuid, const char* path));
extern void _dyld_process_info_for_each_segment(dyld_process_info info, uint64_t machHeaderAddress, void (^callback)(uint64_t segmentAddress, uint64_t segmentSize, const char* segmentName));
#endif

/* DANGER!  CoreSymbolication is a private framework.  This is all SPI. */
// ###FIXME: Remove the 0
struct _CSArchitecture {
    cpu_type_t		cpu_type;
    cpu_subtype_t	cpu_subtype;
};

typedef struct _CSArchitecture CSArchitecture;

static const CSArchitecture kCSArchitectureI386 = { CPU_TYPE_I386, CPU_SUBTYPE_I386_ALL };
static const CSArchitecture kCSArchitectureX86_64 = { CPU_TYPE_I386|CPU_ARCH_ABI64, CPU_SUBTYPE_I386_ALL };
static const CSArchitecture kCSArchitectureArm64 =   { CPU_TYPE_ARM | CPU_ARCH_ABI64, CPU_SUBTYPE_ARM64_ALL };
static const CSArchitecture kCSArchitectureArm64_32 =   { CPU_TYPE_ARM | CPU_ARCH_ABI64_32, CPU_SUBTYPE_ARM64_ALL };
static const CSArchitecture kCSArchitectureArmV7K =  { CPU_TYPE_ARM, CPU_SUBTYPE_ARM_V7K };

typedef struct _CSBinaryRelocationInformation {
  mach_vm_address_t base;
  mach_vm_address_t extent;
  char name[17];
} CSBinaryRelocationInformation;

typedef struct _CSBinaryImageInformation {
  mach_vm_address_t base;
  mach_vm_address_t extent;
  CFUUIDBytes uuid;
  CSArchitecture arch;
  const char *path;
  CSBinaryRelocationInformation *relocations;
  uint32_t relocationCount;
  uint32_t flags;
} CSBinaryImageInformation;

typedef uint64_t CSMachineTime;

static const CSMachineTime kCSBeginningOfTime = 0;
static const CSMachineTime kCSEndOfTime = INT64_MAX;
static const CSMachineTime kCSNow = INT64_MAX + 1ULL;
static const CSMachineTime kCSAllTimes = INT64_MAX + 2ULL;

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
  mach_vm_address_t	location;
  mach_vm_size_t	length;
};

typedef struct _CSRange CSRange;

/* DANGER! This is also SPI */
enum {
  kCRSanitizePathGlobLocalHomeDirectories = 1,
  kCRSanitizePathGlobLocalVolumes = 2,
  kCRSanitizePathGlobAllTypes = 0xff,

  kCRSanitizePathNormalize = 0x100 << 0,
  kCRSanitizePathKeepFile = 0x100 << 1,
};

#endif // TARGET_OS_OSX

#ifdef __cplusplus
} // extern "C"
} // namespace swift
#endif

#endif // SWIFT_BACKTRACING_H
