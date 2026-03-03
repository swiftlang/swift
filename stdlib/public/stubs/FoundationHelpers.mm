//===--- FoundationHelpers.mm - Cocoa framework helper shims --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains shims to refer to framework functions required by the
// standard library. The stdlib cannot directly import these modules without
// introducing circular dependencies.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP
#import <CoreFoundation/CoreFoundation.h>
#include "swift/shims/CoreFoundationShims.h"
#import <objc/runtime.h>
#include "swift/Runtime/Once.h"
#include <dlfcn.h>

typedef enum {
    dyld_objc_string_kind
} DyldObjCConstantKind;

using namespace swift;

static CFHashCode(*_CFStringHashCString)(const uint8_t *bytes, CFIndex len);
static CFHashCode(*_CFStringHashNSString)(id str);
static CFTypeID(*_CFGetTypeID)(CFTypeRef obj);
static CFTypeID _CFStringTypeID = 0;
static swift_once_t initializeBridgingFuncsOnce;

extern "C" bool _dyld_is_objc_constant(DyldObjCConstantKind kind,
                                       const void *addr) SWIFT_RUNTIME_WEAK_IMPORT;

static void _initializeBridgingFunctionsImpl(void *ctxt) {
  auto getStringTypeID =
    (CFTypeID(*)(void))
    dlsym(RTLD_DEFAULT, "CFStringGetTypeID");
  assert(getStringTypeID);
  _CFStringTypeID = getStringTypeID();
  
  _CFGetTypeID = (CFTypeID(*)(CFTypeRef obj))dlsym(RTLD_DEFAULT, "CFGetTypeID");
  _CFStringHashNSString = (CFHashCode(*)(id))dlsym(RTLD_DEFAULT,
                                                   "CFStringHashNSString");
  _CFStringHashCString = (CFHashCode(*)(const uint8_t *, CFIndex))dlsym(
                                                   RTLD_DEFAULT,
                                                   "CFStringHashCString");
}

static inline void initializeBridgingFunctions() {
  swift_once(&initializeBridgingFuncsOnce,
             _initializeBridgingFunctionsImpl,
             nullptr);
}

__swift_uint8_t
_swift_stdlib_isNSString(id obj) {
  initializeBridgingFunctions();
  return _CFGetTypeID((CFTypeRef)obj) == _CFStringTypeID ? 1 : 0;
}

_swift_shims_CFHashCode
_swift_stdlib_CFStringHashNSString(id _Nonnull obj) {
  initializeBridgingFunctions();
  return _CFStringHashNSString(obj);
}

_swift_shims_CFHashCode
_swift_stdlib_CFStringHashCString(const _swift_shims_UInt8 * _Nonnull bytes,
                                  _swift_shims_CFIndex length) {
  initializeBridgingFunctions();
  return _CFStringHashCString(bytes, length);
}

const __swift_uint8_t *
_swift_stdlib_NSStringCStringUsingEncodingTrampoline(id _Nonnull obj,
                                                  unsigned long encoding) {
  typedef __swift_uint8_t * _Nullable (*cStrImplPtr)(id, SEL, unsigned long);
  cStrImplPtr imp = (cStrImplPtr)class_getMethodImplementation([obj superclass],
                                                               @selector(cStringUsingEncoding:));
  return imp(obj, @selector(cStringUsingEncoding:), encoding);
}

__swift_uint8_t
_swift_stdlib_NSStringGetCStringTrampoline(id _Nonnull obj,
                                         _swift_shims_UInt8 *buffer,
                                         _swift_shims_CFIndex maxLength,
                                         unsigned long encoding) {
  typedef __swift_uint8_t (*getCStringImplPtr)(id,
                                             SEL,
                                             _swift_shims_UInt8 *,
                                             _swift_shims_CFIndex,
                                             unsigned long);
  SEL sel = @selector(getCString:maxLength:encoding:);
  getCStringImplPtr imp = (getCStringImplPtr)class_getMethodImplementation([obj superclass], sel);
  
  return imp(obj, sel, buffer, maxLength, encoding);

}

SWIFT_RUNTIME_STDLIB_API
_swift_shims_NSUInteger
_swift_stdlib_NSStringLengthOfBytesInEncodingTrampoline(id _Nonnull obj,
                                                        unsigned long encoding) {
  typedef _swift_shims_NSUInteger (*getLengthImplPtr)(id,
                                                      SEL,
                                                      unsigned long);
  SEL sel = @selector(lengthOfBytesUsingEncoding:);
  getLengthImplPtr imp = (getLengthImplPtr)class_getMethodImplementation([obj superclass], sel);
  
  return imp(obj, sel, encoding);
}

__swift_uint8_t
_swift_stdlib_dyld_is_objc_constant_string(const void *addr) {
  return (SWIFT_RUNTIME_WEAK_CHECK(_dyld_is_objc_constant)
          && SWIFT_RUNTIME_WEAK_USE(_dyld_is_objc_constant(dyld_objc_string_kind, addr))) ? 1 : 0;
}

typedef const void * _Nullable (*createIndirectTaggedImplPtr)(id,
                                            SEL,
                                            const _swift_shims_UInt8 * _Nonnull,
                                            _swift_shims_CFIndex);
static swift_once_t lookUpIndirectTaggedStringCreationOnce;
static createIndirectTaggedImplPtr createIndirectTaggedString;
static Class indirectTaggedStringClass;

static void lookUpIndirectTaggedStringCreationOnceImpl(void *ctxt) {
  Class cls = objc_lookUpClass("NSIndirectTaggedPointerString");
  if (!cls) return;
  SEL sel = @selector(newIndirectTaggedNSStringWithConstantNullTerminatedASCIIBytes_:length_:);
  Method m = class_getClassMethod(cls, sel);
  if (!m) return;
  createIndirectTaggedString = (createIndirectTaggedImplPtr)method_getImplementation(m);
  indirectTaggedStringClass = cls;
}

SWIFT_RUNTIME_STDLIB_API
const void *
_swift_stdlib_CreateIndirectTaggedPointerString(const __swift_uint8_t *bytes,
                                                _swift_shims_CFIndex len) {
  swift_once(&lookUpIndirectTaggedStringCreationOnce,
             lookUpIndirectTaggedStringCreationOnceImpl,
             nullptr);
  
  if (indirectTaggedStringClass) {
    SEL sel = @selector(newIndirectTaggedNSStringWithConstantNullTerminatedASCIIBytes_:length_:);
    return createIndirectTaggedString(indirectTaggedStringClass, sel, bytes, len);
  }
  return NULL;
}

#endif

