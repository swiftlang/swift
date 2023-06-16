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

typedef struct _CFBridgingState {
  int version;
  CFHashCode(*_CFStringHashCString)(const uint8_t *bytes, CFIndex len);
  CFHashCode(*_CFStringHashNSString)(id str);
  CFTypeID(*_CFGetTypeID)(CFTypeRef obj);
  CFTypeID _CFStringTypeID = 0;
  Class NSErrorClass;
  Class NSStringClass;
  Class NSArrayClass;
  Class NSMutableArrayClass;
  Class NSSetClass;
  Class NSDictionaryClass;
  Class NSEnumeratorClass;
  //version 0 ends here
} CFBridgingState;

static CFBridgingState *bridgingState;
static swift_once_t initializeBridgingStateOnce;

extern "C" bool _dyld_is_objc_constant(DyldObjCConstantKind kind,
                                       const void *addr) SWIFT_RUNTIME_WEAK_IMPORT;

static void _initializeBridgingFunctionsFromCFImpl(void *ctxt) {
  bridgingState = (CFBridgingState *)ctxt;
}

static void _initializeBridgingFunctionsImpl(void *ctxt) {
  assert(!bridgingState);
  bridgingState = (CFBridgingState *)calloc(1, sizeof(CFBridgingState));
  auto getStringTypeID = (CFTypeID(*)(void))dlsym(RTLD_DEFAULT, "CFStringGetTypeID");
  assert(getStringTypeID);
  bridgingState->version = 0;
  bridgingState->_CFStringTypeID = getStringTypeID();
  bridgingState->_CFGetTypeID = (CFTypeID(*)(CFTypeRef obj))dlsym(RTLD_DEFAULT, "CFGetTypeID");
  bridgingState->_CFStringHashNSString = (CFHashCode(*)(id))dlsym(RTLD_DEFAULT, "CFStringHashNSString");
  bridgingState->_CFStringHashCString = (CFHashCode(*)(const uint8_t *, CFIndex))dlsym(RTLD_DEFAULT, "CFStringHashCString");
  bridgingState->NSErrorClass = objc_lookUpClass("NSError");
  bridgingState->NSStringClass = objc_lookUpClass("NSString");
  bridgingState->NSArrayClass = objc_lookUpClass("NSArray");
  bridgingState->NSMutableArrayClass = objc_lookUpClass("NSMutableArray");
  bridgingState->NSSetClass = objc_lookUpClass("NSSet");
  bridgingState->NSDictionaryClass = objc_lookUpClass("NSDictionary");
  bridgingState->NSEnumeratorClass = objc_lookUpClass("NSEnumerator");
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  if (bridgingState->NSErrorClass) {
    class_setSuperclass(objc_lookUpClass("__SwiftNativeNSError"), bridgingState->NSErrorClass);
  }
  if (bridgingState->NSArrayClass) {
    class_setSuperclass(objc_lookUpClass("__SwiftNativeNSArrayBase"), bridgingState->NSArrayClass);
  }
  if (bridgingState->NSMutableArrayClass) {
    class_setSuperclass(objc_lookUpClass("__SwiftNativeNSMutableArrayBase"), bridgingState->NSMutableArrayClass);
  }
  if (bridgingState->NSDictionaryClass) {
    class_setSuperclass(objc_lookUpClass("__SwiftNativeNSDictionaryBase"), bridgingState->NSDictionaryClass);
  }
  if (bridgingState->NSSetClass) {
    class_setSuperclass(objc_lookUpClass("__SwiftNativeNSSetBase"), bridgingState->NSSetClass);
  }
  if (bridgingState->NSStringClass) {
    class_setSuperclass(objc_lookUpClass("__SwiftNativeNSStringBase"), bridgingState->NSStringClass);
  }
  if (bridgingState->NSEnumeratorClass) {
    class_setSuperclass(objc_lookUpClass("__SwiftNativeNSEnumeratorBase"), bridgingState->NSEnumeratorClass);
  }
#pragma clang diagnostic pop
}

static inline bool initializeBridgingFunctions() {
  swift_once(&initializeBridgingStateOnce,
             _initializeBridgingFunctionsImpl,
             nullptr);
  return bridgingState->NSStringClass != nullptr;
}

SWIFT_RUNTIME_EXPORT void swift_initializeCoreFoundationState(CFBridgingState const * const state) {
  swift_once(&initializeBridgingStateOnce,
             _initializeBridgingFunctionsFromCFImpl,
             (void *)state);
}

namespace swift {
Class getNSErrorClass();
}

Class swift::getNSErrorClass() {
  if (initializeBridgingFunctions()) {
    return bridgingState->NSErrorClass;
  }
  return nullptr;
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_SPI
bool
swift_stdlib_connectNSBaseClasses() {
  return initializeBridgingFunctions();
}

__swift_uint8_t
_swift_stdlib_isNSString(id obj) {
  assert(initializeBridgingFunctions());
  return bridgingState->_CFGetTypeID((CFTypeRef)obj) == bridgingState->_CFStringTypeID ? 1 : 0;
}

_swift_shims_CFHashCode
_swift_stdlib_CFStringHashNSString(id _Nonnull obj) {
  assert(initializeBridgingFunctions());
  return bridgingState->_CFStringHashNSString(obj);
}

_swift_shims_CFHashCode
_swift_stdlib_CFStringHashCString(const _swift_shims_UInt8 * _Nonnull bytes,
                                  _swift_shims_CFIndex length) {
  assert(initializeBridgingFunctions());
  return bridgingState->_CFStringHashCString(bytes, length);
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

__swift_uint8_t
_swift_stdlib_dyld_is_objc_constant_string(const void *addr) {
  return (SWIFT_RUNTIME_WEAK_CHECK(_dyld_is_objc_constant)
          && SWIFT_RUNTIME_WEAK_USE(_dyld_is_objc_constant(dyld_objc_string_kind, addr))) ? 1 : 0;
}

#endif

