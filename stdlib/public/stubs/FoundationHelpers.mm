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
#include "swift/Runtime/Atomic.h"

typedef enum {
    dyld_objc_string_kind
} DyldObjCConstantKind;

using namespace swift;

typedef struct _CFBridgingState {
  long version;
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

static std::atomic<CFBridgingState const *> bridgingState;
static swift_once_t initializeBridgingStateOnce;

static CFBridgingState const *getBridgingState() {
  return bridgingState.load(SWIFT_MEMORY_ORDER_CONSUME);
}

extern "C" bool _dyld_is_objc_constant(DyldObjCConstantKind kind,
                                       const void *addr) SWIFT_RUNTIME_WEAK_IMPORT;

@class __SwiftNativeNSStringBase, __SwiftNativeNSError, __SwiftNativeNSArrayBase, __SwiftNativeNSMutableArrayBase, __SwiftNativeNSDictionaryBase, __SwiftNativeNSSetBase, __SwiftNativeNSEnumeratorBase;

static void _reparentClasses() {
  auto state = getBridgingState();
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  if (state->NSStringClass) {
    [state->NSStringClass class]; //make sure the class is realized
    class_setSuperclass([__SwiftNativeNSStringBase class],  state->NSStringClass);
  }
  if (state->NSErrorClass) {
    [state->NSErrorClass class]; //make sure the class is realized
    class_setSuperclass([__SwiftNativeNSError class], state->NSErrorClass);
  }
  if (state->NSArrayClass) {
    [state->NSArrayClass class]; //make sure the class is realized
    class_setSuperclass([__SwiftNativeNSArrayBase class],  state->NSArrayClass);
  }
  if (state->NSMutableArrayClass) {
    [state->NSMutableArrayClass class]; //make sure the class is realized
    class_setSuperclass([__SwiftNativeNSMutableArrayBase class],  state->NSMutableArrayClass);
  }
  if (state->NSDictionaryClass) {
    [state->NSDictionaryClass class]; //make sure the class is realized
    class_setSuperclass([__SwiftNativeNSDictionaryBase class],  state->NSDictionaryClass);
  }
  if (state->NSSetClass) {
    [state->NSSetClass class]; //make sure the class is realized
    class_setSuperclass([__SwiftNativeNSSetBase class],  state->NSSetClass);
  }
  if (state->NSEnumeratorClass) {
    [state->NSEnumeratorClass class]; //make sure the class is realized
    class_setSuperclass([__SwiftNativeNSEnumeratorBase class],  state->NSEnumeratorClass);
  }
#pragma clang diagnostic pop
}

static inline bool initializeBridgingFunctions() {
  swift::once(initializeBridgingStateOnce, [](){
    assert(!getBridgingState());
    auto getStringTypeID = (CFTypeID(*)(void))dlsym(RTLD_DEFAULT, "CFStringGetTypeID");
    if (!getStringTypeID) {
      return; //CF not loaded
    }
    auto state = (CFBridgingState *)calloc(1, sizeof(CFBridgingState));
    state->version = 0;
    state->_CFStringTypeID = getStringTypeID();
    state->_CFGetTypeID = (CFTypeID(*)(CFTypeRef obj))dlsym(RTLD_DEFAULT, "CFGetTypeID");
    state->_CFStringHashNSString = (CFHashCode(*)(id))dlsym(RTLD_DEFAULT, "CFStringHashNSString");
    state->_CFStringHashCString = (CFHashCode(*)(const uint8_t *, CFIndex))dlsym(RTLD_DEFAULT, "CFStringHashCString");
    state->NSErrorClass = objc_lookUpClass("NSError");
    state->NSStringClass = objc_lookUpClass("NSString");
    state->NSArrayClass = objc_lookUpClass("NSArray");
    state->NSMutableArrayClass = objc_lookUpClass("NSMutableArray");
    state->NSSetClass = objc_lookUpClass("NSSet");
    state->NSDictionaryClass = objc_lookUpClass("NSDictionary");
    state->NSEnumeratorClass = objc_lookUpClass("NSEnumerator");
    bridgingState.store(state, std::memory_order_relaxed);
    _reparentClasses();
  });
  auto state = getBridgingState();
  return state && state->NSStringClass != nullptr;
}

SWIFT_RUNTIME_EXPORT void swift_initializeCoreFoundationState(CFBridgingState const * const state) {
  //Consume the once token to make sure that the lazy version of this in initializeBridgingFunctions only runs if we didn't hit this
  swift::once(initializeBridgingStateOnce, [state](){
    bridgingState.store(state, std::memory_order_relaxed);
  });
  //It's fine if this runs more than once, it's a noop if it's been done before
  //and we want to make sure it still happens if CF loads late after it failed initially
  bridgingState.store(state, std::memory_order_release);
  _reparentClasses();
}

namespace swift {
Class getNSErrorClass();
}

Class swift::getNSErrorClass() {
  if (initializeBridgingFunctions()) {
    return getBridgingState()->NSErrorClass;
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
  auto state = getBridgingState();
  return state->_CFGetTypeID((CFTypeRef)obj) == state->_CFStringTypeID ? 1 : 0;
}

_swift_shims_CFHashCode
_swift_stdlib_CFStringHashNSString(id _Nonnull obj) {
  assert(initializeBridgingFunctions());
  return getBridgingState()->_CFStringHashNSString(obj);
}

_swift_shims_CFHashCode
_swift_stdlib_CFStringHashCString(const _swift_shims_UInt8 * _Nonnull bytes,
                                  _swift_shims_CFIndex length) {
  assert(initializeBridgingFunctions());
  return getBridgingState()->_CFStringHashCString(bytes, length);
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

