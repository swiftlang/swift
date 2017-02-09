//===--- SwiftObject.mm - Native Swift Object root class ------------------===//
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
// This implements the Objective-C root class that provides basic `id`-
// compatibility and `NSObject` protocol conformance for pure Swift classes.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP
#include <objc/NSObject.h>
#include <objc/runtime.h>
#include <objc/message.h>
#include <objc/objc.h>
#endif
#include "llvm/ADT/StringRef.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Heap.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/ObjCBridge.h"
#include "swift/Strings.h"
#include "../SwiftShims/RuntimeShims.h"
#include "Private.h"
#include "SwiftObject.h"
#include "WeakReference.h"
#include "swift/Runtime/Debug.h"
#if SWIFT_OBJC_INTEROP
#include <dlfcn.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <unordered_map>
#if SWIFT_OBJC_INTEROP
# import <CoreFoundation/CFBase.h> // for CFTypeID
# import <Foundation/Foundation.h>
# include <malloc/malloc.h>
# include <dispatch/dispatch.h>
#endif

using namespace swift;

#if SWIFT_HAS_ISA_MASKING
OBJC_EXPORT __attribute__((__weak_import__))
const uintptr_t objc_debug_isa_class_mask;

static uintptr_t computeISAMask() {
  // The versions of the Objective-C runtime which use non-pointer
  // ISAs also export this symbol.
  if (auto runtimeSymbol = &objc_debug_isa_class_mask)
    return *runtimeSymbol;
  return ~uintptr_t(0);
}

SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_BEGIN
uintptr_t swift::swift_isaMask = computeISAMask();
SWIFT_ALLOWED_RUNTIME_GLOBAL_CTOR_END
#endif

const ClassMetadata *swift::_swift_getClass(const void *object) {
#if SWIFT_OBJC_INTEROP
  if (!isObjCTaggedPointer(object))
    return _swift_getClassOfAllocated(object);
  return reinterpret_cast<const ClassMetadata*>(object_getClass((id) object));
#else
  return _swift_getClassOfAllocated(object);
#endif
}

#if SWIFT_OBJC_INTEROP
static SwiftObject *_allocHelper(Class cls) {
  // XXX FIXME
  // When we have layout information, do precise alignment rounding
  // For now, assume someone is using hardware vector types
#if defined(__x86_64__) || defined(__i386__)
  const size_t mask = 32 - 1;
#else
  const size_t mask = 16 - 1;
#endif
  return reinterpret_cast<SwiftObject *>(swift::swift_allocObject(
    reinterpret_cast<HeapMetadata const *>(cls),
    class_getInstanceSize(cls), mask));
}

NSString *swift::convertStringToNSString(String *swiftString) {
  typedef SWIFT_CC(swift) NSString *ConversionFn(void *sx, void *sy, void *sz);

  // Cached lookup of swift_convertStringToNSString, which is in Foundation.
  static std::atomic<ConversionFn *> TheConvertStringToNSString(nullptr);
  auto convertStringToNSString =
    TheConvertStringToNSString.load(std::memory_order_relaxed);
  if (!convertStringToNSString) {
    convertStringToNSString = (ConversionFn *)(uintptr_t)
      dlsym(RTLD_DEFAULT, "swift_convertStringToNSString");
    // If Foundation hasn't loaded yet, fall back to returning the static string
    // "SwiftObject". The likelihood of someone invoking -description without
    // ObjC interop is low.
    if (!convertStringToNSString)
      return @"SwiftObject";

    TheConvertStringToNSString.store(convertStringToNSString,
                                     std::memory_order_relaxed);
  }

  return convertStringToNSString(swiftString->x,
                                 swiftString->y,
                                 swiftString->z);
}

static NSString *_getDescription(SwiftObject *obj) {
  String tmp;
  swift_retain((HeapObject*)obj);
  swift_getSummary(&tmp, (OpaqueValue*)&obj, _swift_getClassOfAllocated(obj));
  return [convertStringToNSString(&tmp) autorelease];
}

static NSString *_getClassDescription(Class cls) {
  return NSStringFromClass(cls);
}


@implementation SwiftObject
+ (void)initialize {}

+ (instancetype)allocWithZone:(struct _NSZone *)zone {
  assert(zone == nullptr);
  return _allocHelper(self);
}

+ (instancetype)alloc {
  // we do not support "placement new" or zones,
  // so there is no need to call allocWithZone
  return _allocHelper(self);
}

+ (Class)class {
  return self;
}
- (Class)class {
  return (Class) _swift_getClassOfAllocated(self);
}
+ (Class)superclass {
  return (Class) _swift_getSuperclass((const ClassMetadata*) self);
}
- (Class)superclass {
  return (Class) _swift_getSuperclass(_swift_getClassOfAllocated(self));
}

+ (BOOL)isMemberOfClass:(Class)cls {
  return cls == (Class) _swift_getClassOfAllocated(self);
}

- (BOOL)isMemberOfClass:(Class)cls {
  return cls == (Class) _swift_getClassOfAllocated(self);
}

- (instancetype)self {
  return self;
}
- (BOOL)isProxy {
  return NO;
}

- (struct _NSZone *)zone {
  auto zone = malloc_zone_from_ptr(self);
  return (struct _NSZone *)(zone ? zone : malloc_default_zone());
}

- (void)doesNotRecognizeSelector: (SEL) sel {
  Class cls = (Class) _swift_getClassOfAllocated(self);
  fatalError(/* flags = */ 0,
             "Unrecognized selector %c[%s %s]\n",
             class_isMetaClass(cls) ? '+' : '-',
             class_getName(cls), sel_getName(sel));
}

- (id)retain {
  auto SELF = reinterpret_cast<HeapObject *>(self);
  swift_retain(SELF);
  return self;
}
- (void)release {
  auto SELF = reinterpret_cast<HeapObject *>(self);
  swift_release(SELF);
}
- (id)autorelease {
  return _objc_rootAutorelease(self);
}
- (NSUInteger)retainCount {
  return swift::swift_retainCount(reinterpret_cast<HeapObject *>(self));
}
- (BOOL)_isDeallocating {
  return swift_isDeallocating(reinterpret_cast<HeapObject *>(self));
}
- (BOOL)_tryRetain {
  return swift_tryRetain(reinterpret_cast<HeapObject*>(self)) != nullptr;
}
- (BOOL)allowsWeakReference {
  return !swift_isDeallocating(reinterpret_cast<HeapObject *>(self));
}
- (BOOL)retainWeakReference {
  return swift_tryRetain(reinterpret_cast<HeapObject*>(self)) != nullptr;
}

// Retaining the class object itself is a no-op.
+ (id)retain {
  return self;
}
+ (void)release {
  /* empty */
}
+ (id)autorelease {
  return self;
}
+ (NSUInteger)retainCount {
  return ULONG_MAX;
}
+ (BOOL)_isDeallocating {
  return NO;
}
+ (BOOL)_tryRetain {
  return YES;
}
+ (BOOL)allowsWeakReference {
  return YES;
}
+ (BOOL)retainWeakReference {
  return YES;
}

- (void)dealloc {
  swift_rootObjCDealloc(reinterpret_cast<HeapObject *>(self));
}

- (BOOL)isKindOfClass:(Class)someClass {
  for (auto cls = _swift_getClassOfAllocated(self); cls != nullptr;
       cls = _swift_getSuperclass(cls))
    if (cls == (const ClassMetadata*) someClass)
      return YES;

  return NO;
}

+ (BOOL)isSubclassOfClass:(Class)someClass {
  for (auto cls = (const ClassMetadata*) self; cls != nullptr;
       cls = _swift_getSuperclass(cls))
    if (cls == (const ClassMetadata*) someClass)
      return YES;

  return NO;
}

+ (BOOL)respondsToSelector:(SEL)sel {
  if (!sel) return NO;
  return class_respondsToSelector((Class) _swift_getClassOfAllocated(self), sel);
}

- (BOOL)respondsToSelector:(SEL)sel {
  if (!sel) return NO;
  return class_respondsToSelector((Class) _swift_getClassOfAllocated(self), sel);
}

+ (BOOL)instancesRespondToSelector:(SEL)sel {
  if (!sel) return NO;
  return class_respondsToSelector(self, sel);
}

- (BOOL)conformsToProtocol:(Protocol*)proto {
  if (!proto) return NO;
  auto selfClass = (Class) _swift_getClassOfAllocated(self);

  // Walk the superclass chain.
  while (selfClass) {
    if (class_conformsToProtocol(selfClass, proto))
      return YES;
    selfClass = class_getSuperclass(selfClass);
  }

  return NO;
}

+ (BOOL)conformsToProtocol:(Protocol*)proto {
  if (!proto) return NO;

  // Walk the superclass chain.
  Class selfClass = self;
  while (selfClass) {
    if (class_conformsToProtocol(selfClass, proto))
      return YES;
    selfClass = class_getSuperclass(selfClass);
  }

  return NO;
}

- (NSUInteger)hash {
  return (NSUInteger)self;
}

- (BOOL)isEqual:(id)object {
  return self == object;
}

- (id)performSelector:(SEL)aSelector {
  return ((id(*)(id, SEL))objc_msgSend)(self, aSelector);
}

- (id)performSelector:(SEL)aSelector withObject:(id)object {
  return ((id(*)(id, SEL, id))objc_msgSend)(self, aSelector, object);
}

- (id)performSelector:(SEL)aSelector withObject:(id)object1
                                     withObject:(id)object2 {
  return ((id(*)(id, SEL, id, id))objc_msgSend)(self, aSelector, object1,
                                                                 object2);
}

- (NSString *)description {
  return _getDescription(self);
}
- (NSString *)debugDescription {
  return _getDescription(self);
}

+ (NSString *)description {
  return _getClassDescription(self);
}
+ (NSString *)debugDescription {
  return _getClassDescription(self);
}

- (NSString *)_copyDescription {
  // The NSObject version of this pushes an autoreleasepool in case -description
  // autoreleases, but we're OK with leaking things if we're at the top level
  // of the main thread with no autorelease pool.
  return [[self description] retain];
}

- (CFTypeID)_cfTypeID {
  // Adopt the same CFTypeID as NSObject.
  static CFTypeID result;
  static dispatch_once_t predicate;
  dispatch_once_f(&predicate, &result, [](void *resultAddr) {
    id obj = [[NSObject alloc] init];
    *(CFTypeID*)resultAddr = [obj _cfTypeID];
    [obj release];
  });
  return result;
}

// Foundation collections expect these to be implemented.
- (BOOL)isNSArray__      { return NO; }
- (BOOL)isNSDictionary__ { return NO; }
- (BOOL)isNSSet__        { return NO; }
- (BOOL)isNSOrderedSet__ { return NO; }
- (BOOL)isNSNumber__     { return NO; }
- (BOOL)isNSData__       { return NO; }
- (BOOL)isNSDate__       { return NO; }
- (BOOL)isNSString__     { return NO; }
- (BOOL)isNSValue__      { return NO; }

@end

#endif

/// Decide dynamically whether the given class uses native Swift
/// reference-counting.
bool swift::usesNativeSwiftReferenceCounting(const ClassMetadata *theClass) {
#if SWIFT_OBJC_INTEROP
  if (!theClass->isTypeMetadata()) return false;
  return (theClass->getFlags() & ClassFlags::UsesSwift1Refcounting);
#else
  return true;
#endif
}

/// Decide dynamically whether the given type metadata uses native Swift
/// reference-counting.  The metadata is known to correspond to a class
/// type, but note that does not imply being known to be a ClassMetadata
/// due to the existence of ObjCClassWrapper.
SWIFT_RUNTIME_EXPORT
bool
swift_objc_class_usesNativeSwiftReferenceCounting(const Metadata *theClass) {
#if SWIFT_OBJC_INTEROP
  // If this is ObjC wrapper metadata, the class is definitely not using
  // Swift ref-counting.
  if (isa<ObjCClassWrapperMetadata>(theClass)) return false;

  // Otherwise, it's class metadata.
  return usesNativeSwiftReferenceCounting(cast<ClassMetadata>(theClass));
#else
  return true;
#endif
}

// The non-pointer bits, excluding the ObjC tag bits.
static auto const unTaggedNonNativeBridgeObjectBits
  = heap_object_abi::SwiftSpareBitsMask
  & ~heap_object_abi::ObjCReservedBitsMask;

#if SWIFT_OBJC_INTEROP

#if defined(__x86_64__)
static uintptr_t const objectPointerIsObjCBit = 0x4000000000000000ULL;
#elif defined(__arm64__)
static uintptr_t const objectPointerIsObjCBit = 0x4000000000000000ULL;
#else
static uintptr_t const objectPointerIsObjCBit = 0x00000002U;
#endif

void swift::swift_unknownRetain_n(void *object, int n)
    SWIFT_CC(DefaultCC_IMPL) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (objectUsesNativeSwiftReferenceCounting(object)) {
    swift_retain_n(static_cast<HeapObject *>(object), n);
    return;
  }
  for (int i = 0; i < n; ++i)
    objc_retain(static_cast<id>(object));
}

void swift::swift_unknownRelease_n(void *object, int n)
    SWIFT_CC(DefaultCC_IMPL) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (objectUsesNativeSwiftReferenceCounting(object))
    return swift_release_n(static_cast<HeapObject *>(object), n);
  for (int i = 0; i < n; ++i)
    objc_release(static_cast<id>(object));
}

void swift::swift_unknownRetain(void *object)
    SWIFT_CC(DefaultCC_IMPL) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (objectUsesNativeSwiftReferenceCounting(object)) {
    swift_retain(static_cast<HeapObject *>(object));
    return;
  }
  objc_retain(static_cast<id>(object));
}

void swift::swift_unknownRelease(void *object)
    SWIFT_CC(DefaultCC_IMPL) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (objectUsesNativeSwiftReferenceCounting(object))
    return SWIFT_RT_ENTRY_CALL(swift_release)(static_cast<HeapObject *>(object));
  return objc_release(static_cast<id>(object));
}

void swift::swift_nonatomic_unknownRetain_n(void *object, int n)
    SWIFT_CC(DefaultCC_IMPL) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (objectUsesNativeSwiftReferenceCounting(object)) {
    swift_nonatomic_retain_n(static_cast<HeapObject *>(object), n);
    return;
  }
  for (int i = 0; i < n; ++i)
    objc_retain(static_cast<id>(object));
}

void swift::swift_nonatomic_unknownRelease_n(void *object, int n)
    SWIFT_CC(DefaultCC_IMPL) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (objectUsesNativeSwiftReferenceCounting(object))
    return swift_nonatomic_release_n(static_cast<HeapObject *>(object), n);
  for (int i = 0; i < n; ++i)
    objc_release(static_cast<id>(object));
}

void swift::swift_nonatomic_unknownRetain(void *object)
    SWIFT_CC(DefaultCC_IMPL) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (objectUsesNativeSwiftReferenceCounting(object)) {
    swift_nonatomic_retain(static_cast<HeapObject *>(object));
    return;
  }
  objc_retain(static_cast<id>(object));
}

void swift::swift_nonatomic_unknownRelease(void *object)
    SWIFT_CC(DefaultCC_IMPL) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (objectUsesNativeSwiftReferenceCounting(object))
    return SWIFT_RT_ENTRY_CALL(swift_release)(static_cast<HeapObject *>(object));
  return objc_release(static_cast<id>(object));
}


/// Return true iff the given BridgeObject is not known to use native
/// reference-counting.
///
/// Precondition: object does not encode a tagged pointer
static bool isNonNative_unTagged_bridgeObject(void *object) {
  static_assert((heap_object_abi::SwiftSpareBitsMask & objectPointerIsObjCBit) ==
                objectPointerIsObjCBit,
                "isObjC bit not within spare bits");
  return (uintptr_t(object) & objectPointerIsObjCBit) != 0;
}
#endif

// Mask out the spare bits in a bridgeObject, returning the object it
// encodes.
///
/// Precondition: object does not encode a tagged pointer
static void* toPlainObject_unTagged_bridgeObject(void *object) {
  return (void*)(uintptr_t(object) & ~unTaggedNonNativeBridgeObjectBits);
}

void *swift::swift_bridgeObjectRetain(void *object)
    SWIFT_CC(DefaultCC_IMPL) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return object;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  if (!isNonNative_unTagged_bridgeObject(object)) {
    swift_retain(static_cast<HeapObject *>(objectRef));
    return static_cast<HeapObject *>(objectRef);
  }
  return objc_retain(static_cast<id>(objectRef));
#else
  swift_retain(static_cast<HeapObject *>(objectRef));
  return static_cast<HeapObject *>(objectRef);
#endif
}

SWIFT_RUNTIME_EXPORT
void *swift::swift_nonatomic_bridgeObjectRetain(void *object)
    SWIFT_CC(DefaultCC_IMPL) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return object;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  if (!isNonNative_unTagged_bridgeObject(object)) {
    swift_nonatomic_retain(static_cast<HeapObject *>(objectRef));
    return static_cast<HeapObject *>(objectRef);
  }
  return objc_retain(static_cast<id>(objectRef));
#else
  swift_nonatomic_retain(static_cast<HeapObject *>(objectRef));
  return static_cast<HeapObject *>(objectRef);
#endif
}

SWIFT_RUNTIME_EXPORT
void swift::swift_bridgeObjectRelease(void *object)
    SWIFT_CC(DefaultCC_IMPL) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  if (!isNonNative_unTagged_bridgeObject(object))
    return swift_release(static_cast<HeapObject *>(objectRef));
  return objc_release(static_cast<id>(objectRef));
#else
  swift_release(static_cast<HeapObject *>(objectRef));
#endif
}

void swift::swift_nonatomic_bridgeObjectRelease(void *object)
    SWIFT_CC(DefaultCC_IMPL) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  if (!isNonNative_unTagged_bridgeObject(object))
    return swift_nonatomic_release(static_cast<HeapObject *>(objectRef));
  return objc_release(static_cast<id>(objectRef));
#else
  swift_nonatomic_release(static_cast<HeapObject *>(objectRef));
#endif
}

void *swift::swift_bridgeObjectRetain_n(void *object, int n)
    SWIFT_CC(DefaultCC_IMPL) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return object;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  void *objc_ret = nullptr;
  if (!isNonNative_unTagged_bridgeObject(object)) {
    swift_retain_n(static_cast<HeapObject *>(objectRef), n);
    return static_cast<HeapObject *>(objectRef);
  }
  for (int i = 0;i < n; ++i)
    objc_ret = objc_retain(static_cast<id>(objectRef));
  return objc_ret;
#else
  swift_retain_n(static_cast<HeapObject *>(objectRef), n);
  return static_cast<HeapObject *>(objectRef);
#endif
}

void swift::swift_bridgeObjectRelease_n(void *object, int n)
    SWIFT_CC(DefaultCC_IMPL) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  if (!isNonNative_unTagged_bridgeObject(object))
    return swift_release_n(static_cast<HeapObject *>(objectRef), n);
  for (int i = 0; i < n; ++i)
    objc_release(static_cast<id>(objectRef));
#else
  swift_release_n(static_cast<HeapObject *>(objectRef), n);
#endif
}

void *swift::swift_nonatomic_bridgeObjectRetain_n(void *object, int n)
    SWIFT_CC(DefaultCC_IMPL) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return object;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  void *objc_ret = nullptr;
  if (!isNonNative_unTagged_bridgeObject(object)) {
    swift_nonatomic_retain_n(static_cast<HeapObject *>(objectRef), n);
    return static_cast<HeapObject *>(objectRef);
  }
  for (int i = 0;i < n; ++i)
    objc_ret = objc_retain(static_cast<id>(objectRef));
  return objc_ret;
#else
  swift_nonatomic_retain_n(static_cast<HeapObject *>(objectRef), n);
  return static_cast<HeapObject *>(objectRef);
#endif
}

void swift::swift_nonatomic_bridgeObjectRelease_n(void *object, int n)
    SWIFT_CC(DefaultCC_IMPL) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  if (!isNonNative_unTagged_bridgeObject(object))
    return swift_nonatomic_release_n(static_cast<HeapObject *>(objectRef), n);
  for (int i = 0; i < n; ++i)
    objc_release(static_cast<id>(objectRef));
#else
  swift_nonatomic_release_n(static_cast<HeapObject *>(objectRef), n);
#endif
}


#if SWIFT_OBJC_INTEROP

/*****************************************************************************/
/************************ UNKNOWN UNOWNED REFERENCES *************************/
/*****************************************************************************/

// Swift's native unowned references are implemented purely with
// reference-counting: as long as an unowned reference is held to an object,
// it can be destroyed but never deallocated, being that it remains fully safe
// to pass around a pointer and perform further reference-counting operations.
//
// For imported class types (meaning ObjC, for now, but in principle any
// type which supports ObjC-style weak references but not directly Swift-style
// unowned references), we have to implement this on top of the weak-reference
// support, at least for now.  But we'd like to be able to statically take
// advantage of Swift's representational advantages when we know that all the
// objects involved are Swift-native.  That means that whatever scheme we use
// for unowned references needs to interoperate with code just doing naive
// loads and stores, at least when the ObjC case isn't triggered.
//
// We have to be sensitive about making unreasonable assumptions about the
// implementation of ObjC weak references, and we definitely cannot modify
// memory owned by the ObjC runtime.  In the long run, direct support from
// the ObjC runtime can allow an efficient implementation that doesn't violate
// those requirements, both by allowing us to directly check whether a weak
// reference was cleared by deallocation vs. just initialized to nil and by
// guaranteeing a bit pattern that distinguishes Swift references.  In the
// meantime, out-of-band allocation is inefficient but not ridiculously so.
//
// Note that unowned references need not provide guaranteed behavior in
// the presence of read/write or write/write races on the reference itself.
// Furthermore, and unlike weak references, they also do not need to be
// safe against races with the deallocation of the object.  It is the user's
// responsibility to ensure that the reference remains valid at the time
// that the unowned reference is read.

namespace {
  /// An Objective-C unowned reference.  Given an unknown unowned reference
  /// in memory, it is an ObjC unowned reference if the IsObjCFlag bit
  /// is set; if so, the pointer stored in the reference actually points
  /// to out-of-line storage containing an ObjC weak reference.
  ///
  /// It is an invariant that this out-of-line storage is only ever
  /// allocated and constructed for non-null object references, so if the
  /// weak load yields null, it can only be because the object was deallocated.
  struct ObjCUnownedReference : UnownedReference {
    // Pretending that there's a subclass relationship here means that
    // accesses to objects formally constructed as UnownedReferences will
    // technically be aliasing violations.  However, the language runtime
    // will generally not see any such objects.

    enum : uintptr_t { IsObjCMask = 0x1, IsObjCFlag = 0x1 };

    /// The out-of-line storage of an ObjC unowned reference.
    struct Storage {
      /// A weak reference registered with the ObjC runtime.
      mutable id WeakRef;

      Storage(id ref) {
        assert(ref && "creating storage for null reference?");
        objc_initWeak(&WeakRef, ref);
      }

      Storage(const Storage &other) {
        objc_copyWeak(&WeakRef, &other.WeakRef);
      }

      Storage &operator=(const Storage &other) = delete;

      Storage &operator=(id ref) {
        objc_storeWeak(&WeakRef, ref);
        return *this;
      }

      ~Storage() {
        objc_destroyWeak(&WeakRef);
      }

      // Don't use the C++ allocator.
      void *operator new(size_t size) { return malloc(size); }
      void operator delete(void *ptr) { free(ptr); }
    };

    Storage *storage() {
      assert(isa<ObjCUnownedReference>(this));
      return reinterpret_cast<Storage*>(
               reinterpret_cast<uintptr_t>(Value) & ~IsObjCMask);
    }

    static void initialize(UnownedReference *dest, id value) {
      initializeWithStorage(dest, new Storage(value));
    }

    static void initializeWithCopy(UnownedReference *dest, Storage *src) {
      initializeWithStorage(dest, new Storage(*src));
    }

    static void initializeWithStorage(UnownedReference *dest,
                                      Storage *storage) {
      dest->Value = (HeapObject*) (uintptr_t(storage) | IsObjCFlag);
    }

    static bool classof(const UnownedReference *ref) {
      return (uintptr_t(ref->Value) & IsObjCMask) == IsObjCFlag;
    }
  };
}

static bool isObjCForUnownedReference(void *value) {
  return (isObjCTaggedPointer(value) ||
          !objectUsesNativeSwiftReferenceCounting(value));
}

void swift::swift_unknownUnownedInit(UnownedReference *dest, void *value) {
  if (!value) {
    dest->Value = nullptr;
  } else if (isObjCForUnownedReference(value)) {
    ObjCUnownedReference::initialize(dest, (id) value);
  } else {
    swift_unownedInit(dest, (HeapObject*) value);
  }
}

void swift::swift_unknownUnownedAssign(UnownedReference *dest, void *value) {
  if (!value) {
    swift_unknownUnownedDestroy(dest);
    dest->Value = nullptr;
  } else if (isObjCForUnownedReference(value)) {
    if (auto objcDest = dyn_cast<ObjCUnownedReference>(dest)) {
      objc_storeWeak(&objcDest->storage()->WeakRef, (id) value);
    } else {
      swift_unownedDestroy(dest);
      ObjCUnownedReference::initialize(dest, (id) value);
    }
  } else {
    if (auto objcDest = dyn_cast<ObjCUnownedReference>(dest)) {
      delete objcDest->storage();
      swift_unownedInit(dest, (HeapObject*) value);
    } else {
      swift_unownedAssign(dest, (HeapObject*) value);
    }
  }
}

void *swift::swift_unknownUnownedLoadStrong(UnownedReference *ref) {
  if (!ref->Value) {
    return nullptr;
  } else if (auto objcRef = dyn_cast<ObjCUnownedReference>(ref)) {
    auto result = (void*) objc_loadWeakRetained(&objcRef->storage()->WeakRef);
    if (result == nullptr) {
      swift::swift_abortRetainUnowned(nullptr);
    }
    return result;
  } else {
    return swift_unownedLoadStrong(ref);
  }
}

void *swift::swift_unknownUnownedTakeStrong(UnownedReference *ref) {
  if (!ref->Value) {
    return nullptr;
  } else if (auto objcRef = dyn_cast<ObjCUnownedReference>(ref)) {
    auto storage = objcRef->storage();
    auto result = (void*) objc_loadWeakRetained(&objcRef->storage()->WeakRef);
    if (result == nullptr) {
      swift::swift_abortRetainUnowned(nullptr);
    }
    delete storage;
    return result;
  } else {
    return swift_unownedTakeStrong(ref);
  }
}

void swift::swift_unknownUnownedDestroy(UnownedReference *ref) {
  if (!ref->Value) {
    // Nothing to do.
    return;
  } else if (auto objcRef = dyn_cast<ObjCUnownedReference>(ref)) {
    delete objcRef->storage();
  } else {
    swift_unownedDestroy(ref);
  }
}

void swift::swift_unknownUnownedCopyInit(UnownedReference *dest,
                                         UnownedReference *src) {
  assert(dest != src);
  if (!src->Value) {
    dest->Value = nullptr;
  } else if (auto objcSrc = dyn_cast<ObjCUnownedReference>(src)) {
    ObjCUnownedReference::initializeWithCopy(dest, objcSrc->storage());
  } else {
    swift_unownedCopyInit(dest, src);
  }
}

void swift::swift_unknownUnownedTakeInit(UnownedReference *dest,
                                         UnownedReference *src) {
  assert(dest != src);
  dest->Value = src->Value;
}

void swift::swift_unknownUnownedCopyAssign(UnownedReference *dest,
                                           UnownedReference *src) {
  if (dest == src) return;

  if (auto objcSrc = dyn_cast<ObjCUnownedReference>(src)) {
    if (auto objcDest = dyn_cast<ObjCUnownedReference>(dest)) {
      // ObjC unfortunately doesn't expose a copy-assign operation.
      objc_destroyWeak(&objcDest->storage()->WeakRef);
      objc_copyWeak(&objcDest->storage()->WeakRef,
                    &objcSrc->storage()->WeakRef);
      return;
    }

    swift_unownedDestroy(dest);
    ObjCUnownedReference::initializeWithCopy(dest, objcSrc->storage());
  } else {
    if (auto objcDest = dyn_cast<ObjCUnownedReference>(dest)) {
      delete objcDest->storage();
      swift_unownedCopyInit(dest, src);
    } else {
      swift_unownedCopyAssign(dest, src);
    }
  }
}

void swift::swift_unknownUnownedTakeAssign(UnownedReference *dest,
                                           UnownedReference *src) {
  assert(dest != src);

  // There's not really anything more efficient to do here than this.
  swift_unknownUnownedDestroy(dest);
  dest->Value = src->Value;
}

/*****************************************************************************/
/************************** UNKNOWN WEAK REFERENCES **************************/
/*****************************************************************************/

void swift::swift_unknownWeakInit(WeakReference *ref, void *value) {
  return ref->unknownInit(value);
}

void swift::swift_unknownWeakAssign(WeakReference *ref, void *value) {
  return ref->unknownAssign(value);
}

void *swift::swift_unknownWeakLoadStrong(WeakReference *ref) {
  return ref->unknownLoadStrong();
}

void *swift::swift_unknownWeakTakeStrong(WeakReference *ref) {
  return ref->unknownTakeStrong();
}

void swift::swift_unknownWeakDestroy(WeakReference *ref) {
  ref->unknownDestroy();
}

void swift::swift_unknownWeakCopyInit(WeakReference *dest, WeakReference *src) {
  dest->unknownCopyInit(src);
}
void swift::swift_unknownWeakTakeInit(WeakReference *dest, WeakReference *src) {
  dest->unknownTakeInit(src);
}
void swift::swift_unknownWeakCopyAssign(WeakReference *dest,
                                        WeakReference *src) {
  dest->unknownCopyAssign(src);
}
void swift::swift_unknownWeakTakeAssign(WeakReference *dest,
                                        WeakReference *src) {
  dest->unknownTakeAssign(src);
}

// SWIFT_OBJC_INTEROP
#endif

/*****************************************************************************/
/******************************* DYNAMIC CASTS *******************************/
/*****************************************************************************/

#if SWIFT_OBJC_INTEROP
const void *
swift::swift_dynamicCastObjCClass(const void *object,
                                  const ClassMetadata *targetType) {
  // FIXME: We need to decide if this is really how we want to treat 'nil'.
  if (object == nullptr)
    return nullptr;

  if ([(id)object isKindOfClass:(Class)targetType]) {
    return object;
  }

  return nullptr;
}

const void *
swift::swift_dynamicCastObjCClassUnconditional(const void *object,
                                             const ClassMetadata *targetType) {
  // FIXME: We need to decide if this is really how we want to treat 'nil'.
  if (object == nullptr)
    return nullptr;

  if ([(id)object isKindOfClass:(Class)targetType]) {
    return object;
  }

  Class sourceType = object_getClass((id)object);
  swift_dynamicCastFailure(reinterpret_cast<const Metadata *>(sourceType),
                           targetType);
}

const void *
swift::swift_dynamicCastForeignClass(const void *object,
                                     const ForeignClassMetadata *targetType) {
  // FIXME: Actually compare CFTypeIDs, once they are available in the metadata.
  return object;
}

const void *
swift::swift_dynamicCastForeignClassUnconditional(
         const void *object,
         const ForeignClassMetadata *targetType) {
  // FIXME: Actual compare CFTypeIDs, once they are available in the metadata.
  return object;
}

bool swift::objectConformsToObjCProtocol(const void *theObject,
                                         const ProtocolDescriptor *protocol) {
  return [((id) theObject) conformsToProtocol: (Protocol*) protocol];
}


bool swift::classConformsToObjCProtocol(const void *theClass,
                                        const ProtocolDescriptor *protocol) {
  return [((Class) theClass) conformsToProtocol: (Protocol*) protocol];
}

SWIFT_RUNTIME_EXPORT
const Metadata *swift_dynamicCastTypeToObjCProtocolUnconditional(
                                                 const Metadata *type,
                                                 size_t numProtocols,
                                                 Protocol * const *protocols) {
  Class classObject;

  switch (type->getKind()) {
  case MetadataKind::Class:
    // Native class metadata is also the class object.
    classObject = (Class)type;
    break;
  case MetadataKind::ObjCClassWrapper:
    // Unwrap to get the class object.
    classObject = (Class)static_cast<const ObjCClassWrapperMetadata *>(type)
      ->Class;
    break;

  // Other kinds of type can never conform to ObjC protocols.
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::ForeignClass:
    swift_dynamicCastFailure(type, nameForMetadata(type).c_str(),
                             protocols[0], protocol_getName(protocols[0]));

  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    assert(false && "not type metadata");
    break;
  }

  for (size_t i = 0; i < numProtocols; ++i) {
    if (![classObject conformsToProtocol:protocols[i]]) {
      swift_dynamicCastFailure(type, nameForMetadata(type).c_str(),
                               protocols[i], protocol_getName(protocols[i]));
    }
  }

  return type;
}

SWIFT_RUNTIME_EXPORT
const Metadata *swift_dynamicCastTypeToObjCProtocolConditional(
                                                const Metadata *type,
                                                size_t numProtocols,
                                                Protocol * const *protocols) {
  Class classObject;

  switch (type->getKind()) {
  case MetadataKind::Class:
    // Native class metadata is also the class object.
    classObject = (Class)type;
    break;
  case MetadataKind::ObjCClassWrapper:
    // Unwrap to get the class object.
    classObject = (Class)static_cast<const ObjCClassWrapperMetadata *>(type)
      ->Class;
    break;

  // Other kinds of type can never conform to ObjC protocols.
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::ForeignClass:
    return nullptr;

  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    assert(false && "not type metadata");
    break;
  }

  for (size_t i = 0; i < numProtocols; ++i) {
    if (![classObject conformsToProtocol:protocols[i]]) {
      return nullptr;
    }
  }

  return type;
}

SWIFT_RUNTIME_EXPORT
id swift_dynamicCastObjCProtocolUnconditional(id object,
                                              size_t numProtocols,
                                              Protocol * const *protocols) {
  for (size_t i = 0; i < numProtocols; ++i) {
    if (![object conformsToProtocol:protocols[i]]) {
      Class sourceType = object_getClass(object);
      swift_dynamicCastFailure(sourceType, class_getName(sourceType),
                               protocols[i], protocol_getName(protocols[i]));
    }
  }

  return object;
}

SWIFT_RUNTIME_EXPORT
id swift_dynamicCastObjCProtocolConditional(id object,
                                            size_t numProtocols,
                                            Protocol * const *protocols) {
  for (size_t i = 0; i < numProtocols; ++i) {
    if (![object conformsToProtocol:protocols[i]]) {
      return nil;
    }
  }

  return object;
}

void swift::swift_instantiateObjCClass(const ClassMetadata *_c) {
  static const objc_image_info ImageInfo = {0, 0};

  // Ensure the superclass is realized.
  Class c = (Class) _c;
  [class_getSuperclass(c) class];

  // Register the class.
  Class registered = objc_readClassPair(c, &ImageInfo);
  assert(registered == c
         && "objc_readClassPair failed to instantiate the class in-place");
  (void)registered;
}

SWIFT_RT_ENTRY_VISIBILITY
Class swift_getInitializedObjCClass(Class c)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  // Used when we have class metadata and we want to ensure a class has been
  // initialized by the Objective-C runtime. We need to do this because the
  // class "c" might be valid metadata, but it hasn't been initialized yet.
  return [c class];
}

const ClassMetadata *
swift::swift_dynamicCastObjCClassMetatype(const ClassMetadata *source,
                                          const ClassMetadata *dest) {
  if ([(Class)source isSubclassOfClass:(Class)dest])
    return source;
  return nil;
}

const ClassMetadata *
swift::swift_dynamicCastObjCClassMetatypeUnconditional(
                                                   const ClassMetadata *source,
                                                   const ClassMetadata *dest) {
  if ([(Class)source isSubclassOfClass:(Class)dest])
    return source;

  swift_dynamicCastFailure(source, dest);
}
#endif

const ClassMetadata *
swift::swift_dynamicCastForeignClassMetatype(const ClassMetadata *sourceType,
                                             const ClassMetadata *targetType) {
  // FIXME: Actually compare CFTypeIDs, once they are available in
  // the metadata.
  return sourceType;
}

const ClassMetadata *
swift::swift_dynamicCastForeignClassMetatypeUnconditional(
  const ClassMetadata *sourceType,
  const ClassMetadata *targetType)
{
  // FIXME: Actually compare CFTypeIDs, once they arae available in
  // the metadata.
  return sourceType;
}

#if SWIFT_OBJC_INTEROP
// Given a non-nil object reference, return true iff the object uses
// native swift reference counting.
static bool usesNativeSwiftReferenceCounting_nonNull(
  const void* object
) {
  assert(object != nullptr);
  return !isObjCTaggedPointer(object) &&
    objectUsesNativeSwiftReferenceCounting(object);
}
#endif

bool swift::swift_isUniquelyReferenced_nonNull_native(const HeapObject *object)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  assert(object != nullptr);
  assert(!object->refCounts.isDeiniting());
  return object->refCounts.isUniquelyReferenced();
}

bool swift::swift_isUniquelyReferenced_native(const HeapObject* object) {
  return object != nullptr
    && swift::SWIFT_RT_ENTRY_CALL(swift_isUniquelyReferenced_nonNull_native)(object);
}

bool swift::swift_isUniquelyReferencedNonObjC_nonNull(const void* object) {
  assert(object != nullptr);
  return
#if SWIFT_OBJC_INTEROP
    usesNativeSwiftReferenceCounting_nonNull(object) &&
#endif
    SWIFT_RT_ENTRY_CALL(swift_isUniquelyReferenced_nonNull_native)((HeapObject*)object);
}

// Given an object reference, return true iff it is non-nil and refers
// to a native swift object with strong reference count of 1.
bool swift::swift_isUniquelyReferencedNonObjC(
  const void* object
) {
  return object != nullptr
    && swift_isUniquelyReferencedNonObjC_nonNull(object);
}

/// Return true if the given bits of a Builtin.BridgeObject refer to a
/// native swift object whose strong reference count is 1.
bool swift::swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
  uintptr_t bits
) {
  auto bridgeObject = (void*)bits;

  if (isObjCTaggedPointer(bridgeObject))
    return false;

  const auto object = toPlainObject_unTagged_bridgeObject(bridgeObject);

  // Note: we could just return false if all spare bits are set,
  // but in that case the cost of a deeper check for a unique native
  // object is going to be a negligible cost for a possible big win.
#if SWIFT_OBJC_INTEROP
  return !isNonNative_unTagged_bridgeObject(bridgeObject)
             ? SWIFT_RT_ENTRY_CALL(swift_isUniquelyReferenced_nonNull_native)(
                   (const HeapObject *)object)
             : swift_isUniquelyReferencedNonObjC_nonNull(object);
#else
  return SWIFT_RT_ENTRY_CALL(swift_isUniquelyReferenced_nonNull_native)(
      (const HeapObject *)object);
#endif
}

/// Return true if the given bits of a Builtin.BridgeObject refer to a
/// native swift object whose strong reference count is 1.
bool swift::swift_isUniquelyReferencedOrPinnedNonObjC_nonNull_bridgeObject(
  uintptr_t bits
) {
  auto bridgeObject = (void*)bits;

  if (isObjCTaggedPointer(bridgeObject))
    return false;

  const auto object = toPlainObject_unTagged_bridgeObject(bridgeObject);

  // Note: we could just return false if all spare bits are set,
  // but in that case the cost of a deeper check for a unique native
  // object is going to be a negligible cost for a possible big win.
#if SWIFT_OBJC_INTEROP
  if (isNonNative_unTagged_bridgeObject(bridgeObject))
    return swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(object);
#endif
  return swift_isUniquelyReferencedOrPinned_nonNull_native(
                                                   (const HeapObject *)object);
}


/// Given a non-nil object reference, return true if the object is a
/// native swift object and either its strong reference count is 1 or
/// its pinned flag is set.
bool swift::swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(
                                                          const void *object) {
  assert(object != nullptr);
  return
#if SWIFT_OBJC_INTEROP
    usesNativeSwiftReferenceCounting_nonNull(object) &&
#endif
    swift_isUniquelyReferencedOrPinned_nonNull_native(
                                                    (const HeapObject*)object);
}

// Given a non-@objc object reference, return true iff the
// object is non-nil and either has a strong reference count of 1
// or is pinned.
bool swift::swift_isUniquelyReferencedOrPinned_native(const HeapObject *object)
    SWIFT_CC(RegisterPreservingCC_IMPL) {
  return object != nullptr &&
         swift_isUniquelyReferencedOrPinned_nonNull_native(object);
}

/// Given a non-nil native swift object reference, return true if
/// either the object has a strong reference count of 1 or its
/// pinned flag is set.
bool swift::swift_isUniquelyReferencedOrPinned_nonNull_native(
    const HeapObject *object) SWIFT_CC(RegisterPreservingCC_IMPL) {
  assert(object != nullptr);
  assert(!object->refCounts.isDeiniting());
  return object->refCounts.isUniquelyReferencedOrPinned();
}

using ClassExtents = TwoWordPair<size_t, size_t>;

SWIFT_CC(swift) SWIFT_RUNTIME_EXPORT
ClassExtents::Return
swift_class_getInstanceExtents(const Metadata *c) {
  assert(c && c->isClassObject());
  auto metaData = c->getClassObject();
  return ClassExtents{
    metaData->getInstanceAddressPoint(),
    metaData->getInstanceSize() - metaData->getInstanceAddressPoint()
  };
}

#if SWIFT_OBJC_INTEROP

SWIFT_RUNTIME_EXPORT
ClassExtents::Return
swift_objc_class_unknownGetInstanceExtents(const ClassMetadata* c) {
  // Pure ObjC classes never have negative extents.
  if (c->isPureObjC())
    return ClassExtents{0, class_getInstanceSize((Class)c)};

  return swift_class_getInstanceExtents(c);
}

#endif

const ClassMetadata *swift::getRootSuperclass() {
#if SWIFT_OBJC_INTEROP
  static Lazy<const ClassMetadata *> SwiftObjectClass;

  return SwiftObjectClass.get([](void *ptr) {
    *((const ClassMetadata **) ptr) =
        (const ClassMetadata *)[SwiftObject class];
  });
#else
  return nullptr;
#endif
}
