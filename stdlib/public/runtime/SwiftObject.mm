//===--- SwiftObject.mm - Native Swift Object root class ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This implements runtime support for bridging between Swift and Objective-C
// types in cases where they aren't trivial.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"
#if SWIFT_OBJC_INTEROP
#include <objc/NSObject.h>
#include <objc/runtime.h>
#include <objc/message.h>
#if __has_include(<objc/objc-internal.h>)
#include <objc/objc-abi.h>
#include <objc/objc-internal.h>
#endif
#endif
#include "swift/Runtime/Heap.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/ObjCBridge.h"
#include "../SwiftShims/RuntimeShims.h"
#include "Private.h"
#include "Debug.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <mutex>
#include <unordered_map>
#if SWIFT_OBJC_INTEROP
# import <CoreFoundation/CFBase.h> // for CFTypeID
# include <malloc/malloc.h>
# include <dispatch/dispatch.h>
#endif
#if SWIFT_RUNTIME_ENABLE_DTRACE
# include "SwiftRuntimeDTraceProbes.h"
#else
#define SWIFT_ISUNIQUELYREFERENCED()
#define SWIFT_ISUNIQUELYREFERENCEDORPINNED()
#endif

#if SWIFT_OBJC_INTEROP
// Redeclare these just we check them.
extern "C" id objc_retain(id);
extern "C" void objc_release(id);
extern "C" id _objc_rootAutorelease(id);
extern "C" void objc_moveWeak(id*, id*);
extern "C" void objc_copyWeak(id*, id*);
extern "C" id objc_initWeak(id*, id);
extern "C" id objc_storeWeak(id*, id);
extern "C" void objc_destroyWeak(id*);
extern "C" id objc_loadWeakRetained(id*);
#endif

using namespace swift;

#if SWIFT_HAS_ISA_MASKING
extern "C" __attribute__((weak_import))
const uintptr_t objc_debug_isa_class_mask;

static uintptr_t computeISAMask() {
  // The versions of the Objective-C runtime which use non-pointer
  // ISAs also export this symbol.
  if (auto runtimeSymbol = &objc_debug_isa_class_mask)
    return *runtimeSymbol;
  return ~uintptr_t(0);
}

uintptr_t swift::swift_isaMask = computeISAMask();
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
struct SwiftObject_s {
  void *isa  __attribute__((unavailable));
  long refCount  __attribute__((unavailable));
};

static_assert(std::is_trivially_constructible<SwiftObject_s>::value,
              "SwiftObject must be trivially constructible");
static_assert(std::is_trivially_destructible<SwiftObject_s>::value,
              "SwiftObject must be trivially destructible");

#if __has_attribute(objc_root_class)
__attribute__((objc_root_class))
#endif
@interface SwiftObject<NSObject> {
  // FIXME: rdar://problem/18950072 Clang emits ObjC++ classes as having
  // non-trivial structors if they contain any struct fields at all, regardless of
  // whether they in fact have nontrivial default constructors. Dupe the body
  // of SwiftObject_s into here as a workaround because we don't want to pay
  // the cost of .cxx_destruct method dispatch at deallocation time.
  void *magic_isa  __attribute__((unavailable));
  long magic_refCount  __attribute__((unavailable));
}

- (BOOL)isEqual:(id)object;
- (NSUInteger)hash;

- (Class)superclass;
- (Class)class;
- (instancetype)self;
- (struct _NSZone *)zone;

- (id)performSelector:(SEL)aSelector;
- (id)performSelector:(SEL)aSelector withObject:(id)object;
- (id)performSelector:(SEL)aSelector withObject:(id)object1 withObject:(id)object2;

- (BOOL)isProxy;

+ (BOOL)isSubclassOfClass:(Class)aClass;
- (BOOL)isKindOfClass:(Class)aClass;
- (BOOL)isMemberOfClass:(Class)aClass;
- (BOOL)conformsToProtocol:(Protocol *)aProtocol;

- (BOOL)respondsToSelector:(SEL)aSelector;

- (instancetype)retain;
- (oneway void)release;
- (instancetype)autorelease;
- (NSUInteger)retainCount;

- (NSString *)description;
- (NSString *)debugDescription;
@end

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

// Helper from the standard library for stringizing an arbitrary object.
namespace {
  struct String { void *x, *y, *z; };
}

extern "C" void swift_getSummary(String *out, OpaqueValue *value,
                                 const Metadata *T);

static NSString *_getDescription(SwiftObject *obj) {
  // Cached lookup of swift_convertStringToNSString, which is in Foundation.
  static NSString *(*convertStringToNSString)(void *sx, void *sy, void *sz)
    = nullptr;
  
  if (!convertStringToNSString) {
    convertStringToNSString = (decltype(convertStringToNSString))(uintptr_t)
      dlsym(RTLD_DEFAULT, "swift_convertStringToNSString");
    // If Foundation hasn't loaded yet, fall back to returning the static string
    // "SwiftObject". The likelihood of someone invoking -description without
    // ObjC interop is low.
    if (!convertStringToNSString)
      return @"SwiftObject";
  }
  
  String tmp;
  swift_retain((HeapObject*)obj);
  swift_getSummary(&tmp, (OpaqueValue*)&obj, _swift_getClassOfAllocated(obj));
  return [convertStringToNSString(tmp.x, tmp.y, tmp.z) autorelease];
}



@implementation SwiftObject
+ (void)load {}
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
  return (struct _NSZone *)malloc_zone_from_ptr(self);
}

- (void)doesNotRecognizeSelector: (SEL) sel {
  Class cls = (Class) _swift_getClassOfAllocated(self);
  fatalError("Unrecognized selector %c[%s %s]\n", 
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
  _swift_deallocClassInstance(reinterpret_cast<HeapObject *>(self));
}

- (BOOL)isKindOfClass:(Class)someClass {
  for (auto isa = _swift_getClassOfAllocated(self); isa != nullptr;
       isa = _swift_getSuperclass(isa))
    if (isa == (const ClassMetadata*) someClass)
      return YES;

  return NO;
}

+ (BOOL)isSubclassOfClass:(Class)someClass {
  for (auto isa = (const ClassMetadata*) self; isa != nullptr;
       isa = _swift_getSuperclass(isa))
    if (isa == (const ClassMetadata*) someClass)
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
  dispatch_once(&predicate, ^{
    id obj = [[NSObject alloc] init];
    result = [obj _cfTypeID];
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

/*****************************************************************************/
/****************************** WEAK REFERENCES ******************************/
/*****************************************************************************/

/// A side-table of shared weak references for use by the unowned entry.
///
/// FIXME: this needs to be integrated with the ObjC runtime so that
/// entries will actually get collected.  Also, that would make this just
/// a simple manipulation of the internal structures there.
///
/// FIXME: this is not actually safe; if the ObjC runtime deallocates
/// the pointer, the keys in UnownedRefs will become dangling
/// references.  rdar://16968733
namespace {
  struct UnownedRefEntry {
    id Value;
    size_t Count;
  };
}

// The ObjC runtime will hold a point into the UnownedRefEntry,
// so we require pointers to objects to be stable across rehashes.
// DenseMap doesn't guarantee that, but std::unordered_map does.
static std::unordered_map<const void*, UnownedRefEntry> UnownedRefs;
static std::mutex UnownedRefsMutex;

static void objc_rootRetainUnowned(id object) {
  std::lock_guard<std::mutex> lock(UnownedRefsMutex);
  auto it = UnownedRefs.find((const void*) object);
  assert(it != UnownedRefs.end());
  assert(it->second.Count > 0);

  // Do an unbalanced retain.
  id result = objc_loadWeakRetained(&it->second.Value);

  // If that yielded null, abort.
  if (!result) _swift_abortRetainUnowned((const void*) object);
}

static void objc_rootWeakRetain(id object) {
  std::lock_guard<std::mutex> lock(UnownedRefsMutex);
  auto ins = UnownedRefs.insert({ (const void*) object, UnownedRefEntry() });
  if (!ins.second) {
    ins.first->second.Count++;
  } else {
    objc_initWeak(&ins.first->second.Value, object);
    ins.first->second.Count = 1;
  }
}

static void objc_rootWeakRelease(id object) {
  std::lock_guard<std::mutex> lock(UnownedRefsMutex);
  auto it = UnownedRefs.find((const void*) object);
  assert(it != UnownedRefs.end());
  assert(it->second.Count > 0);
  if (--it->second.Count == 0) {
    objc_destroyWeak(&it->second.Value);
    UnownedRefs.erase(it);
  }
}
#endif

/// Decide dynamically whether the given object uses native Swift
/// reference-counting.
bool swift::usesNativeSwiftReferenceCounting(const ClassMetadata *theClass) {
#if SWIFT_OBJC_INTEROP
  if (!theClass->isTypeMetadata()) return false;
  return (theClass->getFlags() & ClassFlags::UsesSwift1Refcounting);
#else
  return true;
#endif
}

// version for SwiftShims
bool
swift::_swift_usesNativeSwiftReferenceCounting_class(const void *theClass) {
#if SWIFT_OBJC_INTEROP
  return usesNativeSwiftReferenceCounting((const ClassMetadata *)theClass);
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

static bool usesNativeSwiftReferenceCounting_allocated(const void *object) {
  assert(!isObjCTaggedPointerOrNull(object));
  return usesNativeSwiftReferenceCounting(_swift_getClassOfAllocated(object));
}

static bool usesNativeSwiftReferenceCounting_unowned(const void *object) {
  // If an unknown object is unowned-referenced, it may in fact be implemented
  // using an ObjC weak reference, which will eagerly deallocate the object
  // when strongly released. We have to check first whether the object is in
  // the side table before dereferencing the pointer.
  if (UnownedRefs.count(object))
    return false;
  // For a natively unowned reference, even after all strong references have
  // been released, there's enough of a husk left behind to determine its
  // species.
  return usesNativeSwiftReferenceCounting_allocated(object);
}

void *swift::swift_unknownRetain(void *object) {
  if (isObjCTaggedPointerOrNull(object)) return object;
  if (usesNativeSwiftReferenceCounting_allocated(object))
    return swift_retain((HeapObject*) object);
  return objc_retain((id) object);
}

void swift::swift_unknownRelease(void *object) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (usesNativeSwiftReferenceCounting_allocated(object))
    return swift_release((HeapObject*) object);
  return objc_release((id) object);
}

/// Return true iff the given BridgeObject is not known to use native
/// reference-counting.
///
/// Requires: object does not encode a tagged pointer
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
/// Requires: object does not encode a tagged pointer
static void* toPlainObject_unTagged_bridgeObject(void *object) {
  return (void*)(uintptr_t(object) & ~unTaggedNonNativeBridgeObjectBits);
}

void *swift::swift_bridgeObjectRetain(void *object) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return object;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  if (!isNonNative_unTagged_bridgeObject(object))
    return swift_retain((HeapObject*) objectRef);
  return objc_retain((id) objectRef);
#else
  return swift_retain((HeapObject*) objectRef);
#endif
}

void swift::swift_bridgeObjectRelease(void *object) {
#if SWIFT_OBJC_INTEROP
  if (isObjCTaggedPointer(object))
    return;
#endif

  auto const objectRef = toPlainObject_unTagged_bridgeObject(object);

#if SWIFT_OBJC_INTEROP
  if (!isNonNative_unTagged_bridgeObject(object))
    return swift_release((HeapObject*) objectRef);
  return objc_release((id) objectRef);
#else
  swift_release((HeapObject*) objectRef);
#endif
}

#if SWIFT_OBJC_INTEROP
void swift::swift_unknownRetainUnowned(void *object) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (usesNativeSwiftReferenceCounting_unowned(object))
    return swift_retainUnowned((HeapObject*) object);
  objc_rootRetainUnowned((id) object);
}

void swift::swift_unknownWeakRetain(void *object) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (usesNativeSwiftReferenceCounting_unowned(object))
    return swift_weakRetain((HeapObject*) object);
  objc_rootWeakRetain((id) object);
}
void swift::swift_unknownWeakRelease(void *object) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (usesNativeSwiftReferenceCounting_unowned(object))
    return swift_weakRelease((HeapObject*) object);
  objc_rootWeakRelease((id) object);
}

// FIXME: these are not really valid implementations; they assume too
// much about the implementation of ObjC weak references, and the
// loads from ->Value can race with clears by the runtime.

static void doWeakInit(WeakReference *addr, void *value, bool valueIsNative) {
  assert(value != nullptr);
  if (valueIsNative) {
    swift_weakInit(addr, (HeapObject*) value);
  } else {
#if SWIFT_OBJC_INTEROP
    objc_initWeak((id*) &addr->Value, (id) value);
#else
    assert(valueIsNative);
#endif
  }
}

static void doWeakDestroy(WeakReference *addr, bool valueIsNative) {
  if (valueIsNative) {
    swift_weakDestroy(addr);
  } else {
#if SWIFT_OBJC_INTEROP
    objc_destroyWeak((id*) &addr->Value);
#else
    assert(valueIsNative);
#endif
  }
}

void swift::swift_unknownWeakInit(WeakReference *addr, void *value) {
  if (isObjCTaggedPointerOrNull(value)) {
    addr->Value = (HeapObject*) value;
    return;
  }
  doWeakInit(addr, value, usesNativeSwiftReferenceCounting_allocated(value));
}

void swift::swift_unknownWeakAssign(WeakReference *addr, void *newValue) {
  // If the incoming value is not allocated, this is just a destroy
  // and re-initialize.
  if (isObjCTaggedPointerOrNull(newValue)) {
    swift_unknownWeakDestroy(addr);
    addr->Value = (HeapObject*) newValue;
    return;
  }

  bool newIsNative = usesNativeSwiftReferenceCounting_allocated(newValue);

  // If the existing value is not allocated, this is just an initialize.
  void *oldValue = addr->Value;
  if (isObjCTaggedPointerOrNull(oldValue))
    return doWeakInit(addr, newValue, newIsNative);

  bool oldIsNative = usesNativeSwiftReferenceCounting_allocated(oldValue);

  // If they're both native, we can use the native function.
  if (oldIsNative && newIsNative)
    return swift_weakAssign(addr, (HeapObject*) newValue);

  // If neither is native, we can use the ObjC function.
  if (!oldIsNative && !newIsNative)
    return (void) objc_storeWeak((id*) &addr->Value, (id) newValue);

  // Otherwise, destroy according to one set of semantics and
  // re-initialize with the other.
  doWeakDestroy(addr, oldIsNative);
  doWeakInit(addr, newValue, newIsNative);
}

void *swift::swift_unknownWeakLoadStrong(WeakReference *addr) {
  void *value = addr->Value;
  if (isObjCTaggedPointerOrNull(value)) return value;

  if (usesNativeSwiftReferenceCounting_allocated(value)) {
    return swift_weakLoadStrong(addr);
  } else {
    return (void*) objc_loadWeakRetained((id*) &addr->Value);
  }
}

void *swift::swift_unknownWeakTakeStrong(WeakReference *addr) {
  void *value = addr->Value;
  if (isObjCTaggedPointerOrNull(value)) return value;

  if (usesNativeSwiftReferenceCounting_allocated(value)) {
    return swift_weakTakeStrong(addr);
  } else {
    void *result = (void*) objc_loadWeakRetained((id*) &addr->Value);
    objc_destroyWeak((id*) &addr->Value);
    return result;
  }
}

void swift::swift_unknownWeakDestroy(WeakReference *addr) {
  id object = (id) addr->Value;
  if (isObjCTaggedPointerOrNull(object)) return;
  doWeakDestroy(addr, usesNativeSwiftReferenceCounting_allocated(object));
}
void swift::swift_unknownWeakCopyInit(WeakReference *dest, WeakReference *src) {
  id object = (id) src->Value;
  if (isObjCTaggedPointerOrNull(object)) {
    dest->Value = (HeapObject*) object;
    return;
  }
  if (usesNativeSwiftReferenceCounting_allocated(object))
    return swift_weakCopyInit(dest, src);
  objc_copyWeak((id*) &dest->Value, (id*) src);
}
void swift::swift_unknownWeakTakeInit(WeakReference *dest, WeakReference *src) {
  id object = (id) src->Value;
  if (isObjCTaggedPointerOrNull(object)) {
    dest->Value = (HeapObject*) object;
    return;
  }
  if (usesNativeSwiftReferenceCounting_allocated(object))
    return swift_weakTakeInit(dest, src);
  objc_moveWeak((id*) &dest->Value, (id*) &src->Value);
}
void swift::swift_unknownWeakCopyAssign(WeakReference *dest, WeakReference *src) {
  if (dest == src) return;
  swift_unknownWeakDestroy(dest);
  swift_unknownWeakCopyInit(dest, src);
}
void swift::swift_unknownWeakTakeAssign(WeakReference *dest, WeakReference *src) {
  if (dest == src) return;
  swift_unknownWeakDestroy(dest);
  swift_unknownWeakTakeInit(dest, src);
}
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

extern "C" bool swift_objcRespondsToSelector(id object, SEL selector) {
  return [object respondsToSelector:selector];
}

extern "C" bool swift::_swift_objectConformsToObjCProtocol(const void *theObject,
                                           const ProtocolDescriptor *protocol) {
  return [((id) theObject) conformsToProtocol: (Protocol*) protocol];
}


extern "C" bool swift::_swift_classConformsToObjCProtocol(const void *theClass,
                                           const ProtocolDescriptor *protocol) {
  return [((Class) theClass) conformsToProtocol: (Protocol*) protocol];
}

extern "C" const Metadata *swift_dynamicCastTypeToObjCProtocolUnconditional(
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
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::ForeignClass:
  case MetadataKind::Block:
    swift_dynamicCastFailure(type, nameForMetadata(type).c_str(),
                             protocols[0], protocol_getName(protocols[0]));
      
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
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

extern "C" const Metadata *swift_dynamicCastTypeToObjCProtocolConditional(
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
  case MetadataKind::Opaque:
  case MetadataKind::Tuple:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Existential:
  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::ForeignClass:
  case MetadataKind::Block:
    return nullptr;
      
  case MetadataKind::PolyFunction:
  case MetadataKind::HeapLocalVariable:
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

extern "C" id swift_dynamicCastObjCProtocolUnconditional(id object,
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

extern "C" id swift_dynamicCastObjCProtocolConditional(id object,
                                                 size_t numProtocols,
                                                 Protocol * const *protocols) {
  for (size_t i = 0; i < numProtocols; ++i) {
    if (![object conformsToProtocol:protocols[i]]) {
      return nil;
    }
  }
  
  return object;
}

extern "C" void swift::swift_instantiateObjCClass(const ClassMetadata *_c) {
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

extern "C" Class swift_getInitializedObjCClass(Class c) {
    // Used when we have class metadata and we want to ensure a class has been
    // initialized by the Objective C runtime. We need to do this because the
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

extern "C" const char *swift_getGenericClassObjCName(const ClassMetadata *clas,
                                                     const char *basename) {
  // FIXME: We should use a runtime mangler to form the real mangled name of the
  // generic instance. Since we don't have a runtime mangler yet, just tack the
  // address of the class onto the basename, which is totally lame but at least
  // gives a unique name to the ObjC runtime.
  size_t baseLen = strlen(basename);
  size_t alignMask = alignof(char) - 1;
  auto fullName = (char*)swift_slowAlloc(baseLen + 17, alignMask);
  snprintf(fullName, baseLen + 17, "%s%016llX", basename,
           (unsigned long long)clas);
  return fullName;
}
#endif

// Given a non-nil object reference, return true iff the object uses
// native swift reference counting.
bool swift::_swift_usesNativeSwiftReferenceCounting_nonNull(
  const void* object
) {
    assert(object != nullptr);
#if SWIFT_OBJC_INTEROP
    return !isObjCTaggedPointer(object) &&
      usesNativeSwiftReferenceCounting_allocated(object);
#else 
    return true;
#endif 
}

// Given a non-nil non-@objc object reference, return true iff the
// object has a strong reference count of 1.
bool swift::_swift_isUniquelyReferenced_nonNull_native(
  const HeapObject* object
) {
  assert(object != nullptr);
  assert(!object->refCount.isDeallocating());
  SWIFT_ISUNIQUELYREFERENCED();
  return object->refCount.isUniquelyReferenced();
}

// Given a non-@objc object reference, return true iff the
// object is non-nil and has a strong reference count of 1.
bool swift::_swift_isUniquelyReferenced_native(
  const HeapObject* object
) {
  return object != nullptr
    && _swift_isUniquelyReferenced_nonNull_native(object);
}

// Given a non-nil object reference, return true iff the object is a
// native swift object with strong reference count of 1.
bool swift::_swift_isUniquelyReferencedNonObjC_nonNull(
  const void* object
) {
  assert(object != nullptr);
  return
#if SWIFT_OBJC_INTEROP
    swift::_swift_usesNativeSwiftReferenceCounting_nonNull(object) &&
#endif 
    _swift_isUniquelyReferenced_nonNull_native((HeapObject*)object);
}

// Given an object reference, return true iff it is non-nil and refers
// to a native swift object with strong reference count of 1.
bool swift::_swift_isUniquelyReferencedNonObjC(
  const void* object
) {
  return object != nullptr
    && _swift_isUniquelyReferencedNonObjC_nonNull(object);
}

/// Return true if the given bits of a Builtin.BridgeObject refer to a
/// native swift object whose strong reference count is 1.
bool swift::_swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
  __swift_uintptr_t bits
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
    ? _swift_isUniquelyReferenced_nonNull_native((const HeapObject *)object)
    : _swift_isUniquelyReferencedNonObjC_nonNull(object);
#else
  return _swift_isUniquelyReferenced_nonNull_native((const HeapObject *)object);
#endif
}

/// Return true if the given bits of a Builtin.BridgeObject refer to a
/// native swift object whose strong reference count is 1.
bool swift::_swift_isUniquelyReferencedOrPinnedNonObjC_nonNull_bridgeObject(
  __swift_uintptr_t bits
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
    return _swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(object);
#endif
  return _swift_isUniquelyReferencedOrPinned_nonNull_native(
                                                   (const HeapObject *)object);
}


/// Given a non-nil object reference, return true if the object is a
/// native swift object and either its strong reference count is 1 or
/// its pinned flag is set.
bool swift::_swift_isUniquelyReferencedOrPinnedNonObjC_nonNull(
                                                          const void *object) {
  assert(object != nullptr);
  return
#if SWIFT_OBJC_INTEROP
    swift::_swift_usesNativeSwiftReferenceCounting_nonNull(object) &&
#endif 
    _swift_isUniquelyReferencedOrPinned_nonNull_native(
                                                    (const HeapObject*)object);
}

// Given a non-@objc object reference, return true iff the
// object is non-nil and either has a strong reference count of 1
// or is pinned.
bool swift::_swift_isUniquelyReferencedOrPinned_native(
  const HeapObject* object
) {
  return object != nullptr
    && _swift_isUniquelyReferencedOrPinned_nonNull_native(object);
}

/// Given a non-nil native swift object reference, return true if
/// either the object has a strong reference count of 1 or its
/// pinned flag is set.
bool swift::_swift_isUniquelyReferencedOrPinned_nonNull_native(
                                                    const HeapObject* object) {
  SWIFT_ISUNIQUELYREFERENCEDORPINNED();
  assert(object != nullptr);
  assert(!object->refCount.isDeallocating());
  return object->refCount.isUniquelyReferencedOrPinned();
}

#if SWIFT_OBJC_INTEROP
/// Returns class_getInstanceSize(c)
///
/// That function is otherwise unavailable to the core stdlib.
size_t swift::_swift_class_getInstancePositiveExtentSize(const void* c) {
  return class_getInstanceSize((Class)c);
}
#endif

extern "C" size_t _swift_class_getInstancePositiveExtentSize_native(
    const Metadata *c) {
  assert(c && c->isClassObject());
  auto metaData = c->getClassObject();
  return metaData->getInstanceSize() - metaData->getInstanceAddressPoint();
}

const ClassMetadata *swift::getRootSuperclass() {
#if SWIFT_OBJC_INTEROP
  return (const ClassMetadata *)[SwiftObject class];
#else
  return nullptr;
#endif
}
