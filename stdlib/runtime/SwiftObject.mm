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

#include <objc/NSObject.h>
#include <objc/runtime.h>
#include <objc/message.h>
#if __has_include(<objc/objc-internal.h>)
#include <objc/objc-abi.h>
#include <objc/objc-internal.h>
#endif
#include "swift/Runtime/Heap.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/ObjCBridge.h"
#include "../shims/RuntimeShims.h"
#include "Private.h"
#include "Debug.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <mutex>
#include <unordered_map>
#import <CoreFoundation/CFBase.h> // for CFTypeID

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

#if SWIFT_OBJC_INTEROP
const ClassMetadata *swift::_swift_getClass(const void *object) {
  if (!isObjCTaggedPointer(object))
    return _swift_getClassOfAllocated(object);
  return reinterpret_cast<const ClassMetadata*>(object_getClass((id) object));
}
#endif

struct SwiftObject_s {
  void *isa;
  long refCount;
};

#if __has_attribute(objc_root_class)
__attribute__((objc_root_class))
#endif
@interface SwiftObject<NSObject> {
  SwiftObject_s magic;
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
  return 1;
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
  return class_conformsToProtocol((Class) _swift_getClassOfAllocated(self), proto);
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

/// Decide dynamically whether the given object uses native Swift
/// reference-counting.
bool swift::usesNativeSwiftReferenceCounting(const ClassMetadata *theClass) {
  if (!theClass->isTypeMetadata()) return false;
  return (theClass->getFlags() & ClassFlags::UsesSwift1Refcounting);
}

// version for SwiftShims
unsigned char
swift::_swift_usesNativeSwiftReferenceCounting_class(const void *theClass) {
  return usesNativeSwiftReferenceCounting((const ClassMetadata *)theClass);
}

static bool usesNativeSwiftReferenceCounting_allocated(const void *object) {
  assert(!isObjCTaggedPointerOrNull(object));
  return usesNativeSwiftReferenceCounting(_swift_getClassOfAllocated(object));
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

void swift::swift_unknownRetainUnowned(void *object) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (usesNativeSwiftReferenceCounting_allocated(object))
    return swift_retainUnowned((HeapObject*) object);
  objc_rootRetainUnowned((id) object);
}

void swift::swift_unknownWeakRetain(void *object) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (usesNativeSwiftReferenceCounting_allocated(object))
    return swift_weakRetain((HeapObject*) object);
  objc_rootWeakRetain((id) object);
}
void swift::swift_unknownWeakRelease(void *object) {
  if (isObjCTaggedPointerOrNull(object)) return;
  if (usesNativeSwiftReferenceCounting_allocated(object))
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
    objc_initWeak((id*) &addr->Value, (id) value);
  }
}

static void doWeakDestroy(WeakReference *addr, bool valueIsNative) {
  if (valueIsNative) {
    swift_weakDestroy(addr);
  } else {
    objc_destroyWeak((id*) &addr->Value);  
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

/*****************************************************************************/
/******************************* DYNAMIC CASTS *******************************/
/*****************************************************************************/

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

  swift::crash("Swift dynamic cast failed");
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

extern "C" bool swift::_swift_classConformsToObjCProtocol(const void *theClass,
                                           const ProtocolDescriptor *protocol) {
  return [((Class) theClass) conformsToProtocol: (Protocol*) protocol];
}

extern "C" id swift_dynamicCastObjCProtocolUnconditional(id object,
                                                 size_t numProtocols,
                                                 Protocol * const *protocols) {
  for (size_t i = 0; i < numProtocols; ++i) {
    // FIXME: More informative failure message
    if (![object conformsToProtocol:protocols[i]]) {
      abort();
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

extern "C" void swift_instantiateObjCClass(Class c) {
  static const objc_image_info ImageInfo = {0, 0};

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
  swift::crash("Swift dynamic cast failed");
}

const ClassMetadata *
swift::swift_dynamicCastForeignClassMetatype(const ClassMetadata *sourceType,
                                             const ClassMetadata *targetType) {
  // FIXME: Actually compare CFTypeIDs, once they arae available in
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

// Given a non-nil object reference, return true iff the object uses
// native swift reference counting.
unsigned char swift::_swift_usesNativeSwiftReferenceCounting_nonNull(
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
unsigned char swift::_swift_isUniquelyReferenced_nonNull_native(
  const HeapObject* object
) {
  assert(object != nullptr);
  return object->refCount < 2 * RC_INTERVAL;
}

// Given a non-nil object reference, return true iff the object is a
// native swift object with strong reference count of 1.
unsigned char swift::_swift_isUniquelyReferencedNonObjC_nonNull(
  const void* object
) {
  assert(object != nullptr);
  return
#if SWIFT_OBJC_INTEROP
    swift::_swift_usesNativeSwiftReferenceCounting_nonNull(object) &&
#endif 
    _swift_isUniquelyReferenced_nonNull_native((HeapObject*)object);
}

//===----------------------------------------------------------------------===//
// FIXME: this should return bool but it chokes the compiler
// <rdar://problem/18573806>
//===----------------------------------------------------------------------===//
/// Given the bits of a possibly-nil Native swift object reference, or of a
/// word-sized Swift enum containing a Native swift object reference as
/// a payload, return true iff the object's strong reference count is
/// 1.
unsigned char swift::_swift_isUniquelyReferenced_native_spareBits(
  std::uintptr_t bits
) {
  const auto object = reinterpret_cast<HeapObject*>(
    bits & ~heap_object_abi::SwiftSpareBitsMask);

  // Sometimes we have a NULL "owner" object, e.g. because the data
  // being referenced (usually via UnsafeMutablePointer<T>) has infinite
  // lifetime, or lifetime managed outside the Swift object system.
  // In these cases we have to assume the data is shared among
  // multiple references, and needs to be copied before modification.
  return object != nullptr && object->refCount < 2 * RC_INTERVAL;
}

/// Returns class_getInstanceSize(c)
///
/// That function is otherwise unavailable to the core stdlib.
size_t swift::_swift_class_getInstanceSize_class(const void* c) {
  return class_getInstanceSize((Class)c);
}
