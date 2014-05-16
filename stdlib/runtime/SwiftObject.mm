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
#include "llvm/ADT/DenseMap.h"
#include "Private.h"
#include "Debug.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <mutex>
#include "../shims/shims.h"

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
  swift_getSummary(&tmp, (OpaqueValue*)&obj,
                   (const Metadata*)object_getClass(obj));
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
- (id)class {
  return object_getClass(self);
}
+ (Class)superclass {
  return class_getSuperclass(self);
}
- (Class)superclass {
  return class_getSuperclass([self class]);
}

+ (BOOL)isMemberOfClass:(Class)cls {
  return object_getClass((id)self) == cls;
}

- (BOOL)isMemberOfClass:(Class)cls {
  return [self class] == cls;
}

- (instancetype)self {
  return self;
}
- (BOOL)isProxy {
  return NO;
}

- (struct _NSZone *)zone {
  return (struct _NSZone *)_swift_zone_get_shims();
}

- (void)doesNotRecognizeSelector: (SEL) sel {
  swift::crash("Unrecognized selector");
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

- (void)dealloc {
  _swift_deallocClassInstance(reinterpret_cast<HeapObject *>(self));
}

- (BOOL)isKindOfClass:(Class)someClass {
  for (Class isa = object_getClass(self); isa != Nil;
       isa = class_getSuperclass(isa))
    if (isa == someClass)
      return YES;

  return NO;
}

+ (BOOL)isSubclassOfClass:(Class)someClass {
  for (Class isa = self; isa != Nil; isa = class_getSuperclass(isa))
    if (isa == someClass)
      return YES;

  return NO;
}

+ (BOOL)respondsToSelector:(SEL)sel {
  if (!sel) return NO;
  return class_respondsToSelector(object_getClass((id)self), sel);
}

- (BOOL)respondsToSelector:(SEL)sel {
  if (!sel) return NO;
  return class_respondsToSelector([self class], sel);
}

- (BOOL)conformsToProtocol:(Protocol*)proto {
  if (!proto) return NO;
  return class_conformsToProtocol([self class], proto);
}

- (bool) __usesNativeSwiftReferenceCounting {
  return true;
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
@end

// FIXME: there can be no exhaustive list of ObjC root classes; this
// really needs to be done by the runtime.
@implementation NSObject (__UsesNativeSwiftReferenceCounting)
- (bool) __usesNativeSwiftReferenceCounting {
  return false;
}
@end
// FIXME: NSProxy?

/*****************************************************************************/
/****************************** WEAK REFERENCES ******************************/
/*****************************************************************************/

/// A side-table of shared weak references for use by the unowned entry.
///
/// FIXME: this needs to be integrated with the ObjC runtime so that
/// entries will actually get collected.  Also, that would make this just
/// a simple manipulation of the internal structures there.
namespace {
  struct UnownedRefEntry {
    id Value;
    size_t Count;
  };
}
static llvm::DenseMap<const void*, UnownedRefEntry> UnownedRefs;
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
  if (ins.second) {
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
static bool usesNativeSwiftReferenceCounting(void *object) {
  // This could be *much* faster if it were integrated into the ObjC runtime.
  assert(object);
  return [(id) object __usesNativeSwiftReferenceCounting];
}

void *swift::swift_unknownRetain(void *object) {
  if (object == nullptr) return nullptr;
  if (usesNativeSwiftReferenceCounting(object))
    return swift_retain((HeapObject*) object);
  return objc_retain((id) object);
}

void swift::swift_unknownRelease(void *object) {
  if (object == nullptr) return;
  if (usesNativeSwiftReferenceCounting(object))
    return swift_release((HeapObject*) object);
  return objc_release((id) object);
}

void swift::swift_unknownRetainUnowned(void *object) {
  if (object == nullptr) return;
  if (usesNativeSwiftReferenceCounting(object))
    return swift_retainUnowned((HeapObject*) object);
  objc_rootRetainUnowned((id) object);
}

void swift::swift_unknownWeakRetain(void *object) {
  if (object == nullptr) return;
  if (usesNativeSwiftReferenceCounting(object))
    return swift_weakRetain((HeapObject*) object);
  objc_rootWeakRetain((id) object);
}
void swift::swift_unknownWeakRelease(void *object) {
  if (object == nullptr) return;
  if (usesNativeSwiftReferenceCounting(object))
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
  if (value == nullptr) {
    addr->Value = nullptr;
    return;
  }
  doWeakInit(addr, value, usesNativeSwiftReferenceCounting(value));
}

void swift::swift_unknownWeakAssign(WeakReference *addr, void *newValue) {
  // If the incoming value is null, this is just a destroy and
  // re-initialize.
  if (newValue == nullptr) {
    swift_unknownWeakDestroy(addr);
    addr->Value = nullptr;
    return;
  }

  bool newIsNative = usesNativeSwiftReferenceCounting(newValue);

  // If the existing value is null, this is just an initialize.
  void *oldValue = addr->Value;
  if (oldValue == nullptr)
    return doWeakInit(addr, newValue, newIsNative);

  bool oldIsNative = usesNativeSwiftReferenceCounting(oldValue);

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
  if (value == nullptr) return nullptr;

  if (usesNativeSwiftReferenceCounting(value)) {
    return swift_weakLoadStrong(addr);
  } else {
    return (void*) objc_loadWeakRetained((id*) &addr->Value);
  }
}

void *swift::swift_unknownWeakTakeStrong(WeakReference *addr) {
  void *value = addr->Value;
  if (value == nullptr) return nullptr;

  if (usesNativeSwiftReferenceCounting(value)) {
    return swift_weakTakeStrong(addr);
  } else {
    void *result = (void*) objc_loadWeakRetained((id*) &addr->Value);
    objc_destroyWeak((id*) &addr->Value);
    return result;
  }
}

void swift::swift_unknownWeakDestroy(WeakReference *addr) {
  id object = (id) addr->Value;
  if (object == nullptr) return;
  doWeakDestroy(addr, usesNativeSwiftReferenceCounting(object));
}
void swift::swift_unknownWeakCopyInit(WeakReference *dest, WeakReference *src) {
  id object = (id) src->Value;
  if (object == nullptr) {
    dest->Value = nullptr;
    return;
  }
  if (usesNativeSwiftReferenceCounting(object))
    return swift_weakCopyInit(dest, src);
  objc_copyWeak((id*) &dest->Value, (id*) src);
}
void swift::swift_unknownWeakTakeInit(WeakReference *dest, WeakReference *src) {
  id object = (id) src->Value;
  if (object == nullptr) {
    dest->Value = nullptr;
    return;
  }
  if (usesNativeSwiftReferenceCounting(object))
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


/// \brief Fetch the type metadata associated with the formal dynamic
/// type of the given (possibly Objective-C) object.  The formal
/// dynamic type ignores dynamic subclasses such as those introduced
/// by KVO.
///
/// The object pointer may be a tagged pointer, but cannot be null.
const Metadata *swift::swift_getObjectType(id object) {
  auto theClass = object_getClass(object);
  auto classAsMetadata = reinterpret_cast<ClassMetadata*>(theClass);
  if (classAsMetadata->isTypeMetadata()) return classAsMetadata;
  
  return swift_getObjCClassMetadata(classAsMetadata);
}

extern "C" bool swift_objcRespondsToSelector(id object, SEL selector) {
  return [object respondsToSelector:selector];
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
  
  // objc_readClassPair does not preserve the "is Swift" bit in the rodata, so
  // set it back.
  ((ClassMetadata*)c)->Data |= 1;
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

extern "C" const char *swift_getGenericClassObjCName(const ClassMetadata *clas,
                                                     const char *basename) {
  // FIXME: We should use a runtime mangler to form the real mangled name of the
  // generic instance. Since we don't have a runtime mangler yet, just tack the
  // address of the class onto the basename, which is totally lame but at least
  // gives a unique name to the ObjC runtime.
  size_t baseLen = strlen(basename);
  size_t alignMask = alignof(char) - 1;
  auto fullName = (char*)swift_slowAlloc(baseLen + 17, alignMask, 0);
  snprintf(fullName, baseLen + 17, "%s%016llX", basename,
           (unsigned long long)clas);
  return fullName;
}
