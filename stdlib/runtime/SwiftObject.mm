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

#include <Block.h>
#include <objc/runtime.h>
#if __has_include(<objc/objc-internal.h>)
#include <objc/objc-internal.h>
#else
extern "C" id _objc_rootAutorelease(id);
#endif
#include "swift/Runtime/Alloc.h"
#include "swift/Runtime/Metadata.h"
#include <stdio.h>
#include <stdlib.h>

using namespace swift;

struct SwiftObject_s {
  void *isa;
  long refCount;
};

#if __has_attribute(objc_root_class)
__attribute__((objc_root_class))
#endif
@interface SwiftObject
{
  SwiftObject_s magic;
}
- (id)retain;
- (void)release;
- (id)autorelease;
- (void)dealloc;
@end

void reportUnrecognizedSelector(Class cls, SEL sel) {
  printf("class '%s' does not recognize %s selector '%s', aborting\n",
         class_getName(cls),
         class_isMetaClass(cls) ? "class" : "instance",
         sel_getName(sel));
  abort();
}


@implementation SwiftObject
+ (void)load {}
+ (void)initialize {}

+ (Class)class {
  return self;
}
- (id)class {
  return object_getClass(self);
}

- (void)doesNotRecognizeSelector: (SEL) sel {
  reportUnrecognizedSelector(object_getClass(self), sel);
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

- (void)dealloc {
  auto SELF = reinterpret_cast<HeapObject *>(self);
  swift_deallocObject(SELF, 0);
}

- (BOOL)isKindOfClass:(Class)someClass {
  for (Class isa = object_getClass(self); isa != Nil;
       isa = class_getSuperclass(isa))
    if (isa == someClass)
      return YES;

  return NO;
}
@end


const void *
swift::swift_dynamicCastObjCClass(const void *object,
                                  const ClassMetadata *targetType) {
  // FIXME: We need to decide if this is really how we want to treat 'nil'.
  if (object == nullptr)
    return nullptr;
  // FIXME: -isKindOfClass: is not a core part of the Objective-C language.
  if ([(id)object isKindOfClass:(Class)targetType])
    return object;
  return nullptr;
}

const void *
swift::swift_dynamicCastObjCClassUnconditional(const void *object,
                                               const ClassMetadata *targetType) {
  // FIXME: We need to decide if this is really how we want to treat 'nil'.
  if (object == nullptr)
    return nullptr;
  // FIXME: -isKindOfClass: is not a core part of the Objective-C language.
  if ([(id)object isKindOfClass:(Class)targetType])
    return object;
  abort();
}

extern "C"
const char *swift_getClassName(HeapMetadata *s) {
  // FIXME: The class name should be available in a more Swiftish way.
  // FIXME: Generic parameters of generic class instances.
  return class_getName(reinterpret_cast<Class>(s));
}

//
// Temporary block conversion shims
//

namespace {
  template<typename T> struct block_shim;
  
  template<typename R, typename...A>
  struct block_shim<R(A...)> {
    using block_type = R (^)(A...);
    using code_type = R (*)(A..., HeapObject *);
    
    template<typename...U>
    static void pass(U &&...) {}
    
    template<typename U>
    static void retain_if_id(U const &x) {}
    static void retain_if_id(id x) { [x retain]; }
    
    static block_type shim(code_type code, HeapObject *context) {
      // Manage non-class Swift memory in a way that blocks can understand.
      SwiftRAII contextRAII{context, /*AlreadyRetained=*/true};
      return Block_copy(^R (A...args) {
        // Adjust the context and any id arguments to +1 retain count.
        pass((retain_if_id(args), 0)...);
        return code(args..., swift_retain(*contextRAII));
      });
    }
  };
}

/// MAKE_BLOCK_SHIM(_Tmangled_name, return_type(arg_types...));
#define MAKE_BLOCK_SHIM(MANGLED_NAME, ...) \
  extern "C" block_shim<__VA_ARGS__>::block_type \
  MANGLED_NAME(block_shim<__VA_ARGS__>::code_type code, HeapObject *context) { \
    return block_shim<__VA_ARGS__>::shim(code, context); \
  }

@class NSString;

/// () -> ()
MAKE_BLOCK_SHIM(_TTbbT_T_, void(void));
/// (NSString, UnsafePointer<BOOL>) -> ()
MAKE_BLOCK_SHIM(_TTbbTCSo8NSStringGVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(id, BOOL *));
/// (int64_t) -> ()
MAKE_BLOCK_SHIM(_TTbbSiT_,
                void(int64_t));
// (NSObject, NSObject) -> NSComparisonResult aka NSComparator
MAKE_BLOCK_SHIM(_TTbbTCSo8NSObjectS__V10Foundation18NSComparisonResult,
                long(id, id));
