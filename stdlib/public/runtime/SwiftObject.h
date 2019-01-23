//===--- SwiftObject.h - Native Swift Object root class ---------*- C++ -*-===//
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

#ifndef SWIFT_RUNTIME_SWIFTOBJECT_H
#define SWIFT_RUNTIME_SWIFTOBJECT_H

#include "swift/Runtime/Config.h"
#include <cstdint>
#include <utility>
#include "swift/Runtime/HeapObject.h"
#if SWIFT_OBJC_INTEROP
#include "llvm/Support/Compiler.h"
#include <objc/NSObject.h>
#endif


#if SWIFT_OBJC_INTEROP

#if SWIFT_DARWIN_ENABLE_STABLE_ABI_BIT
// Source code: "SwiftObject"
// Real class name: mangled "Swift._SwiftObject"
#define SwiftObject _TtCs12_SwiftObject
#else
// Pre-stable ABI uses un-mangled name for SwiftObject
#endif

#if __has_attribute(objc_root_class)
__attribute__((__objc_root_class__))
#endif
SWIFT_RUNTIME_EXPORT @interface SwiftObject<NSObject> {
 @private
  Class isa;
  SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS;
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
+ (BOOL)instancesRespondToSelector:(SEL)aSelector;
- (IMP)methodForSelector:(SEL)aSelector;
+ (IMP)instanceMethodForSelector:(SEL)aSelector;

- (instancetype)retain;
- (oneway void)release;
- (instancetype)autorelease;
- (NSUInteger)retainCount;

- (NSString *)description;
- (NSString *)debugDescription;
@end

namespace swift {

NSString *getDescription(OpaqueValue *value, const Metadata *type);

}

#endif

namespace swift {

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_SPI
HeapObject *_swift_reallocObject(HeapObject *obj, size_t size);

}

#endif
