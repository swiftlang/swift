//===--- SwiftNativeNSObject.mm - NSObject-inheriting native class --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Define the SwiftNativeNSObject class, which inherits from
// NSObject but uses Swift reference-counting.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP
#import <Foundation/Foundation.h>
#import <CoreFoundation/CoreFoundation.h>
#include <objc/NSObject.h>
#include <objc/runtime.h>
#include <objc/objc.h>
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/ObjCBridge.h"

using namespace swift;

SWIFT_RUNTIME_STDLIB_API
@interface SwiftNativeNSObject : NSObject
{
@private
  SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS;
}
@end

@implementation SwiftNativeNSObject

+ (instancetype)allocWithZone: (NSZone *)zone {
  // Allocate the object with swift_allocObject().
  // Note that this doesn't work if called on SwiftNativeNSObject itself,
  // which is not a Swift class.
  auto cls = cast<ClassMetadata>(reinterpret_cast<const HeapMetadata *>(self));
  assert(cls->isTypeMetadata());
  auto result = swift_allocObject(cls, cls->getInstanceSize(),
                                  cls->getInstanceAlignMask());
  return reinterpret_cast<id>(result);
}

- (instancetype)initWithCoder: (NSCoder *)coder {
  return [super init];
}

+ (BOOL)automaticallyNotifiesObserversForKey:(NSString *)key {
  return NO;
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wobjc-missing-super-calls"

STANDARD_OBJC_METHOD_IMPLS_FOR_SWIFT_OBJECTS

#pragma clang diagnostic pop

@end

#endif
