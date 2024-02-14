//===--- SwiftValueNSObject.m - Test SwiftValue's NSObject interop ------===//
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

// This file is compiled and run by SwiftValueNSObject.swift.

#include <Foundation/Foundation.h>
#include <objc/runtime.h>
#include <objc/message.h>

static int Errors;

#define expectTrue(expr)                                            \
  do {                                                              \
    if (!(expr)) {                                                  \
      printf("%s:%d: not true:  %s\n", __FILE__, __LINE__, #expr);  \
      Errors++;                                                     \
    }                                                               \
  } while (0)

#define expectFalse(expr)                                           \
  do {                                                              \
    if (expr) {                                                     \
      printf("%s:%d: not false: %s\n", __FILE__, __LINE__, #expr);  \
      Errors++;                                                     \
    }                                                               \
  } while (0)

/*
  Summary of Swift definitions from SwiftValueNSObject.swift:

  class Swift._SwiftValue <NSObject> { ... }


*/

// Add methods to class SwiftValue that can be called by performSelector: et al

static const char *SwiftValueDemangledName;

static id Perform0(id self, SEL sel) {
    return self;
}

static id Perform1(id self, SEL sel, id one) {
  expectTrue ([one isEqual:@1]);
  return self;
}

static id Perform2(id self, SEL sel, id one, id two) {
  expectTrue ([one isEqual:@1]);
  expectTrue ([two isEqual:@2]);
  return self;
}

static __attribute__((constructor))
void HackSwiftValue()
{
    SwiftValueDemangledName = "__SwiftValue";
    Class cls = objc_getClass(SwiftValueDemangledName);

    class_addMethod(cls, @selector(perform0), (IMP)Perform0, "@@:");
    class_addMethod(cls, @selector(perform1:), (IMP)Perform1, "@@:@");
    class_addMethod(cls, @selector(perform2::), (IMP)Perform2, "@@:@@");
}

void TestSwiftValueNSObjectAssertNoErrors(void)
{
  printf("\nTotal: %d error%s\n",
         Errors, Errors == 1 ? "" : "s");
  if (Errors > 0) {
    exit(1);
  }
}


void TestSwiftValueNSObjectEquals(id e1, id e2)
{
  printf("NSObjectProtocol.isEqual: Expect %s == %s\n",
	 [[e1 description] UTF8String],
	 [[e2 description] UTF8String]);
  expectTrue([e1 isEqual:e2]);
  expectTrue([e2 isEqual:e1]);
}

void TestSwiftValueNSObjectNotEquals(id e1, id e2)
{
  printf("NSObjectProtocol.isEqual: Expect %s != %s\n",
	 [[e1 description] UTF8String],
	 [[e2 description] UTF8String]);
  expectFalse([e1 isEqual:e2]);
  expectFalse([e2 isEqual:e1]);
}

void TestSwiftValueNSObjectHashValue(id e, NSUInteger hashValue)
{
  printf("NSObjectProtocol.hash: Expect [%s hashValue] == %lu,  Got %lu\n",
	 [[e description] UTF8String],
	 (unsigned long)hashValue,
	 [e hash]);
  expectTrue([e hash] == hashValue);
}

void TestSwiftValueNSObjectDefaultHashValue(id e)
{
  NSUInteger hashValue = (NSUInteger)e;
  TestSwiftValueNSObjectHashValue(e, hashValue);
}

void TestSwiftValueNSObject(id c, id d)
{
  printf("TestSwiftValueNSObject\n");

  // Swift struct types don't get ObjC classes
  expectTrue(objc_getClass("SwiftValueNSObject.C") == nil);
  expectTrue(objc_getClass("SwiftValueNSObject.D") == nil);

  Class S = objc_getClass(SwiftValueDemangledName);
  Class S_meta = object_getClass(S);

  printf("Check connectivity.\n");

  expectTrue (S && S_meta && c && d);
  NSSet *distinctnessCheck =
    [NSSet setWithObjects:c, d, S, S_meta, nil];
  expectTrue (distinctnessCheck.count == 4);

  //=== Methods from protocol NSObject ===//

  printf("NSObjectProtocol.class\n");

  expectTrue ([d class] == S);
  expectTrue ([c class] == S);
  expectTrue ([S class] == S);
  expectTrue ([S_meta class] == S_meta);


  printf("NSObjectProtocol.superclass\n");

  expectTrue ([d superclass] == [NSObject class]);
  expectTrue ([c superclass] == [NSObject class]);
  expectTrue ([S superclass] == [NSObject class]);
  expectTrue ([S_meta superclass] == object_getClass(objc_getClass("NSObject")));
  expectTrue ([S_meta superclass] == object_getClass([NSObject class]));

  printf("NSObjectProtocol.isEqual\n");

  expectTrue ([d isEqual:d]);
  expectTrue ([c isEqual:c]);
  expectTrue ([S isEqual:S]);
  expectTrue ([S_meta isEqual:S_meta]);

  expectFalse([d isEqual:S_meta]);
  expectFalse([c isEqual:d]);

  printf("NSObjectProtocol.hash\n");

  expectTrue ([d hash] + [c hash] + [S hash] + [S_meta hash] != 0);

  printf("NSObjectProtocol.self\n");

  expectTrue ([d self] == d);
  expectTrue ([c self] == c);
  expectTrue ([S self] == S);
  expectTrue ([S_meta self] == S_meta);


  printf("NSObjectProtocol.isKindOfClass\n");

  expectTrue ([d isKindOfClass:S]);
  expectTrue ([d isKindOfClass:[NSObject class]]);

  expectTrue ([c isKindOfClass:S]);
  expectFalse([c isKindOfClass:S_meta]);
  expectTrue ([c isKindOfClass:[NSObject class]]);

  expectFalse([S isKindOfClass:S]);
  expectTrue ([S isKindOfClass:S_meta]);
  expectTrue ([S isKindOfClass:[NSObject class]]);

  expectFalse([S_meta isKindOfClass:S]);
  expectFalse([S_meta isKindOfClass:S_meta]);
  expectTrue ([S_meta isKindOfClass:[NSObject class]]);


  printf("NSObjectProtocol.isMemberOfClass\n");

  expectTrue ([d isMemberOfClass:S]);
  expectFalse([d isMemberOfClass:S_meta]);
  expectFalse([d isMemberOfClass:[NSObject class]]);

  expectTrue ([c isMemberOfClass:S]);
  expectFalse([c isMemberOfClass:S_meta]);
  expectFalse([c isMemberOfClass:[NSObject class]]);

  expectFalse([S isMemberOfClass:S]);
  expectTrue ([S isMemberOfClass:S_meta]);
  expectFalse([S isMemberOfClass:[NSObject class]]);

  expectFalse([S_meta isMemberOfClass:S]);
  expectFalse([S_meta isMemberOfClass:S_meta]);
  expectFalse([S_meta isMemberOfClass:[NSObject class]]);


  printf("NSObjectProtocol.respondsToSelector\n");

  // instance method from root class
  expectTrue ([d respondsToSelector:@selector(class)]);
  expectTrue ([c respondsToSelector:@selector(class)]);
  expectTrue ([S respondsToSelector:@selector(class)]);
  expectTrue ([S_meta respondsToSelector:@selector(class)]);

  // non-instance class method from root class
  expectFalse([d respondsToSelector:@selector(alloc)]);
  expectFalse([c respondsToSelector:@selector(alloc)]);
  expectTrue ([S respondsToSelector:@selector(alloc)]);
  expectTrue ([S_meta respondsToSelector:@selector(alloc)]);

  // nonexistent method
  expectFalse([d respondsToSelector:@selector(DESSLOK)]);
  expectFalse([c respondsToSelector:@selector(DESSLOK)]);
  expectFalse([S respondsToSelector:@selector(DESSLOK)]);
  expectFalse([S_meta respondsToSelector:@selector(DESSLOK)]);


  printf("NSObjectProtocol.conformsToProtocol\n");

  expectTrue ([d conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([c conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([S conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([S_meta conformsToProtocol:@protocol(NSObject)]);

  expectFalse([d conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([c conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([S conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([S_meta conformsToProtocol:@protocol(NSCoding)]);


  printf("NSObjectProtocol.description\n");

  expectTrue ([[d description] isEqual:@"This is D's description"]);
  expectTrue ([[c description] isEqual:@"This is C's debug description"]);
  expectTrue ([[S_meta description] isEqual:@(SwiftValueDemangledName)]);

  // NSLog() calls -description and also some private methods.
  // This output is checked by FileCheck in SwiftValueNSObject.swift.
  NSLog(@"c ##%@##", c);
  NSLog(@"d ##%@##", d);
  NSLog(@"S ##%@##", S);


  printf("NSObjectProtocol.debugDescription\n");

  expectTrue ([[d debugDescription] isEqual:@"This is D's description"]);
  expectTrue ([[c debugDescription] isEqual:@"This is C's debug description"]);
  expectTrue ([[S_meta debugDescription] isEqual:@(SwiftValueDemangledName)]);


  // UNIMPLEMENTED for __SwiftValue
  printf("NSObjectProtocol.performSelector\n");
  printf("NSObjectProtocol.performSelector:withObject:\n");
  printf("NSObjectProtocol.performSelector:withObject:withObject:\n");


  printf("NSObjectProtocol.isProxy\n");

  expectFalse([d isProxy]);
  expectFalse([c isProxy]);
  expectFalse([S isProxy]);
  expectFalse([S_meta isProxy]);


  printf("NSObjectProtocol.retain\n");
  printf("NSObjectProtocol.release\n");
  printf("NSObjectProtocol.autorelease\n");
  printf("NSObjectProtocol.retainCount\n");
  @autoreleasepool {
    expectTrue ([[[d retain] autorelease] retainCount] != 0);
    expectTrue ([[[c retain] autorelease] retainCount] != 0);
    expectTrue ([[[S retain] autorelease] retainCount] != 0);
    expectTrue ([[[S_meta retain] autorelease] retainCount] != 0);
  }


/*
  // TODO: Figure out why this breaks on macOS x86_64 and
  // then decide whether or not we should fix it.
  printf("NSObjectProtocol.zone\n");

  expectTrue ([d zone] != nil);
  expectTrue ([c zone] != nil);
  expectTrue ([S zone] != nil);
  expectTrue ([S_meta zone] != nil);
*/

  //=== Other methods from class NSObject ===//

  // FIXME: mostly untested

  printf("NSObject.instancesRespondToSelector\n");

  // non-class objects need not apply
  expectFalse([d respondsToSelector:@selector(instancesRespondToSelector:)]);
  expectFalse([c respondsToSelector:@selector(instancesRespondToSelector:)]);
  expectTrue ([S respondsToSelector:@selector(instancesRespondToSelector:)]);

  // instance method from root class
  expectTrue ([S instancesRespondToSelector:@selector(class)]);
  expectTrue ([S_meta instancesRespondToSelector:@selector(class)]);

  // non-instance class method from root class
  expectFalse([S instancesRespondToSelector:@selector(alloc)]);
  expectTrue ([S_meta instancesRespondToSelector:@selector(alloc)]);

  // nonexistent method
  expectFalse([S instancesRespondToSelector:@selector(DESSLOK)]);
  expectFalse([S_meta instancesRespondToSelector:@selector(DESSLOK)]);


  printf("NSObject.methodForSelector\n");

  IMP fwd = (IMP)_objc_msgForward;
  IMP imp;

  // instance method from root class
  // that has no root metaclass override
  imp = [S methodForSelector:@selector(self)];
  expectTrue (imp != nil);
  expectTrue (imp != fwd);
  expectFalse([c methodForSelector:@selector(self)] == imp);
  expectFalse([d methodForSelector:@selector(self)] == imp);
  expectTrue ([S_meta methodForSelector:@selector(self)] == imp);

  // instance method from root class
  // that also has a root metaclass override
  imp = [c methodForSelector:@selector(class)];
  expectTrue (imp != nil);
  expectTrue (imp != fwd);
  expectTrue ([d methodForSelector:@selector(class)] == imp);

  // non-instance class method from root class
  imp = [S methodForSelector:@selector(alloc)];
  expectTrue (imp != nil);
  expectTrue (imp != fwd);
  expectTrue ([c methodForSelector:@selector(alloc)] == fwd);
  expectTrue ([d methodForSelector:@selector(alloc)] == fwd);
  expectTrue ([S_meta methodForSelector:@selector(alloc)] == imp);

  // nonexistent method
  expectTrue ([c methodForSelector:@selector(DESSLOK)] == fwd);
  expectTrue ([d methodForSelector:@selector(DESSLOK)] == fwd);
  expectTrue ([S methodForSelector:@selector(DESSLOK)] == fwd);
  expectTrue ([S_meta methodForSelector:@selector(DESSLOK)] == fwd);

  printf("NSObject.instanceMethodForSelector\n");

  // non-class objects need not apply
  expectFalse([d respondsToSelector:@selector(instanceMethodForSelector:)]);
  expectFalse([c respondsToSelector:@selector(instanceMethodForSelector:)]);
  expectTrue ([S respondsToSelector:@selector(instanceMethodForSelector:)]);

  // nonexistent method
  expectTrue ([S instanceMethodForSelector:@selector(DESSLOK)] == fwd);
  expectTrue ([S_meta instanceMethodForSelector:@selector(DESSLOK)] == fwd);
}
