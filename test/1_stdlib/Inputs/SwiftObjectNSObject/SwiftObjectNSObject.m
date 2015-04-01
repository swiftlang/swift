//===--- SwiftObjectNSObject.m - Test SwiftObject's NSObject interop ------===//
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

// This file is compiled and run by SwiftObjectNSObject.swift.

#include <Foundation/Foundation.h>
#include <objc/runtime.h>

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
  Summary of Swift definitions from SwiftObjectNSObject.swift:

  class SwiftObject <NSObject> { ... }

  class C : SwiftObject {
  @objc func cInstanceMethod()
  @objc class func cClassMethod()
  }

  class D : C {
  @objc func dInstanceMethod()
  @objc class func dClassMethod()
  }
*/

// Add methods to class SwiftObject that can be called by performSelector: et al
    @interface SwiftObject /* trust me, I know what I'm doing */ @end
      @implementation SwiftObject (MethodsToPerform)
      -(id) perform0 {
        return self;
    }

-(id) perform1:(id)one {
  expectTrue ([one isEqual:@1]);
  return self;
}

-(id) perform2:(id)one :(id)two {
  expectTrue ([one isEqual:@1]);
  expectTrue ([two isEqual:@2]);
  return self;
}
@end

void TestSwiftObjectNSObject(id c, id d) 
{
  printf("TestSwiftObjectNSObject\n");

  Class S = objc_getClass("SwiftObject");
  Class C = objc_getClass("SwiftObjectNSObject.C");
  Class D = objc_getClass("SwiftObjectNSObject.D");

  Class S_meta = object_getClass(S);
  Class C_meta = object_getClass(C);
  Class D_meta = object_getClass(D);


  printf("Check connectivity.\n");

  expectTrue (S && C && D && S_meta && C_meta && D_meta && c && d);
  NSSet *distinctnessCheck = 
    [NSSet setWithObjects:c, d, S, C, D, S_meta, C_meta, D_meta, nil];
  expectTrue (distinctnessCheck.count == 8);

  expectTrue (class_getSuperclass(D) == C);
  expectTrue (class_getSuperclass(C) == S);
  expectTrue (class_getSuperclass(S) == nil);

  expectTrue (class_getSuperclass(D_meta) == C_meta);
  expectTrue (class_getSuperclass(C_meta) == S_meta);
  expectTrue (class_getSuperclass(S_meta) == S);

  expectTrue (object_getClass(D_meta) == S_meta);
  expectTrue (object_getClass(C_meta) == S_meta);
  expectTrue (object_getClass(S_meta) == S_meta);


  //=== Methods from protocol NSObject ===//

  printf("NSObjectProtocol.class\n");

  expectTrue ([d class] == D);
  expectTrue ([c class] == C);
  expectTrue ([D class] == D);
  expectTrue ([C class] == C);
  expectTrue ([S class] == S);
  expectTrue ([D_meta class] == D_meta);
  expectTrue ([C_meta class] == C_meta);
  expectTrue ([S_meta class] == S_meta);


  printf("NSObjectProtocol.superclass\n");

  expectTrue ([d superclass] == C);
  expectTrue ([c superclass] == S);
  expectTrue ([D superclass] == C);
  expectTrue ([C superclass] == S);
  expectTrue ([S superclass] == nil);
  expectTrue ([D_meta superclass] == C_meta);
  expectTrue ([C_meta superclass] == S_meta);
  expectTrue ([S_meta superclass] == S);


  printf("NSObjectProtocol.isEqual\n");

  expectTrue ([d isEqual:d]);
  expectTrue ([c isEqual:c]);
  expectTrue ([D isEqual:D]);
  expectTrue ([C isEqual:C]);
  expectTrue ([S isEqual:S]);
  expectTrue ([D_meta isEqual:D_meta]);
  expectTrue ([C_meta isEqual:C_meta]);
  expectTrue ([S_meta isEqual:S_meta]);

  expectFalse([d isEqual:S_meta]);
  expectFalse([c isEqual:d]);
  expectFalse([D isEqual:c]);
  expectFalse([C isEqual:D]);
  expectFalse([S isEqual:C]);
  expectFalse([D_meta isEqual:S]);
  expectFalse([C_meta isEqual:D_meta]);
  expectFalse([S_meta isEqual:C_meta]);


  printf("NSObjectProtocol.hash\n");

  expectTrue ([d hash] + [c hash] + [D hash] + [C hash] + [S hash] + 
              [D_meta hash] + [C_meta hash] + [S_meta hash] != 0);


  printf("NSObjectProtocol.self\n");

  expectTrue ([d self] == d);
  expectTrue ([c self] == c);
  expectTrue ([D self] == D);
  expectTrue ([C self] == C);
  expectTrue ([S self] == S);
  expectTrue ([D_meta self] == D_meta);
  expectTrue ([C_meta self] == C_meta);
  expectTrue ([S_meta self] == S_meta);


  printf("NSObjectProtocol.isKindOfClass\n");

  expectTrue ([d isKindOfClass:D]);
  expectTrue ([d isKindOfClass:C]);
  expectTrue ([d isKindOfClass:S]);
  expectFalse([d isKindOfClass:D_meta]);
  expectFalse([d isKindOfClass:C_meta]);
  expectFalse([d isKindOfClass:S_meta]);
  expectFalse([d isKindOfClass:[NSObject class]]);

  expectFalse([c isKindOfClass:D]);
  expectTrue ([c isKindOfClass:C]);
  expectTrue ([c isKindOfClass:S]);
  expectFalse([c isKindOfClass:D_meta]);
  expectFalse([c isKindOfClass:C_meta]);
  expectFalse([c isKindOfClass:S_meta]);
  expectFalse([c isKindOfClass:[NSObject class]]);

  expectFalse([D isKindOfClass:D]);
  expectFalse([D isKindOfClass:C]);
  expectTrue ([D isKindOfClass:S]);
  expectTrue ([D isKindOfClass:D_meta]);
  expectTrue ([D isKindOfClass:C_meta]);
  expectTrue ([D isKindOfClass:S_meta]);
  expectFalse([D isKindOfClass:[NSObject class]]);

  expectFalse([C isKindOfClass:D]);
  expectFalse([C isKindOfClass:C]);
  expectTrue ([C isKindOfClass:S]);
  expectFalse([C isKindOfClass:D_meta]);
  expectTrue ([C isKindOfClass:C_meta]);
  expectTrue ([C isKindOfClass:S_meta]);
  expectFalse([C isKindOfClass:[NSObject class]]);

  expectFalse([S isKindOfClass:D]);
  expectFalse([S isKindOfClass:C]);
  expectTrue ([S isKindOfClass:S]);
  expectFalse([S isKindOfClass:D_meta]);
  expectFalse([S isKindOfClass:C_meta]);
  expectTrue ([S isKindOfClass:S_meta]);
  expectFalse([S isKindOfClass:[NSObject class]]);

  expectFalse([D_meta isKindOfClass:D]);
  expectFalse([D_meta isKindOfClass:C]);
  expectTrue ([D_meta isKindOfClass:S]);
  expectFalse([D_meta isKindOfClass:D_meta]);
  expectFalse([D_meta isKindOfClass:C_meta]);
  expectTrue ([D_meta isKindOfClass:S_meta]);
  expectFalse([D_meta isKindOfClass:[NSObject class]]);

  expectFalse([C_meta isKindOfClass:D]);
  expectFalse([C_meta isKindOfClass:C]);
  expectTrue ([C_meta isKindOfClass:S]);
  expectFalse([C_meta isKindOfClass:D_meta]);
  expectFalse([C_meta isKindOfClass:C_meta]);
  expectTrue ([C_meta isKindOfClass:S_meta]);
  expectFalse([C_meta isKindOfClass:[NSObject class]]);

  expectFalse([S_meta isKindOfClass:D]);
  expectFalse([S_meta isKindOfClass:C]);
  expectTrue ([S_meta isKindOfClass:S]);
  expectFalse([S_meta isKindOfClass:D_meta]);
  expectFalse([S_meta isKindOfClass:C_meta]);
  expectTrue ([S_meta isKindOfClass:S_meta]);
  expectFalse([S_meta isKindOfClass:[NSObject class]]);


  printf("NSObjectProtocol.isMemberOfClass\n");

  expectTrue ([d isMemberOfClass:D]);
  expectFalse([d isMemberOfClass:C]);
  expectFalse([d isMemberOfClass:S]);
  expectFalse([d isMemberOfClass:D_meta]);
  expectFalse([d isMemberOfClass:C_meta]);
  expectFalse([d isMemberOfClass:S_meta]);
  expectFalse([d isMemberOfClass:[NSObject class]]);

  expectFalse([c isMemberOfClass:D]);
  expectTrue ([c isMemberOfClass:C]);
  expectFalse([c isMemberOfClass:S]);
  expectFalse([c isMemberOfClass:D_meta]);
  expectFalse([c isMemberOfClass:C_meta]);
  expectFalse([c isMemberOfClass:S_meta]);
  expectFalse([c isMemberOfClass:[NSObject class]]);

  expectFalse([D isMemberOfClass:D]);
  expectFalse([D isMemberOfClass:C]);
  expectFalse([D isMemberOfClass:S]);
  expectTrue ([D isMemberOfClass:D_meta]);
  expectFalse([D isMemberOfClass:C_meta]);
  expectFalse([D isMemberOfClass:S_meta]);
  expectFalse([D isMemberOfClass:[NSObject class]]);

  expectFalse([C isMemberOfClass:D]);
  expectFalse([C isMemberOfClass:C]);
  expectFalse([C isMemberOfClass:S]);
  expectFalse([C isMemberOfClass:D_meta]);
  expectTrue ([C isMemberOfClass:C_meta]);
  expectFalse([C isMemberOfClass:S_meta]);
  expectFalse([C isMemberOfClass:[NSObject class]]);

  expectFalse([S isMemberOfClass:D]);
  expectFalse([S isMemberOfClass:C]);
  expectFalse([S isMemberOfClass:S]);
  expectFalse([S isMemberOfClass:D_meta]);
  expectFalse([S isMemberOfClass:C_meta]);
  expectTrue ([S isMemberOfClass:S_meta]);
  expectFalse([S isMemberOfClass:[NSObject class]]);

  expectFalse([D_meta isMemberOfClass:D]);
  expectFalse([D_meta isMemberOfClass:C]);
  expectFalse([D_meta isMemberOfClass:S]);
  expectFalse([D_meta isMemberOfClass:D_meta]);
  expectFalse([D_meta isMemberOfClass:C_meta]);
  expectTrue ([D_meta isMemberOfClass:S_meta]);
  expectFalse([D_meta isMemberOfClass:[NSObject class]]);

  expectFalse([C_meta isMemberOfClass:D]);
  expectFalse([C_meta isMemberOfClass:C]);
  expectFalse([C_meta isMemberOfClass:S]);
  expectFalse([C_meta isMemberOfClass:D_meta]);
  expectFalse([C_meta isMemberOfClass:C_meta]);
  expectTrue ([C_meta isMemberOfClass:S_meta]);
  expectFalse([C_meta isMemberOfClass:[NSObject class]]);

  expectFalse([S_meta isMemberOfClass:D]);
  expectFalse([S_meta isMemberOfClass:C]);
  expectFalse([S_meta isMemberOfClass:S]);
  expectFalse([S_meta isMemberOfClass:D_meta]);
  expectFalse([S_meta isMemberOfClass:C_meta]);
  expectTrue ([S_meta isMemberOfClass:S_meta]);
  expectFalse([S_meta isMemberOfClass:[NSObject class]]);


  printf("NSObjectProtocol.respondsToSelector\n");

  // instance method from root class
  expectTrue ([d respondsToSelector:@selector(class)]);
  expectTrue ([c respondsToSelector:@selector(class)]);
  expectTrue ([D respondsToSelector:@selector(class)]);
  expectTrue ([C respondsToSelector:@selector(class)]);
  expectTrue ([S respondsToSelector:@selector(class)]);
  expectTrue ([D_meta respondsToSelector:@selector(class)]);
  expectTrue ([C_meta respondsToSelector:@selector(class)]);
  expectTrue ([S_meta respondsToSelector:@selector(class)]);

  // non-instance class method from root class
  expectFalse([d respondsToSelector:@selector(alloc)]);
  expectFalse([c respondsToSelector:@selector(alloc)]);
  expectTrue ([D respondsToSelector:@selector(alloc)]);
  expectTrue ([C respondsToSelector:@selector(alloc)]);
  expectTrue ([S respondsToSelector:@selector(alloc)]);
  expectTrue ([D_meta respondsToSelector:@selector(alloc)]);
  expectTrue ([C_meta respondsToSelector:@selector(alloc)]);
  expectTrue ([S_meta respondsToSelector:@selector(alloc)]);

  // instance method from subclass C
  expectTrue ([d respondsToSelector:@selector(cInstanceMethod)]);
  expectTrue ([c respondsToSelector:@selector(cInstanceMethod)]);
  expectFalse([D respondsToSelector:@selector(cInstanceMethod)]);
  expectFalse([C respondsToSelector:@selector(cInstanceMethod)]);
  expectFalse([S respondsToSelector:@selector(cInstanceMethod)]);
  expectFalse([D_meta respondsToSelector:@selector(cInstanceMethod)]);
  expectFalse([C_meta respondsToSelector:@selector(cInstanceMethod)]);
  expectFalse([S_meta respondsToSelector:@selector(cInstanceMethod)]);

  // class method from subclass C
  expectFalse([d respondsToSelector:@selector(cClassMethod)]);
  expectFalse([c respondsToSelector:@selector(cClassMethod)]);
  expectTrue ([D respondsToSelector:@selector(cClassMethod)]);
  expectTrue ([C respondsToSelector:@selector(cClassMethod)]);
  expectFalse([S respondsToSelector:@selector(cClassMethod)]);
  expectFalse([D_meta respondsToSelector:@selector(cClassMethod)]);
  expectFalse([C_meta respondsToSelector:@selector(cClassMethod)]);
  expectFalse([S_meta respondsToSelector:@selector(cClassMethod)]);

  // instance method from subclass D
  expectTrue ([d respondsToSelector:@selector(dInstanceMethod)]);
  expectFalse([c respondsToSelector:@selector(dInstanceMethod)]);
  expectFalse([D respondsToSelector:@selector(dInstanceMethod)]);
  expectFalse([C respondsToSelector:@selector(dInstanceMethod)]);
  expectFalse([S respondsToSelector:@selector(dInstanceMethod)]);
  expectFalse([D_meta respondsToSelector:@selector(dInstanceMethod)]);
  expectFalse([C_meta respondsToSelector:@selector(dInstanceMethod)]);
  expectFalse([S_meta respondsToSelector:@selector(dInstanceMethod)]);

  // class method from subclass D
  expectFalse([d respondsToSelector:@selector(dClassMethod)]);
  expectFalse([c respondsToSelector:@selector(dClassMethod)]);
  expectTrue ([D respondsToSelector:@selector(dClassMethod)]);
  expectFalse([C respondsToSelector:@selector(dClassMethod)]);
  expectFalse([S respondsToSelector:@selector(dClassMethod)]);
  expectFalse([D_meta respondsToSelector:@selector(dClassMethod)]);
  expectFalse([C_meta respondsToSelector:@selector(dClassMethod)]);
  expectFalse([S_meta respondsToSelector:@selector(dClassMethod)]);

  // nonexistent method
  expectFalse([d respondsToSelector:@selector(DESSLOK)]);
  expectFalse([c respondsToSelector:@selector(DESSLOK)]);
  expectFalse([D respondsToSelector:@selector(DESSLOK)]);
  expectFalse([C respondsToSelector:@selector(DESSLOK)]);
  expectFalse([S respondsToSelector:@selector(DESSLOK)]);
  expectFalse([D_meta respondsToSelector:@selector(DESSLOK)]);
  expectFalse([C_meta respondsToSelector:@selector(DESSLOK)]);
  expectFalse([S_meta respondsToSelector:@selector(DESSLOK)]);


  printf("NSObjectProtocol.conformsToProtocol\n");

  expectTrue ([d conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([c conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([D conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([C conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([S conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([D_meta conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([C_meta conformsToProtocol:@protocol(NSObject)]);
  expectTrue ([S_meta conformsToProtocol:@protocol(NSObject)]);

  expectFalse([d conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([c conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([D conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([C conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([S conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([D_meta conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([C_meta conformsToProtocol:@protocol(NSCoding)]);
  expectFalse([S_meta conformsToProtocol:@protocol(NSCoding)]);


  printf("NSObjectProtocol.description\n");

  expectTrue ([d description].length > 0);
  expectTrue ([c description].length > 0);
  /* FIXME: radar TBD
     expectTrue ([D description].length > 0);
     expectTrue ([C description].length > 0);
     expectTrue ([S description].length > 0);
     expectTrue ([D_meta description].length > 0);
     expectTrue ([C_meta description].length > 0);
     expectTrue ([S_meta description].length > 0);
  */


  printf("NSObjectProtocol.debugDescription\n");

  expectTrue ([d debugDescription].length > 0);
  expectTrue ([c debugDescription].length > 0);
  /* FIXME: radar TBD
     expectTrue ([D debugDescription].length > 0);
     expectTrue ([C debugDescription].length > 0);
     expectTrue ([S debugDescription].length > 0);
     expectTrue ([D_meta debugDescription].length > 0);
     expectTrue ([C_meta debugDescription].length > 0);
     expectTrue ([S_meta debugDescription].length > 0);
  */


  printf("NSObjectProtocol.performSelector\n");

  SEL sel0 = @selector(perform0);
  expectTrue ([d performSelector:sel0] == d);
  expectTrue ([c performSelector:sel0] == c);
  expectTrue ([D performSelector:sel0] == D);
  expectTrue ([C performSelector:sel0] == C);
  expectTrue ([S performSelector:sel0] == S);
  expectTrue ([D_meta performSelector:sel0] == D_meta);
  expectTrue ([C_meta performSelector:sel0] == C_meta);
  expectTrue ([S_meta performSelector:sel0] == S_meta);


  printf("NSObjectProtocol.performSelector:withObject:\n");

  SEL sel1 = @selector(perform1:);
  expectTrue ([d performSelector:sel1 withObject:@1] == d);
  expectTrue ([c performSelector:sel1 withObject:@1] == c);
  expectTrue ([D performSelector:sel1 withObject:@1] == D);
  expectTrue ([C performSelector:sel1 withObject:@1] == C);
  expectTrue ([S performSelector:sel1 withObject:@1] == S);
  expectTrue ([D_meta performSelector:sel1 withObject:@1] == D_meta);
  expectTrue ([C_meta performSelector:sel1 withObject:@1] == C_meta);
  expectTrue ([S_meta performSelector:sel1 withObject:@1] == S_meta);


  printf("NSObjectProtocol.performSelector:withObject:withObject:\n");

  SEL sel2 = @selector(perform2::);
  expectTrue ([d performSelector:sel2 withObject:@1 withObject:@2] == d);
  expectTrue ([c performSelector:sel2 withObject:@1 withObject:@2] == c);
  expectTrue ([D performSelector:sel2 withObject:@1 withObject:@2] == D);
  expectTrue ([C performSelector:sel2 withObject:@1 withObject:@2] == C);
  expectTrue ([S performSelector:sel2 withObject:@1 withObject:@2] == S);
  expectTrue ([D_meta performSelector:sel2 
                           withObject:@1 withObject:@2] == D_meta);
  expectTrue ([C_meta performSelector:sel2 
                           withObject:@1 withObject:@2] == C_meta);
  expectTrue ([S_meta performSelector:sel2 
                           withObject:@1 withObject:@2] == S_meta);


  printf("NSObjectProtocol.isProxy\n");

  expectFalse([d isProxy]);
  expectFalse([c isProxy]);
  expectFalse([D isProxy]);
  expectFalse([C isProxy]);
  expectFalse([S isProxy]);
  expectFalse([D_meta isProxy]);
  expectFalse([C_meta isProxy]);
  expectFalse([S_meta isProxy]);


  printf("NSObjectProtocol.retain\n");
  printf("NSObjectProtocol.release\n");
  printf("NSObjectProtocol.autorelease\n");
  printf("NSObjectProtocol.retainCount\n");
  @autoreleasepool {
    expectTrue ([[[d retain] autorelease] retainCount] != 0);
    expectTrue ([[[c retain] autorelease] retainCount] != 0);
    expectTrue ([[[D retain] autorelease] retainCount] != 0);
    expectTrue ([[[C retain] autorelease] retainCount] != 0);
    expectTrue ([[[S retain] autorelease] retainCount] != 0);
    expectTrue ([[[D_meta retain] autorelease] retainCount] != 0);
    expectTrue ([[[C_meta retain] autorelease] retainCount] != 0);
    expectTrue ([[[S_meta retain] autorelease] retainCount] != 0);
  }


  printf("NSObjectProtocol.zone\n");

  expectTrue ([d zone] != nil);
  expectTrue ([c zone] != nil);
  /* FIXME: radar TBD
     expectTrue ([D zone] != nil);
     expectTrue ([C zone] != nil);
     expectTrue ([S zone] != nil);
     expectTrue ([D_meta zone] != nil);
     expectTrue ([C_meta zone] != nil);
     expectTrue ([S_meta zone] != nil);
  */


  //=== Other methods from class NSObject ===//

  // FIXME: untested


  printf("TestSwiftObjectNSObject: %d error%s\n",
         Errors, Errors == 1 ? "" : "s");
  exit(Errors ? 1 : 0);
}
