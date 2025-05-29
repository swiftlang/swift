//
// Build objc_implementation.framework
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/objc_implementation.framework/Modules/objc_implementation.swiftmodule)
// RUN: %empty-directory(%t/objc_implementation.framework/Headers)
// RUN: cp %S/Inputs/objc_implementation.modulemap %t/objc_implementation.framework/Modules/module.modulemap
// RUN: cp %S/Inputs/objc_implementation.h %t/objc_implementation.framework/Headers
// RUN: %target-build-swift-dylib(%t/objc_implementation.framework/objc_implementation) -emit-module-path %t/objc_implementation.framework/Modules/objc_implementation.swiftmodule/%module-target-triple.swiftmodule -module-name objc_implementation -F %t -import-underlying-module -Xlinker -install_name -Xlinker @executable_path/objc_implementation.framework/objc_implementation %S/objc_implementation.swift -enable-experimental-feature CImplementation -target %target-stable-abi-triple

//
// Execute this file
//
// RUN: %target-clang %s -isysroot %sdk -F %t -lobjc -fmodules -fobjc-arc -o %t/objc_implementation_objc_client
// RUN: %target-codesign %t/objc_implementation_objc_client
// Note: we pass the .framework as an argument so that remote-run will copy it across when running tests remotely.
// RUN: %target-run %t/objc_implementation_objc_client %t/objc_implementation.framework 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// FIXME: This test fails in Swift CI simulators, but I have not been able to
//        reproduce this locally.
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_CImplementation

#import <Foundation/Foundation.h>
#import <objc_implementation/objc_implementation.h>
#import <stdlib.h>

@interface ObjCClientSubclass : ImplClass

@property (assign) NSInteger otherProperty;

- (NSString *)someMethod;

@end

static void print(NSString *str) {
  // Flush any buffered Swift output.
  fflush(stdout);

  NSData *strData = [str dataUsingEncoding:NSUTF8StringEncoding];
  NSData *nlData = [@"\n" dataUsingEncoding:NSUTF8StringEncoding];

  NSFileHandle *out = NSFileHandle.fileHandleWithStandardOutput;

  if (@available(macOS 10.15, iOS 13.0, watchOS 6.0, *)) {
    NSError *error;
    if (![out writeData:strData error:&error]
        || ![out writeData:nlData error:&error]) {
      NSLog(@"I/O error writing to stdout: %@", error);
      exit(EXIT_FAILURE);
    }
  }
  else {
    [out writeData:strData];
    [out writeData:nlData];
  }
}

static void printInt(Class class, NSString *property, NSUInteger num) {
  NSString *fullStr =
    [NSString stringWithFormat:@"%@.%@ = %lld", class, property, (long long)num];
  print(fullStr);
}

static void printString(Class class, NSString *property, NSString *str) {
  NSString *fullStr =
    [NSString stringWithFormat:@"%@.%@ = %@", class, property, str];
  print(fullStr);
}

int main() {
  [ImplClass runTests];
  // CHECK: implFunc(1989)
  // CHECK-LABEL: *** ImplClass init ***
  // CHECK: ImplClass.init()
  // CHECK-RESILIENCE-LABEL: *** ImplClassWithResilientStoredProperty #1 ***
  // CHECK-RESILIENCE: ImplClassWithResilientStoredProperty.mirror = nil
  // CHECK-RESILIENCE: ImplClassWithResilientStoredProperty.afterMirrorProperty = 0
  // CHECK-RESILIENCE-LABEL: *** ImplClassWithResilientStoredProperty #2 ***
  // CHECK-RESILIENCE: ImplClassWithResilientStoredProperty.mirror = nil
  // CHECK-RESILIENCE: ImplClassWithResilientStoredProperty.afterMirrorProperty = 42
  // CHECK-LABEL: *** ImplClass #1 ***
  // CHECK: ImplClass.someMethod() = ImplClass
  // CHECK: ImplClass.implProperty = 0
  // CHECK: ImplClass.defaultIntProperty = 17
  // CHECK: ImplClass.description = ImplClass(implProperty: 0, object: objc_implementation.LastWords)
  // CHECK-LABEL: *** ImplClass #2 ***
  // CHECK: ImplClass.someMethod() = ImplClass
  // CHECK: ImplClass.implProperty = 42
  // CHECK: ImplClass.defaultIntProperty = 17
  // CHECK: ImplClass.description = ImplClass(implProperty: 42, object: objc_implementation.LastWords)
  // CHECK-LABEL: *** ImplClass end ***
  // CHECK: ImplClass It's better to burn out than to fade away.
  // CHECK-LABEL: *** SwiftSubclass init ***
  // CHECK: SwiftSubclass.init()
  // CHECK: ImplClass.init()
  // CHECK-RESILIENCE-LABEL: *** SwiftResilientStoredSubclass #1 ***
  // CHECK-RESILIENCE: SwiftResilientStoredSubclass.mirror = nil
  // CHECK-RESILIENCE: SwiftResilientStoredSubclass.afterMirrorProperty = 0
  // CHECK-RESILIENCE: SwiftResilientStoredSubclass.mirror2 = nil
  // CHECK-RESILIENCE: SwiftResilientStoredSubclass.afterMirrorProperty2 = 1
  // CHECK-RESILIENCE-LABEL: *** SwiftResilientStoredSubclass #2 ***
  // CHECK-RESILIENCE: SwiftResilientStoredSubclass.mirror = nil
  // CHECK-RESILIENCE: SwiftResilientStoredSubclass.afterMirrorProperty = 42
  // CHECK-RESILIENCE: SwiftResilientStoredSubclass.mirror2 = nil
  // CHECK-RESILIENCE: SwiftResilientStoredSubclass.afterMirrorProperty2 = 43
  // CHECK-LABEL: *** SwiftSubclass #1 ***
  // CHECK: SwiftSubclass.someMethod() = SwiftSubclass
  // CHECK: SwiftSubclass.implProperty = 0
  // CHECK: SwiftSubclass.defaultIntProperty = 17
  // CHECK: SwiftSubclass.description = ImplClass(implProperty: 0, object: objc_implementation.LastWords)
  // CHECK: SwiftSubclass.otherProperty = 1
  // CHECK-LABEL: *** SwiftSubclass #2 ***
  // CHECK: SwiftSubclass.someMethod() = SwiftSubclass
  // CHECK: SwiftSubclass.implProperty = 42
  // CHECK: SwiftSubclass.defaultIntProperty = 17
  // CHECK: SwiftSubclass.description = ImplClass(implProperty: 42, object: objc_implementation.LastWords)
  // CHECK: SwiftSubclass.otherProperty = 1
  // CHECK-LABEL: *** SwiftSubclass #3 ***
  // CHECK: SwiftSubclass.someMethod() = SwiftSubclass
  // CHECK: SwiftSubclass.implProperty = 42
  // CHECK: SwiftSubclass.defaultIntProperty = 17
  // CHECK: SwiftSubclass.description = ImplClass(implProperty: 42, object: objc_implementation.LastWords)
  // CHECK: SwiftSubclass.otherProperty = 13
  // CHECK-LABEL: *** SwiftSubclass end ***
  // CHECK: SwiftSubclass It's better to burn out than to fade away.

  {
    print(@"*** ObjCClientSubclass init ***");
    ObjCClientSubclass *objcSub = [[ObjCClientSubclass alloc] init];
    [objcSub testSelf];
    print(@"*** ObjCClientSubclass end ***");
  }
  // CHECK-LABEL: *** ObjCClientSubclass init ***
  // CHECK: -[ObjCClientSubclass init]
  // CHECK: ImplClass.init()
  // CHECK-RESILIENCE-LABEL: *** ObjCClientResilientSubclass #1 ***
  // CHECK-RESILIENCE: ObjCClientResilientSubclass.mirror = nil
  // CHECK-RESILIENCE: ObjCClientResilientSubclass.afterMirrorProperty = 0
  // CHECK-RESILIENCE: ObjCClientResilientSubclass.subclassProperty = 100
  // CHECK-RESILIENCE-LABEL: *** ObjCClientResilientSubclass #2 ***
  // CHECK-RESILIENCE: ObjCClientResilientSubclass.mirror = nil
  // CHECK-RESILIENCE: ObjCClientResilientSubclass.afterMirrorProperty = 42
  // CHECK-RESILIENCE: ObjCClientResilientSubclass.subclassProperty = 101
  // CHECK-LABEL: *** ObjCClientSubclass #1 ***
  // CHECK: ObjCClientSubclass.someMethod() = ObjCClientSubclass
  // CHECK: ObjCClientSubclass.implProperty = 0
  // CHECK: ObjCClientSubclass.defaultIntProperty = 17
  // CHECK: ObjCClientSubclass.description = ImplClass(implProperty: 0, object: objc_implementation.LastWords)
  // CHECK: ObjCClientSubclass.otherProperty = 4
  // CHECK-LABEL: *** ObjCClientSubclass #2 ***
  // CHECK: ObjCClientSubclass.someMethod() = ObjCClientSubclass
  // CHECK: ObjCClientSubclass.implProperty = 42
  // CHECK: ObjCClientSubclass.defaultIntProperty = 17
  // CHECK: ObjCClientSubclass.description = ImplClass(implProperty: 42, object: objc_implementation.LastWords)
  // CHECK: ObjCClientSubclass.otherProperty = 4
  // CHECK-LABEL: *** ObjCClientSubclass #3 ***
  // CHECK: ObjCClientSubclass.someMethod() = ObjCClientSubclass
  // CHECK: ObjCClientSubclass.implProperty = 42
  // CHECK: ObjCClientSubclass.defaultIntProperty = 17
  // CHECK: ObjCClientSubclass.description = ImplClass(implProperty: 42, object: objc_implementation.LastWords)
  // CHECK: ObjCClientSubclass.otherProperty = 6
  // CHECK-LABEL: *** ObjCClientSubclass end ***
  // CHECK: ObjCClientSubclass It's better to burn out than to fade away.

  fflush(stdout);

  ImplClass *impl = [[ImplClass alloc] init];
  printInt([impl class], @"implProperty", impl.implProperty);
  impl.implProperty = -2;
  printInt([impl class], @"implProperty", impl.implProperty);
  // CHECK: ImplClass.implProperty = 0
  // CHECK: ImplClass.implProperty = -2

  ObjCClientSubclass *objcSub = [[ObjCClientSubclass alloc] init];
  print([objcSub someMethod]);
  // CHECK: ObjCClientSubclass

  printInt([objcSub class], @"implProperty", objcSub.implProperty);
  printInt([objcSub class], @"otherProperty", objcSub.otherProperty);
  objcSub.implProperty = 7;
  objcSub.otherProperty = 9;
  printInt([objcSub class], @"implProperty", objcSub.implProperty);
  printInt([objcSub class], @"otherProperty", objcSub.otherProperty);
  // CHECK: ObjCClientSubclass.implProperty = 0
  // CHECK: ObjCClientSubclass.otherProperty = 4
  // CHECK: ObjCClientSubclass.implProperty = 7
  // CHECK: ObjCClientSubclass.otherProperty = 9

  return 0;
}

#if RESILIENCE
@interface ObjCClientResilientSubclass : ImplClassWithResilientStoredProperty

@property (assign) NSInteger subclassProperty;

@end
#endif

@implementation ObjCClientSubclass

- (id)init {
  print(@"-[ObjCClientSubclass init]");
  if (self = [super init]) {
    _otherProperty = 4;
  }
  return self;
}

- (void)testSelf {
  [super testSelf];
  self.otherProperty = 6;
  [self printSelfWithLabel:3];
}

- (void)printSelfWithLabel:(int)label {
  [super printSelfWithLabel:label];
  printInt([self class], @"otherProperty", self.otherProperty);
}

- (NSString *)someMethod {
  return @"ObjCClientSubclass";
}

#if RESILIENCE
+ (ImplClassWithResilientStoredProperty *)makeResilientImpl {
  return [ObjCClientResilientSubclass new];
}
#endif

@end

#if RESILIENCE
@implementation ObjCClientResilientSubclass

- (id)init {
  if (self = [super init]) {
    _subclassProperty = 100;
  }
  return self;
}

- (void)printSelfWithLabel:(int)label {
  [super printSelfWithLabel:label];
  printInt([self class], @"subclassProperty", self.subclassProperty);
}

- (void)mutate {
  [super mutate];
  self.subclassProperty = 101;
}

@end
#endif
