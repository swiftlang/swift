// REQUIRES: rdar101497120

//
// Build objc_implementation.framework
//
// RUN: %empty-directory(%t-frameworks)
// RUN: %empty-directory(%t-frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule)
// RUN: %empty-directory(%t-frameworks/objc_implementation.framework/Headers)
// RUN: cp %S/Inputs/objc_implementation.modulemap %t-frameworks/objc_implementation.framework/Modules/module.modulemap
// RUN: cp %S/Inputs/objc_implementation.h %t-frameworks/objc_implementation.framework/Headers
// RUN: %target-build-swift-dylib(%t-frameworks/objc_implementation.framework/objc_implementation) -emit-module-path %t-frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule/%module-target-triple.swiftmodule -module-name objc_implementation -F %t-frameworks -import-underlying-module -Xlinker -install_name -Xlinker %t-frameworks/objc_implementation.framework/objc_implementation %S/objc_implementation.swift

//
// Execute this file
//
// RUN: %empty-directory(%t)
// RUN: %target-clang %s -isysroot %sdk -F %t-frameworks -lobjc -fmodules -fobjc-arc -o %t/objc_implementation_objc_client
// RUN: %target-codesign %t/objc_implementation_objc_client
// RUN: %target-run %t/objc_implementation_objc_client 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// FIXME: This test fails in Swift CI simulators, but I have not been able to
//        reproduce this locally.
// REQUIRES: OS=macosx

#import <Foundation/Foundation.h>
#import <objc_implementation/objc_implementation.h>
#import <stdlib.h>

@interface ObjCClientSubclass : ImplClass

@property (assign) NSInteger otherProperty;

- (NSString *)someMethod;

@end

static void print(NSString *str) {
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

static void printInt(NSString *str, NSUInteger num) {
  NSString *fullStr =
    [NSString stringWithFormat:@"%@ %lld", str, (long long)num];
  print(fullStr);
}

int main() {
  [ImplClass runTests];
  // CHECK: someMethod = ImplClass.someMethod()
  // CHECK: implProperty = 0
  // CHECK: implProperty = 42
  // CHECK: someMethod = SwiftSubclass.someMethod()
  // CHECK: implProperty = 0
  // CHECK: implProperty = 42
  // CHECK: otherProperty = 1
  // CHECK: otherProperty = 13
  // CHECK: implProperty = 42

  fflush(stdout);

  ImplClass *impl = [[ImplClass alloc] init];
  print([impl someMethod]);
  // CHECK: ImplClass.someMethod()
  printInt(@"implProperty", impl.implProperty);
  // CHECK: implProperty 0
  impl.implProperty = -2;
  printInt(@"implProperty", impl.implProperty);
  // CHECK: implProperty -2

  ObjCClientSubclass *objcSub = [[ObjCClientSubclass alloc] init];
  print([objcSub someMethod]);
  // CHECK: -[ObjCClientSubclass someMethod]

  printInt(@"implProperty", objcSub.implProperty);
  // CHECK: implProperty 0
  printInt(@"otherProperty", objcSub.otherProperty);
  // CHECK: otherProperty 4
  objcSub.implProperty = 7;
  objcSub.otherProperty = 9;
  printInt(@"implProperty", objcSub.implProperty);
  // CHECK: implProperty 7
  printInt(@"otherProperty", objcSub.otherProperty);
  // CHECK: otherProperty 9

  return 0;
}

@implementation ObjCClientSubclass

- (id)init {
  if (self = [super init]) {
    _otherProperty = 4;
  }
  return self;
}

- (NSString *)someMethod {
  return @"-[ObjCClientSubclass someMethod]";
}

@end
