// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Generate the Objective-C header for the Swift @objc global functions.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %t/Lib.swift -emit-module -verify -o %t \
// RUN:   -emit-objc-header-path %t/Lib.h

/// Build and run a binary mixing Swift and Objective-C code.
// RUN: %target-clang -fobjc-arc -c %t/Client.m -o %t/Client.o \
// RUN:   -isysroot %sdk -I %t
// RUN: %target-build-swift %t/Lib.swift %t/Client.o -o %t/a.out -parse-as-library
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out > %t/run.log
// RUN: %FileCheck %s --input-file %t/run.log

// REQUIRES: executable_test
// REQUIRES: objc_interop

//--- Lib.swift

import Foundation

@objc(addOne) public func addOne(_ x: Int) -> Int {
    return x + 1
}

@objc(greeting) public func greeting(_ name: NSString) -> NSString {
    return "Hi \(name)" as NSString
}

// Bridged type: String <-> NSString
@objc(greetSwift) public func greetSwift(_ name: String) -> String {
    return "Hello \(name)"
}

// Bridged collection: [NSObject] <-> NSArray
@objc(countItems) public func countItems(_ items: [NSObject]) -> Int {
    return items.count
}

//--- Client.m

#import <Foundation/Foundation.h>
#import "Lib.h"

int main() {
    printf("%ld\n", (long)addOne(41));
    // CHECK: 42

    NSString *g = greeting(@"world");
    printf("%s\n", g.UTF8String);
    // CHECK-NEXT: Hi world

    // Bridged String: caller passes NSString, thunk bridges to Swift String
    NSString *gs = greetSwift(@"Swift");
    printf("%s\n", gs.UTF8String);
    // CHECK-NEXT: Hello Swift

    // Bridged Array: caller passes NSArray, thunk bridges to [NSObject]
    NSArray *items = @[@"a", @"b", @"c"];
    printf("%ld\n", (long)countItems(items));
    // CHECK-NEXT: 3

    return 0;
}
