// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name ProtocolInitializer -I %t/ProtocolInitializer -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/ProtocolInitializer.symbols.json

// the initializer's base USR should only appear as the source of one relationship
// CHECK: "source": "c:objc(pl)MyProtocol(im)initWithSomeNumber:"
// CHECK-NOT: "source": "c:objc(pl)MyProtocol(im)initWithSomeNumber:"

//--- ProtocolInitializer/module.modulemap
module ProtocolInitializer {
    header "ProtocolInitializer.h"
}

//--- ProtocolInitializer/ProtocolInitializer.h
@import Foundation;

@protocol MyProtocol <NSObject>

@optional
- (nullable id)initWithSomeNumber:(NSInteger)number;

@end

@interface MyClass : NSObject
@end

@interface MyClass () <MyProtocol>
@end
