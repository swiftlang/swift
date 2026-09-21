// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name MultiConformer -I %t/Base -I %t/MultiConformer -output-dir %t -pretty-print -v -emit-extension-block-symbols

// RUN: %FileCheck %s --input-file %t/MultiConformer.symbols.json --check-prefix MAIN
// RUN: %FileCheck %s --input-file %t/MultiConformer@Base.symbols.json --check-prefix EXT

// The requirement is present in the module that declares the protocol.
// MAIN: "precise": "c:objc(pl)MyProtocol(im)initWithSomeNumber:"

// It is not duplicated in the extension graph for the module with conforming types.
// EXT-NOT: "precise": "c:objc(pl)MyProtocol(im)initWithSomeNumber:"

//--- Base/module.modulemap
module Base {
    header "Base.h"
}

//--- Base/Base.h
@import Foundation;

@interface Widget : NSObject
@end

@interface Gadget : NSObject
@end

//--- MultiConformer/module.modulemap
module MultiConformer {
    header "MultiConformer.h"
}

//--- MultiConformer/MultiConformer.h
@import Foundation;
@import Base;

@protocol MyProtocol <NSObject>
- (nullable instancetype)initWithSomeNumber:(NSInteger)number;
@end

@interface Widget (MyProtocol) <MyProtocol> @end
@interface Gadget (MyProtocol) <MyProtocol> @end
