// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name HeaderCollision -F %t -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/HeaderCollision.symbols.json --check-prefix MODULE
// RUN: %FileCheck %s --input-file %t/HeaderCollision@Dependency.symbols.json --check-prefix EXTENSION

// REQUIRES: objc_interop

// Ensure that extensions to a dependency's types, declared in a header with the same name as the
// dependency's type's header, correctly get sorted into an extension header rather than the module
// itself.

// MODULE: "symbols": []
// EXTENSION: "precise": "c:objc(cs)DependencyClass(im)addNumber:to:"

//--- Dependency.framework/Modules/module.modulemap
framework module Dependency {
    umbrella header "Dependency.h"
    export *
    module * { export * }
}

//--- Dependency.framework/Headers/Dependency.h
#import <Dependency/DependencyClass.h>

//--- Dependency.framework/Headers/DependencyClass.h
@import Foundation;

@class DependencyClass;

@interface DependencyClass : NSObject

@end

//--- HeaderCollision.framework/Modules/module.modulemap
framework module HeaderCollision {
    umbrella header "HeaderCollision.h"
    export *
    module * { export * }
}

//--- HeaderCollision.framework/Headers/HeaderCollision.h
#import <HeaderCollision/DependencyClass.h>

//--- HeaderCollision.framework/Headers/DependencyClass.h
#import <Dependency/DependencyClass.h>

@interface DependencyClass (HeaderCollisionClassAdditions)

- (NSInteger)addNumber:(NSInteger)x to:(NSInteger)y;

@end
