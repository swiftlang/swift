// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/swift)
// RUN: split-file %s %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -enable-objc-interop -emit-module -module-name FinalMod \
// RUN:   -o %t/swift/FinalMod.swiftmodule %t/FinalMod.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -enable-objc-interop -typecheck -I %t/swift -I %t %t/main.swift

// REQUIRES: asserts
// REQUIRES: objc_interop

// FinalMod-Swift.h stands in for the generated header of the Swift module
// FinalMod; the importer resolves FinalBase to the native Swift class because
// the Swift module FinalMod is the overlay of the Clang module FinalMod.

//--- FinalMod.swift
import Foundation

@objc(FinalBase)
public final class FinalBase: NSObject {}

//--- FinalMod-Swift.h
#import <Foundation.h>

__attribute__((objc_subclassing_restricted))
__attribute__((external_source_symbol(
    language="Swift", defined_in="FinalMod", generated_declaration)))
@interface FinalBase : NSObject
@end

//--- Cat.h
#import "FinalMod-Swift.h"

@interface FinalBase (Cat)
+ (nullable instancetype)newWithValue:(int)value;
+ (nullable instancetype)newWithValue:(int)value other:(int)other;
@end

//--- module.modulemap
module FinalMod {
  header "FinalMod-Swift.h"
  export *
}

module Cat {
  header "Cat.h"
  export *
}

//--- main.swift
import FinalMod
import Cat

func test() {
  // Leave the second overload lazy so AST verification checks it at the end of
  // the pipeline.
  _ = FinalBase.new(withValue: 1)
}
