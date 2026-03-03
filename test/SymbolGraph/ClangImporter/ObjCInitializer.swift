// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name ObjCInitializer -Fsystem %t -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/ObjCInitializer.symbols.json
// RUN: %FileCheck %s --input-file %t/ObjCInitializer.symbols.json --check-prefix SYNTH

//--- ObjCInitializer.framework/Modules/module.modulemap
framework module ObjCInitializer [system] {
    header "ObjCInitializer.h"
}

//--- ObjCInitializer.framework/Headers/ObjCInitializer.h
@import Foundation;

@interface NSScrubberLayoutAttributes : NSObject <NSCopying>

// This initializer gets imported twice - once as `init(forItemAt:)` and once as
// `init(forItemAtIndex:)`. Make sure one of them gets a synthesized USR
// CHECK:     "precise": "c:objc(cs)NSScrubberLayoutAttributes(cm)layoutAttributesForItemAtIndex:"
// CHECK-NOT: "precise": "c:objc(cs)NSScrubberLayoutAttributes(cm)layoutAttributesForItemAtIndex:"
// SYNTH:     "precise": "c:objc(cs)NSScrubberLayoutAttributes(cm)layoutAttributesForItemAtIndex:::SYNTHESIZED::c:objc(cs)NSScrubberLayoutAttributes",
// SYNTH-NOT: "precise": "c:objc(cs)NSScrubberLayoutAttributes(cm)layoutAttributesForItemAtIndex:::SYNTHESIZED::c:objc(cs)NSScrubberLayoutAttributes",
+ (instancetype)layoutAttributesForItemAtIndex:(NSInteger)index;

@end
