// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-synthesize-interface -module-name ObjCDecls -F %t/Inputs/Frameworks | %FileCheck %s

//--- Inputs/Frameworks/ObjCDecls.framework/Modules/module.modulemap
framework module ObjCDecls {
  header "ObjCDecls.h"
  export *
}

//--- Inputs/Frameworks/ObjCDecls.framework/Headers/ObjCDecls.h
@import Foundation;

__attribute__((objc_root_class))
@interface RootThing
- (void)greet;
@end
// Instance methods on root classes get mirrored as class methods
// CHECK: open class RootThing {
// CHECK:     open class func greet()
// CHECK:     open func greet()
// CHECK: }

@protocol Greetable <NSObject>
- (instancetype)initWithName:(NSString *)name;
@end

@interface Greetee : NSObject <Greetable>
@end
// When a class adopts a protocol with a required init, the init is mirrored
// onto the class.
// CHECK: open class Greetee : NSObject, Greetable {
// CHECK:     public init()
// CHECK:     public init!(name: String!)
// CHECK: }
