// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/artifacts)
// RUN: %empty-directory(%t/artifacts/ObjcBuilds)
// RUN: %empty-directory(%t/artifacts/SwiftBuilds)
// RUN: split-file %s %t/src

// RUN: cp %t/src/ObjCAPI.modulemap %t/artifacts/ObjcBuilds/module.modulemap
// RUN: cp %t/src/ObjCAPI.h %t/artifacts/ObjcBuilds/ObjCAPI.h
// RUN: %target-clang -dynamiclib %t/src/ObjCAPI.m -I %t/src -fobjc-arc \
// RUN: -o %t/artifacts/ObjcBuilds/libObjCAPI.dylib -isysroot %sdk -framework Foundation \
// RUN: -fmodules -fmodule-map-file=%t/artifacts/ObjcBuilds/module.modulemap -DINCLUDE_FOO

// RUN: %target-build-swift-dylib(%t/artifacts/SwiftBuilds/%target-library-name(MyCore)) %t/src/Core.swift \
// RUN: -module-name MyCore -emit-module -package-name pkg \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -enable-library-evolution -O -wmo \
// RUN: -I %t/artifacts/ObjcBuilds -L %t/artifacts/ObjcBuilds \
// RUN: -lObjCAPI -Xcc -DINCLUDE_FOO

// RUN: rm -rf %t/artifacts/ObjcBuilds/libObjCAPI.dylib
// RUN: %target-clang -dynamiclib %t/src/ObjCAPI.m -I %t/src -fobjc-arc \
// RUN: -o %t/artifacts/ObjcBuilds/libObjCAPI.dylib -isysroot %sdk -framework Foundation \
// RUN: -fmodules -fmodule-map-file=%t/artifacts/ObjcBuilds/module.modulemap


// RUN: not %target-build-swift-dylib(%t/artifacts/SwiftBuilds/%target-library-name(MyUIA)) %t/src/UIA.swift \
// RUN: -module-name MyUIA -emit-module -package-name pkg \
// RUN: -enable-library-evolution -O -wmo \
// RUN: -I %t/artifacts/SwiftBuilds -L %t/artifacts/SwiftBuilds \
// RUN: -I %t/artifacts/ObjcBuilds -L %t/artifacts/ObjcBuilds \
// RUN: -lMyCore -lObjCAPI -Rmodule-loading \
// RUN: 2>&1 | %FileCheck %s
// CHECK-DAG: error: cannot bypass resilience when accessing 'PkgStructA' because some of its members failed to deserialize
// CHECK-DAG: error: cannot bypass resilience when accessing 'WrapperA' because some of its members failed to deserialize

// RUN: %target-build-swift-dylib(%t/artifacts/SwiftBuilds/%target-library-name(MyUIB)) %t/src/UIB.swift \
// RUN: -module-name MyUIB -emit-module -package-name pkg \
// RUN: -enable-library-evolution -O -wmo \
// RUN: -I %t/artifacts/SwiftBuilds -L %t/artifacts/SwiftBuilds \
// RUN: -I %t/artifacts/ObjcBuilds -L %t/artifacts/ObjcBuilds \
// RUN: -lMyCore -lObjCAPI -Rmodule-loading

// REQUIRES: swift_in_compiler
// REQUIRES: objc_interop


//--- UIA.swift
public import MyCore
public import ObjCAPI

public func testA(_ arg: PubProto) {
  if let x = arg as? PkgStructA {
    let y = x.pkgVar
    print(y)
  }
}

package func testWrapperA(_ arg: WrapperA) {
  let v = arg.varA
  print(v)
}

//--- UIB.swift
public import MyCore
public import ObjCAPI

public func testB(_ arg: PubProto) {
  if let x = arg as? PkgStructB { // no-error
    let y = x.pkgVar
    print(y)
  }
}

package func testWrapperB(_ arg: WrapperB) {
  let v = arg.varB
  print(v)
}

//--- Core.swift
package import ObjCAPI

public protocol PubProto {}

package struct PkgStructA: PubProto {
  package var _pkgVar: FooObjc
  package var pkgVar: AnyObject { _pkgVar.propObjc as AnyObject }
}

package struct PkgStructB: PubProto {
  package var _pkgVar: BarObjc
  package var pkgVar: AnyObject { _pkgVar.propObjc as AnyObject }
}

package struct WrapperA {
  package var varA: PkgStructA?
}

package struct WrapperB {
  package var varB: PkgStructB?
}

//--- ObjCAPI.h
#import <Foundation/Foundation.h>

@interface BazObjc: NSObject
@end

@interface BarObjc: NSObject
@property (nonatomic, strong) BazObjc *propObjc;
@end

#if INCLUDE_FOO
@interface FooObjc: NSObject
@property (nonatomic, strong) BazObjc *propObjc;
@end
#endif

//--- ObjCAPI.m
#include "ObjCAPI.h"

@implementation BazObjc
- (instancetype)init {
  return [super init];
}
@end

@implementation BarObjc
@synthesize propObjc = _propObjc;

- (instancetype)init {
  self = [super init];
  if (self) {
    _propObjc = [BazObjc new];
  }
  return self;
}
@end

#if INCLUDE_FOO
@implementation FooObjc
@synthesize propObjc = _propObjc;

- (instancetype)init {
  self = [super init];
  if (self) {
    _propObjc = [BazObjc new];
  }
  return self;
}
@end
#endif

//--- ObjCAPI.modulemap
module ObjCAPI {
  header "ObjCAPI.h"
  export *
}
