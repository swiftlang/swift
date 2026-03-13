// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/artifacts)
// RUN: %empty-directory(%t/artifacts/ObjcBuilds)
// RUN: %empty-directory(%t/artifacts/SwiftBuilds)
// RUN: split-file %s %t/src

/// Build a clang module with `INCLUDE_FOO`
// RUN: cp %t/src/ObjCAPI.modulemap %t/artifacts/ObjcBuilds/module.modulemap
// RUN: cp %t/src/ObjCAPI.h %t/artifacts/ObjcBuilds/ObjCAPI.h
// RUN: %target-clang -dynamiclib %t/src/ObjCAPI.m -I %t/src -fobjc-arc \
// RUN: -o %t/artifacts/ObjcBuilds/libObjCAPI.dylib -isysroot %sdk -framework Foundation \
// RUN: -fmodules -fmodule-map-file=%t/artifacts/ObjcBuilds/module.modulemap -DINCLUDE_FOO

/// Build a swift module that depends on the clang module with `INCLUDE_FOO`
// RUN: %target-build-swift-dylib(%t/artifacts/SwiftBuilds/%target-library-name(MyCore)) %t/src/Core.swift \
// RUN: -module-name MyCore -emit-module -package-name pkg \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -enable-library-evolution -O -wmo \
// RUN: -I %t/artifacts/ObjcBuilds -L %t/artifacts/ObjcBuilds \
// RUN: -lObjCAPI -Xcc -DINCLUDE_FOO

/// Build a swift module that depends on the above Core swift module without `INCLUDE_FOO`;
/// it should fail and diagnose that there was a deserialization failure.
// RUN: not %target-build-swift-dylib(%t/artifacts/SwiftBuilds/%target-library-name(MyUIA)) %t/src/UIA.swift \
// RUN: -module-name MyUIA -emit-module -package-name pkg \
// RUN: -enable-library-evolution -O -wmo \
// RUN: -Xfrontend -experimental-package-cmo-abort-on-deserialization-fail \
// RUN: -I %t/artifacts/SwiftBuilds -L %t/artifacts/SwiftBuilds \
// RUN: -I %t/artifacts/ObjcBuilds -L %t/artifacts/ObjcBuilds \
// RUN: -lMyCore -lObjCAPI -Rmodule-loading 2>&1 | tee %t/MyUIA-build.txt 
// RUN: %FileCheck %s --check-prefix=CHECK-ERROR < %t/MyUIA-build.txt
// CHECK-ERROR: error: cannot bypass resilience due to member deserialization failure while attempting to access missing member of 'PkgStructA' in module 'MyCore' from module 'MyUIA'

/// Build another swift module that depends on Core; since FooObjc is not referenced
/// in the call chain, there's no deserialization failure, and bypassing resilience is allowed.
// RUN: %target-build-swift-dylib(%t/artifacts/SwiftBuilds/%target-library-name(MyUIB)) %t/src/UIB.swift \
// RUN: -module-name MyUIB -emit-module -package-name pkg \
// RUN: -enable-library-evolution -O -wmo \
// RUN: -I %t/artifacts/SwiftBuilds -L %t/artifacts/SwiftBuilds \
// RUN: -I %t/artifacts/ObjcBuilds -L %t/artifacts/ObjcBuilds \
// RUN: -lMyCore -lObjCAPI -Rmodule-loading

// REQUIRES: swift_in_compiler
// REQUIRES: objc_interop


//--- UIA.swift
package import MyCore
import ObjCAPI

package func testWrapperA(_ arg: WrapperA) {
  if let x = arg.varA, let y = arg.varAdict, let z = arg.varAarray {
      print(x, y, z)
  }
}

package func testKlass(_ arg: Klass) {
  print(arg.kStr, arg.kVar)
}

//--- UIB.swift
public import MyCore
import ObjCAPI

package func testWrapperB(_ arg: WrapperB) {
  if let v = arg.varB {
    print(v)
  }
}

package func testKlass(_ arg: Klass) {
  print(arg.kStr, arg.kVar)
}

//--- Core.swift
package import ObjCAPI

package struct WrapperA {
  package var varA: PkgStructA?
  package var varAdict: [String: PkgStructA]?
  package var varAarray: [PkgStructA]?
}

package struct WrapperB {
  package var varB: PkgStructB?
  package var varBstr: String?
}

public protocol PubProto {}

package struct PkgStructA: PubProto {
  package var _pkgVar: FooObjc
  package var pkgVar: AnyObject { _pkgVar.propObjc as AnyObject }
}

package struct PkgStructB: PubProto {
  package var _pkgVar: BarObjc
  package var pkgVar: AnyObject { _pkgVar.propObjc as AnyObject }
}


open class Klass {
  package var kVar: Klass
  package var kStr: String
  package init(arg: Klass) {
    self.kVar = arg
    self.kStr = "asdf"
  }
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
