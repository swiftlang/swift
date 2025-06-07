// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-objc-types.swift -module-name UseObjCTy -typecheck -verify -emit-clang-header-path %t/UseObjCTy.h -I %t -enable-experimental-cxx-interop -clang-header-expose-decls=all-public

// RUN: %target-interop-build-clangxx -std=c++20 -fobjc-arc -c %t/use-swift-objc-types.mm -I %t -o %t/swift-objc-execution.o
// RUN: %target-interop-build-swift %t/use-objc-types.swift -o %t/swift-objc-execution -Xlinker %t/swift-objc-execution.o -module-name UseObjCTy -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t

// RUN: %target-codesign %t/swift-objc-execution
// RUN: %target-run %t/swift-objc-execution | %FileCheck %s
// RUN: %target-run %t/swift-objc-execution | %FileCheck --check-prefix=DESTROY %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// REQUIRES: rdar107657204

//--- header.h

#import <Foundation/Foundation.h>

@interface ObjCKlass: NSObject
-(ObjCKlass * _Nonnull) init:(int)x;
-(int)getValue;
@end

@protocol ObjCProtocol
@required
- (int)method;
@end

@interface ObjCKlassConforming : NSObject<ObjCProtocol>
- (ObjCKlassConforming * _Nonnull) init:(int)x;
- (int)method;
@end

//--- module.modulemap
module ObjCTest {
    header "header.h"
}

//--- use-objc-types.swift
import ObjCTest

public func retObjClass() -> ObjCKlass {
    return ObjCKlass(1)
}

public func retObjClassNull() -> ObjCKlass? {
    return nil
}

public func retObjClassNullable() -> ObjCKlass? {
    return retObjClass()
}

public func takeObjCClass(_ x: ObjCKlass) {
    print("OBJClass:", x.getValue());
}

public func takeObjCClassInout(_ x: inout ObjCKlass) {
    x = ObjCKlass(x.getValue() + 1)
}

public func takeObjCClassNullable(_ x: ObjCKlass?) {
    if let x = x {
        print("OBJClass:", x.getValue());
    } else {
        print("NIL");
    }
}

public func passThroughObjClass(_ x: ObjCKlass?) -> ObjCKlass? {
    return x
}

public func takeObjCProtocol(_ x: ObjCProtocol) {
    print("ObjCKlassConforming:", x.method());
}

public func retObjCProtocol() -> ObjCProtocol {
    return ObjCKlassConforming(2)
}

public func retObjCProtocolNullable() -> ObjCProtocol? {
    return ObjCKlassConforming(2)
}

public func retObjClassArray() -> [ObjCKlass] {
    return [ObjCKlass(1)]
}

//--- use-swift-objc-types.mm

#include "header.h"
#include "UseObjCTy.h"
#include <assert.h>
#include <stdio.h>

int globalCounter = 0;

struct DeinitPrinter {
    ~DeinitPrinter() {
        puts("destroy ObjCKlass");
        --globalCounter;
    }
};

@implementation ObjCKlass {
    int _x;
    DeinitPrinter _printer;
}
- (ObjCKlass * _Nonnull) init:(int)x {
    ObjCKlass *result = [super init];
    result->_x = x;
    puts("create ObjCKlass");
    ++globalCounter;
    return result;
}

-(int)getValue {
    return self->_x;
}

@end

struct DeinitPrinter2 {
    ~DeinitPrinter2() {
        puts("destroy ObjCKlassConforming");
        --globalCounter;
    }
};

@implementation ObjCKlassConforming {
    int _x;
    DeinitPrinter2 _printer;
}
- (ObjCKlassConforming * _Nonnull) init:(int)x {
    ObjCKlassConforming *result = [super init];
    result->_x = x;
    puts("create ObjCKlassConforming");
    ++globalCounter;
    return result;
}

-(int)method {
    return self->_x;
}
@end

int main() {
  using namespace UseObjCTy;
  @autoreleasepool {
    ObjCKlass* val = retObjClass();
    assert(val.getValue == 1);
    assert(globalCounter == 1);
    takeObjCClass(val);
    takeObjCClassInout(val);
    takeObjCClassNullable(val);
    takeObjCClassNullable(retObjClassNull());
  }
  assert(globalCounter == 0);
// CHECK: create ObjCKlass
// CHECK-NEXT: OBJClass: 1
// CHECK-NEXT: create ObjCKlass
// CHECK: OBJClass: 2
// CHECK-NEXT: NIL
// DESTROY: destroy ObjCKlass
// DESTROY: destroy ObjCKlass
  puts("Part2");
  @autoreleasepool {
    ObjCKlass* val = retObjClassNullable();
    assert(globalCounter == 1);
    assert(val.getValue == 1);
    takeObjCClassNullable(passThroughObjClass(val));
  }
  assert(globalCounter == 0);
// CHECK: Part2
// CHECK-NEXT: create ObjCKlass
// CHECK-NEXT: OBJClass: 1
// CHECK-NEXT: destroy ObjCKlass
// DESTROY: destroy ObjCKlass
  puts("Part3");
  @autoreleasepool {
    id <ObjCProtocol> val = retObjCProtocol();
    assert(globalCounter == 1);
    assert([val method] == 2);
    takeObjCProtocol(val);
  }
// CHECK: Part3
// CHECK-NEXT: create ObjCKlassConforming
// CHECK-NEXT: ObjCKlassConforming: 2
// CHECK-NEXT: destroy ObjCKlassConforming
// DESTROY: destroy ObjCKlassConforming
  puts("Part4");
  @autoreleasepool {
    swift::Array<ObjCKlass*> val = retObjClassArray();
    assert(val[0].getValue == 1);
    assert(globalCounter == 1);
  }
  assert(globalCounter == 0);
// CHECK: create ObjCKlass
// DESTROY: destroy ObjCKlass
  return 0;
}
