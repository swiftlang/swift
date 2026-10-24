// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/main.swift -emit-ir -o - -cxx-interoperability-mode=default -import-objc-header %t/arc-structs.h -Xcc -fobjc-arc | %FileCheck %s

// REQUIRES: objc_interop

// Referencing an imported @objc protocol whose methods return structs with
// __strong references must not crash swift-frontend in
// ObjCMethodConventions::getResult (the retainable-result assertion).
// The structs import as directly-returned loadable values, and emitting
// ObjC metadata for the protocol must assign them a convention instead of
// asserting.

// The selector references in the emitted ObjC metadata prove emission ran
// for every struct-returning method (the original crash was an assertion
// during exactly this emission).
// CHECK-DAG: L_selector_data(pointInfo)
// CHECK-DAG: L_selector_data(currentInfo)
// CHECK-DAG: L_selector_data(boxedInfo)
// CHECK-DAG: L_selector_data(nestedInfo)
// CHECK-DAG: L_selector_data(cxxBoxedInfo)

//--- arc-structs.h
#import <Foundation/Foundation.h>

// Visible in both C and C++ modes: a plain struct with strong references.
struct ARCPointInfo {
  id _Nullable object;
  double x;
  double y;
};

struct ARCBox {
  NSString *_Nullable name;
  id _Nullable object;
};

struct ARCWrapper {
  struct ARCBox box;
  int depth;
};

#if __cplusplus
// A C++ struct whose only non-triviality is its __strong field. Clang
// treats it as trivial for calls, so it imports as a directly-returned
// loadable value, exercising the same convention path as the C structs.
struct CXXARCBox {
  id _Nullable object;
  int tag;
};
#endif

NS_SWIFT_NAME(ARCStructTestProtocol)
@protocol ARCStructTestProtocol <NSObject>

- (struct ARCPointInfo)pointInfo;

@property (nonatomic, readonly) struct ARCPointInfo currentInfo;

- (struct ARCBox)boxedInfo;

- (struct ARCWrapper)nestedInfo;

#if __cplusplus
- (struct CXXARCBox)cxxBoxedInfo;
#endif

@end

//--- main.swift
import Foundation

public func useProtocol(_ p: ARCStructTestProtocol) {
  _ = p.pointInfo()
  _ = p.currentInfo
  _ = p.boxedInfo()
  _ = p.nestedInfo()
  _ = p.cxxBoxedInfo()
}
