// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/frameworks/OptionalRequirementOf.framework/Modules/OptionalRequirementOf.swiftmodule
// RUN: split-file %s %t

// RUN: %target-build-swift %t/reqs.swift -module-name OptionalRequirementOf -emit-module-path %t/frameworks/OptionalRequirementOf.framework/Modules/OptionalRequirementOf.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t/frameworks -Xfrontend -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-symbolgraph-extract -module-name OptionalRequirementOf -F %t/frameworks -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/OptionalRequirementOf.symbols.json

// CHECK-NOT: "kind": "requirementOf"

// ObjCProto.objcReq -> ObjCProto
// CHECK-DAG: "kind": "optionalRequirementOf",{{[[:space:]]*}}"source": "c:@M@OptionalRequirementOf@objc(pl)SwiftProto(im)swiftReq",{{[[:space:]]*}}"target": "c:@M@OptionalRequirementOf@objc(pl)SwiftProto"

// SwiftProto.swiftReq -> SwiftProto
// CHECK-DAG: "kind": "optionalRequirementOf",{{[[:space:]]*}}"source": "c:objc(pl)ObjCProto(im)objcReq",{{[[:space:]]*}}"target": "c:objc(pl)ObjCProto"

//--- reqs.swift
@objc
public protocol SwiftProto {
  @objc optional func swiftReq()
}

//--- frameworks/OptionalRequirementOf.framework/Modules/module.modulemap
framework module OptionalRequirementOf {
  header "req.h"
  export *
}

//--- frameworks/OptionalRequirementOf.framework/Headers/req.h
@protocol ObjCProto
@optional
- (void)objcReq;
@end
