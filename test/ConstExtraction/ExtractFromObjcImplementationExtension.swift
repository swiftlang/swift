// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: echo "[MyProto]" > %t/protocols.json

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractFromObjcImplementationExtension.swiftconstvalues -const-gather-protocols-file %t/protocols.json %s -import-objc-header %S/Inputs/objc_implementation.h -disable-objc-attr-requires-foundation-module -target %target-stable-abi-triple
// RUN: cat %t/ExtractFromObjcImplementationExtension.swiftconstvalues 2>&1 | %FileCheck %s

protocol MyProto { }

extension ImplClass: MyProto { 
    static let notStoredProperty = true
}

@_objcImplementation extension ImplClass {
    @objc var defaultIntProperty: CInt = 17
    final weak var defaultNilProperty: AnyObject?
}

// CHECK: "typeName": "__ObjC.ImplClass",
// CHECK: "properties": [
// CHECK:   "label": "defaultIntProperty",
// CHECK:   "type": "Swift.Int32",
// CHECK:   "value": "17"

// CHECK:   "label": "defaultNilProperty",
// CHECK:   "type": "Swift.Optional<AnyObject>",
// CHECK:   "valueKind": "NilLiteral"

// CHECK:   "label": "notStoredProperty",
// CHECK:   "type": "Swift.Bool",
// CHECK:   "value": "true"
