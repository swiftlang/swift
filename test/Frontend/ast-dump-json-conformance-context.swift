// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/SomeProtocolModule.swift -parse-as-library -emit-module -emit-module-path %t/SomeProtocolModule.swiftmodule -module-name SomeProtocolModule
// RUN: %target-swift-frontend %t/SomeTypeModule.swift -parse-as-library -emit-module -emit-module-path %t/SomeTypeModule.swiftmodule -module-name SomeTypeModule
// RUN: %target-swift-frontend %t/SomeTypeConformanceModule.swift -I %t -parse-as-library -emit-module -emit-module-path %t/SomeTypeConformanceModule.swiftmodule -module-name SomeTypeConformanceModule
// RUN: %target-swift-frontend %t/Client.swift -I %t -parse-as-library -dump-ast -dump-ast-format json -module-name Client -o - > %t/Client.json
// RUN: %{python} -c 'import json, sys; print(json.dumps(json.load(sys.stdin), indent=4))' < %t/Client.json | %FileCheck %s

//--- SomeProtocolModule.swift
public protocol SomeProtocol {}

//--- SomeTypeModule.swift
public struct SomeType {
    public init() {}
}

//--- SomeTypeConformanceModule.swift
import SomeProtocolModule
import SomeTypeModule

extension SomeType: @retroactive SomeProtocol {}

//--- Client.swift
import SomeProtocolModule
import SomeTypeConformanceModule
import SomeTypeModule

func g<T: SomeProtocol>(_ t: T) {}

func f() {
    g(SomeType())
}

// CHECK:      "substitutions": {
// CHECK-NEXT:     "_kind": "substitution_map",
// CHECK:          "reqs": [
// CHECK:              {
// CHECK-NEXT:             "_kind": "conformance",
// CHECK-NEXT:             "type": "$sxD",
// CHECK-NEXT:             "conformance": {
// CHECK-NEXT:                 "_kind": "normal_conformance",
// CHECK-NEXT:                 "type": "$s14SomeTypeModule0aB0VD",
// CHECK-NEXT:                 "protocol": "s:18SomeProtocolModule0aB0P",
// CHECK:                      "context": {
// CHECK-NEXT:                     "_kind": "conformance_context",
// CHECK-NEXT:                     "decl": "s:e:s:14SomeTypeModule0aB0Vs:18SomeProtocolModule0aB0P",
// CHECK-NEXT:                     "module": "SomeTypeConformanceModule"
// CHECK-NEXT:                 }
// CHECK-NEXT:             }
// CHECK-NEXT:         }
// CHECK-NEXT:     ]
// CHECK-NEXT: }
