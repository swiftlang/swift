// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-sib -emit-module-path %t/Lib.swiftmodule \
// RUN:   -module-name Lib -target wasm32-unknown-wasip1 \
// RUN:   -enable-experimental-feature Extern -parse-stdlib %t/Lib.swift -o %t/Lib.sib
// RUN: %target-swift-frontend -emit-ir -I %t -target wasm32-unknown-wasip1 \
// RUN:   -enable-experimental-feature Extern -parse-stdlib %t/Client.swift | %FileCheck %s

// Check that wasm import names are preserved in the serialized SIL
// RUN: %target-swift-frontend -emit-sib -I %t -target wasm32-unknown-wasip1 \
// RUN:   -enable-experimental-feature Extern -parse-stdlib %t/Client.swift -o %t/Client.sib
// RUN: %target-swift-frontend -emit-ir -I %t -target wasm32-unknown-wasip1 \
// RUN:   -enable-experimental-feature Extern -parse-stdlib %t/Client.sib | %FileCheck %s

// REQUIRES: swift_feature_Extern
// REQUIRES: CODEGENERATOR=WebAssembly

//--- Lib.swift
@_extern(wasm, module: "mod", name: "foo") public func foo()
@_extern(wasm, module: "modOnly", name: "") public func modOnly()
@_extern(wasm, module: "", name: "nameOnly") public func nameOnly()

//--- Client.swift
import Lib
public func use() {
    foo()
    modOnly()
    nameOnly()
}

// CHECK: declare swiftcc void @"$s3Lib3fooyyF"() #[[ATTR:[0-9]+]]
// CHECK: declare swiftcc void @"$s3Lib7modOnlyyyF"() #[[ATTR_MOD_ONLY:[0-9]+]]
// CHECK: declare swiftcc void @"$s3Lib8nameOnlyyyF"() #[[ATTR_NAME_ONLY:[0-9]+]]
// CHECK: attributes #[[ATTR]] = {{.*}}"wasm-import-module"="mod"{{.*}}"wasm-import-name"="foo"
// CHECK: attributes #[[ATTR_MOD_ONLY]] = {{.*}}"wasm-import-module"="modOnly"{{.*}}
// CHECK: attributes #[[ATTR_NAME_ONLY]] = {{.*}}"wasm-import-name"="nameOnly"{{.*}}
