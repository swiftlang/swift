// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ProtocolClassInheritance -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ProtocolClassInheritance -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ProtocolClassInheritance.symbols.json

// When a protocol that declares a class inheritance requirement is added by an extension, make sure
// that SymbolGraphGen does not crash (rdar://109418762)

public class BaseClass {}

public protocol MyProtocol: BaseClass {}

public class MyClass: BaseClass {}

extension MyClass: MyProtocol {}

// CHECK:      "kind": "conformsTo",
// CHECK-NEXT: "source": "s:24ProtocolClassInheritance02MyB0C",
// CHECK-NEXT: "target": "s:24ProtocolClassInheritance02MyA0P"
