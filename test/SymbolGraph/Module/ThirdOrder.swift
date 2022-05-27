// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/ThirdOrder/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend %S/Inputs/ThirdOrder/B.swift -module-name B -emit-module -emit-module-path %t/B.swiftmodule -I %t
// RUN: %target-swift-frontend %s -module-name ThirdOrder -emit-module -emit-module-path %t/ThirdOrder.swiftmodule -I %t -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %FileCheck %s --input-file %t/ThirdOrder.symbols.json --check-prefix BASE
// RUN: %FileCheck %s --input-file %t/ThirdOrder@A.symbols.json --check-prefix EXT

// Module B extends a symbol from module A that includes a synthesized symbol.
// To ensure that we track source modules correctly, we need to make sure that
// the synthesized equality operators don't appear in the ThirdOrder symbol graph.

@_exported import B

// BASE-NOT: "s:SQsE2neoiySbx_xtFZ::SYNTHESIZED::s:1A10SomeStructV1BE05InnerB0V"
// EXT: "s:SQsE2neoiySbx_xtFZ::SYNTHESIZED::s:1A10SomeStructV1BE05InnerB0V"
