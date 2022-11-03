// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ConditionalConformance -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ConditionalConformance -I %t -pretty-print -output-dir %t

// R\UN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=SYNTH
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=CONFORMS
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=MEMBER

// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=SYNTHEXT,EBSOff_SYNTHEXT
// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=CONFORMSEXT,EBSOff_CONFORMSEXT
// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=MEMBEREXT,EBSOff_MEMBEREXT

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ConditionalConformance -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ConditionalConformance -I %t -pretty-print -output-dir %t -emit-extension-block-symbols

// R\UN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=SYNTH
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=CONFORMS
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=MEMBER

// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=SYNTHEXT,EBSOn_SYNTHEXT
// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=CONFORMSEXT,EBSOn_CONFORMSEXT
// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=MEMBEREXT,EBSOn_MEMBEREXT

// Relationships to Swift.Array should only go into the @Swift file.
// C\HECK-NOT: "s:Sa"

public protocol P {
  func foo()
}

extension P {
  public func foo() {}
}

public struct S<T> {
  var x: T
  public init(x: T) {
    self.x = x
  }
}

// CONFORMS: "kind": "conformsTo"
// CONFORMS-NEXT: "source": "s:22ConditionalConformance1SV"
// CONFORMS-NEXT: "target": "s:22ConditionalConformance1PP"
// CONFORMS-NEXT: swiftConstraints
// CONFORMS: "kind": "sameType"
// CONFORMS-NEXT: "lhs": "T"
// CONFORMS-NEXT: "rhs": "Int"

extension S: P where T == Int {
  // SYNTH: "source": "s:22ConditionalConformance1PPAAE3fooyyF::SYNTHESIZED::s:22ConditionalConformance1SV"
  // SYNTH-NEXT: "target": "s:22ConditionalConformance1SV"

  // MEMBER: "source": "s:22ConditionalConformance1SVAASiRszlE3baryyF",
  // MEMBER-NEXT: "target": "s:22ConditionalConformance1SV"
  public func bar() {
    foo()
  }
}

// CONFORMSEXT: "kind": "conformsTo"
// EBSOff_CONFORMSEXT-NEXT: "source": "s:Sa"
// EBSOn_CONFORMSEXT-NEXT: "source": "s:e:s:Sa22ConditionalConformanceSiRszlE3baryyF"
// CONFORMSEXT-NEXT: "target": "s:22ConditionalConformance1PP"
// CONFORMSEXT-NEXT: swiftConstraints
// CONFORMSEXT: "kind": "sameType"
// CONFORMSEXT-NEXT: "lhs": "Element"
// CONFORMSEXT-NEXT: "rhs": "Int"

extension Array: P where Element == Int {
  // SYNTHEXT: "source": "s:22ConditionalConformance1PPAAE3fooyyF::SYNTHESIZED::s:Sa"
  // EBSOff_SYNTHEXT-NEXT: "target": "s:Sa"
  // EBSOn_SYNTHEXT-NEXT: "target": "s:e:s:Sa22ConditionalConformanceSiRszlE3baryyF"

  // MEMBEREXT: "source": "s:Sa22ConditionalConformanceSiRszlE3baryyF"
  // EBSOff_MEMBEREXT-NEXT: "target": "s:Sa"
  // EBSOn_MEMBEREXT-NEXT: "target": "s:e:s:Sa22ConditionalConformanceSiRszlE3baryyF"
  public func bar() {}
}
