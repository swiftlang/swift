// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ConditionalConformance -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ConditionalConformance -I %t -pretty-print -output-dir %t

// R\UN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=SYNTH
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=CONFORMS
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=MEMBER

// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=SYNTHEXT -DEXTID=s:Sa
// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=CONFORMSEXT -DEXTID=s:Sa
// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=MEMBEREXT -DEXTID=s:Sa

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ConditionalConformance -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ConditionalConformance -I %t -pretty-print -output-dir %t -emit-extension-block-symbols

// R\UN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=SYNTH
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=CONFORMS
// RUN: %FileCheck %s --input-file %t/ConditionalConformance.symbols.json --check-prefix=MEMBER

// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=SYNTHEXT -DEXTID=s:e:s:Sa22ConditionalConformanceSiRszlE3baryyF
// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=CONFORMSEXT -DEXTID=s:e:s:Sa22ConditionalConformanceSiRszlE3baryyF
// RUN: %FileCheck %s --input-file %t/ConditionalConformance@Swift.symbols.json --check-prefixes=MEMBEREXT -DEXTID=s:e:s:Sa22ConditionalConformanceSiRszlE3baryyF

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

// S<Int> will also have synthesized conformances to Copyable and Escapable, so match multiple lines
// at once to make sure we only find the conformance we want to see
// CONFORMS: "kind": "conformsTo"{{.*[[:space:]].*}} "source": "s:22ConditionalConformance1SV"{{.*[[:space:]].*}} "target": "s:22ConditionalConformance1PP"{{.*[[:space:]].*}} "swiftConstraints"
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

// CONFORMSEXT: "kind": "conformsTo"{{.*[[:space:]].*}} "source": "[[EXTID]]"{{.*[[:space:]].*}} "target": "s:22ConditionalConformance1PP"{{.*[[:space:]].*}} "swiftConstraints"
// CONFORMSEXT: "kind": "sameType"
// CONFORMSEXT-NEXT: "lhs": "Element"
// CONFORMSEXT-NEXT: "rhs": "Int"

extension Array: P where Element == Int {
  // SYNTHEXT: "source": "s:22ConditionalConformance1PPAAE3fooyyF::SYNTHESIZED::s:Sa"
  // SYNTHEXT-NEXT: "target": "[[EXTID]]"

  // MEMBEREXT: "source": "s:Sa22ConditionalConformanceSiRszlE3baryyF"
  // MEMBEREXT-NEXT: "target": "[[EXTID]]"
  public func bar() {}
}
