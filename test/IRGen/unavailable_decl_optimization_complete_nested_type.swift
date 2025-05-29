// RUN: %target-swift-frontend -parse-as-library -module-name Test -validate-tbd-against-ir=missing -unavailable-decl-optimization=none %s -emit-ir | %FileCheck %s --check-prefixes=CHECK-NO-STRIP

// RUN: %target-swift-frontend -parse-as-library -module-name Test -validate-tbd-against-ir=missing -unavailable-decl-optimization=complete %s -emit-ir | %FileCheck %s --implicit-check-not=unavailableFuncWithNestedObsoleteType --implicit-check-not=unavailableFuncWithNestedType --implicit-check-not=NestedInExtension

// CHECK-NO-STRIP-DAG: s4Test29unavailableFuncWithNestedType
// CHECK-NO-STRIP-DAG: s4Test29unavailableFuncWithNestedTypeyyF0E10InFunction

@available(*, unavailable)
public func unavailableFuncWithNestedType() {
  struct NestedInFunction {
    init() {}
  }

  _ = NestedInFunction()
}

// CHECK-NO-STRIP-DAG: s4Test37unavailableFuncWithNestedObsoleteType
// CHECK-NO-STRIP-DAG: s4Test37unavailableFuncWithNestedObsoleteTypeyyF0E10InFunction

@available(*, unavailable)
public func unavailableFuncWithNestedObsoleteType() {
  @available(swift, obsoleted: 4)
  struct NestedInFunction {
    init() {}
  }
}

public struct S {}

// CHECK-NO-STRIP-DAG: s4Test1SV17NestedInExtension

extension S {
  @available(*, unavailable)
  public struct NestedInExtension {
    public func method() {}
  }
}

