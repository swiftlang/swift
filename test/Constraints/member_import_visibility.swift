// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A \
// RUN:   -emit-module-path %t/A.swiftmodule

/// Build the library B
// RUN: %target-swift-frontend -I %t -emit-module %t/src/B.swift \
// RUN:   -module-name B \
// RUN:   -emit-module-path %t/B.swiftmodule

// RUN: %target-swift-frontend -typecheck -I %t %t/src/Main.swift %t/src/Other.swift -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- A.swift
public struct Test {
  public init(a: Double) { }
}

//--- B.swift
import A

extension Test {
  public init(a: Int) { fatalError() }
}

//--- Main.swift
import A

func test() {
  _ = Test(a: 0) // Ok, selects the overload that takes Double.
}

//--- Other.swift
import B
