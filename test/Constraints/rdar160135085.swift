// RUN: %empty-directory(%t/modules)
// RUN: split-file %s %t

// RUN: %target-swift-frontend-emit-module -emit-module-path %t/modules/A.swiftmodule -module-name A %t/a.swift
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/modules/B.swiftmodule -module-name B -I %t/modules %t/b.swift
// RUN: %target-swift-frontend -typecheck -I %t/modules %t/c.swift

//--- a.swift

public struct S<T> {
  public init(_ x: T) {}
}

//--- b.swift

import A
public typealias S = A.S

//--- c.swift

import A
import B

_ = S(0)
