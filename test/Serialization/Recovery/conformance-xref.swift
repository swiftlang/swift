// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Reference build: Build libs and client for a normal build.
// RUN: %target-swift-frontend -emit-module %t/ChangingLib.swift -I %t \
// RUN:   -emit-module-path %t/ChangingLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/MiddleLib.swift -I %t \
// RUN:   -emit-module-path %t/MiddleLib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t

/// Case 1: Remove a conformance from one lib, leave the other as stale and
/// rebuild client. Error on the missing conformance.
// RUN: %target-swift-frontend -emit-module %t/ChangingLib.swift -I %t \
// RUN:   -emit-module-path %t/ChangingLib.swiftmodule \
// RUN:   -D DROP_CONFORMANCE
// RUN: not %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   2> %t/errors.log
// RUN: %FileCheck %s --input-file=%t/errors.log \
// RUN:    --check-prefix CHECK-REMARK-CONFORMANCE

/// No errors when extended recovery is enabled.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -experimental-allow-module-with-compiler-errors -verify

/// Case 2: Remove a requirement from one lib, leave the other as stale and
/// rebuild client. Error on the mismatch.
// RUN: %target-swift-frontend -emit-module %t/ChangingLib.swift -I %t \
// RUN:   -emit-module-path %t/ChangingLib.swiftmodule \
// RUN:   -D DROP_REQUIREMENT
// RUN: not %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   2> %t/errors.log
// RUN: %FileCheck %s --input-file=%t/errors.log \
// RUN:   --check-prefix CHECK-REMARK-REQUIREMENT

/// No errors when extended recovery is enabled.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -experimental-allow-module-with-compiler-errors -verify

/// Combined case: Remove both the conformance and the requirement.
/// Fail on the removed conformance only as it was first.
// RUN: %target-swift-frontend -emit-module %t/ChangingLib.swift -I %t \
// RUN:   -emit-module-path %t/ChangingLib.swiftmodule \
// RUN:   -D DROP_CONFORMANCE -D DROP_REQUIREMENT
// RUN: not %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   2> %t/errors.log
// RUN: %FileCheck %s --input-file=%t/errors.log \
// RUN:   --check-prefixes CHECK-REMARK-CONFORMANCE

//--- ChangingLib.swift

public protocol SimpleProto {
  func successor() -> Self
}

#if DROP_CONFORMANCE
// CHECK-REMARK-CONFORMANCE: MiddleLib.swiftmodule:1:1: error: Conformance of 'Counter' to 'SimpleProto' not found in referenced module 'ChangingLib'
// CHECK-REMARK-CONFORMANCE: note: Breaks conformances of 'OneToAThousand' to 'ProtoUser'
#else
extension Counter: SimpleProto {}
#endif

public protocol ProtoUser {
  associatedtype Element
#if DROP_REQUIREMENT
// CHECK-REMARK-REQUIREMENT: MiddleLib.swiftmodule:1:1: error: Conformances of 'OneToAThousand' do not match requirement signature of 'ProtoUser'; 5 conformances for 6 requirements
// CHECK-REMARK-REQUIREMENT: Requirements:
// Skipping implicits.
// CHECK-REMARK-REQUIREMENT: Conformances:
// Skipping implicits.
// CHECK-REMARK-REQUIREMENT: (specialized_conformance type="OneToAThousand.Impl" protocol="SimpleProto"
// CHECK-REMARK-REQUIREMENT:   (normal_conformance type="Counter<T>" protocol="SimpleProto"{{.*}} lazy))
  associatedtype Impl
#else
  associatedtype Impl: SimpleProto
#endif

  var start: Impl { get }

  subscript(_: Impl) -> Element { get }
}

public struct Counter<T> {
  public var value = 0

  public init(value: Int) { self.value = value }

  public func successor() -> Counter {
    return Counter(value: value + 1)
  }
}

//--- MiddleLib.swift

import ChangingLib

// Instantiate Counter<Int>, relying on Counter's adoption of SimpleProto.
public struct OneToAThousand : ProtoUser {
  public typealias Impl = Counter<Int>

  public var start: Impl {
    return Impl(value: 1)
  }

  public subscript(i: Impl) -> Int {
    return i.value
  }

  public init() {}
}

//--- Client.swift

import MiddleLib

var test = OneToAThousand()
var s = test.start
print(test[s])
