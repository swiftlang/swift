// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift-dylib(%t/%target-library-name(a)) -O -target %target-future-triple -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-layout-string-value-witnesses-instantiation -enable-library-evolution -wmo -emit-library -emit-module -emit-module-path=%t/a.swiftmodule %t/a.swift
// RUN: %target-codesign %t/%target-library-name(a)
// RUN: %target-build-swift -O -target %target-future-triple -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation -Xfrontend -enable-layout-string-value-witnesses -Xfrontend -enable-layout-string-value-witnesses-instantiation -enable-library-evolution -wmo -I%t -L%t -l a %t/b.swift -o %t/b %target-rpath(%t)
// RUN: %target-codesign %t/b
// RUN: %target-run %t/b %t/%target-library-name(a) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_LayoutStringValueWitnesses
// REQUIRES: swift_feature_LayoutStringValueWitnessesInstantiation

// Requires runtime functions added in Swift 5.9.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

//--- a.swift

@frozen
public struct DummyArray<Element: ~Copyable>: ~Copyable {
  @usableFromInline
  internal var _storage: UnsafeMutableBufferPointer<Element>

  @inlinable
  public init(first: consuming Element, second: consuming Element) {
    _storage = .allocate(capacity: 2)
    _storage.initializeElement(at: 0, to: first)
    _storage.initializeElement(at: 1, to: second)
  }

  @inlinable
  public var capacity: Int { _storage.count }

  deinit {
    _storage.deinitialize()
  }
}

public struct ResilientType {
  public let x: Int = 0
  public let y: AnyObject

  public init(y: AnyObject) {
    self.y = y
  }
}

//--- b.swift

import a

// Non-copyable type containing a field of resilient type
private struct NonCopyableWrapper: ~Copyable {
    let x: Int?
    let y: ResilientType
}

class TestClass {
    deinit {
        print("TestClass deinitialized")
    }
}

func test() {
    let wrapper1 = NonCopyableWrapper(
        x: nil,
        y: ResilientType(y: TestClass())
    )
    let wrapper2 = NonCopyableWrapper(
        x: nil,
        y: ResilientType(y: TestClass())
    )

    // CHECK: Creating DummyArray
    print("Creating DummyArray")
    let items = DummyArray<NonCopyableWrapper>(first: wrapper1, second: wrapper2)
    // CHECK: Created DummyArray with capacity = 2
    print("Created DummyArray with capacity = \(items.capacity)")
    // CHECK: About to deinitialize DummyArray
    print("About to deinitialize DummyArray")
}

test()

// CHECK: TestClass deinitialized
// CHECK: TestClass deinitialized
