// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-build-swift %t/src/Library.swift -swift-version 5 -emit-module -emit-library \
// RUN:    -enable-library-evolution \
// RUN:    -module-name Library \
// RUN:    -o %t/%target-library-name(Library) \
// RUN:    -emit-module-interface-path %t/Library.swiftinterface

// RUN: %target-codesign %t/%target-library-name(Library)

// RUN: %target-build-swift -I %t -L %t -l Library %t/src/main.swift %target-rpath(%t) -o %t/main.out
// RUN: %target-codesign %t/main.out
// RUN: %target-run %t/main.out %t/%target-library-name(Library) 2>&1 | %FileCheck %t/src/main.swift

// RUN: rm %t/Library.swiftmodule

// RUN: %target-build-swift -I %t -L %t -l Library %t/src/main.swift %target-rpath(%t) -o %t/main.out
// RUN: %target-codesign %t/main.out
// RUN: %target-run %t/main.out %t/%target-library-name(Library) 2>&1 | %FileCheck %t/src/main.swift

// REQUIRES: executable_test

// UNSUPPORTED: remote_run || device_run

//--- Library.swift
@frozen
public struct Inlinable {
  var _x: Int

  public var x: Int {
    @usableFromInline
    @storageRestrictions(initializes: _x)
    init {
      self._x = newValue
    }

    get {
      _x
    }
  }

  @inlinable
  public init(x: Int) {
    self.x = x
  }
}

@frozen
public struct Transparent {
  @usableFromInline
  var _x: Int

  public var x: Int {
    @_alwaysEmitIntoClient
    @storageRestrictions(initializes: _x)
    init {
      self._x = newValue
    }

    get {
      _x
    }
  }

  @_alwaysEmitIntoClient
  public init(x: Int) {
    self.x = x
  }
}

//--- main.swift
import Library

let inlinable = Inlinable(x: 42)
print("Inlinable.x = \(inlinable.x)")
// CHECK: Inlinable.x = 42

let transparent = Transparent(x: -1)
print("Transparent.x = \(transparent.x)")
// CHECK: Transparent.x = -1
