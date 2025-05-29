// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build LibA
// RUN: %host-build-swift %t/src/LibA.swift -swift-version 5 -enable-experimental-feature KeyPathWithMethodMembers -emit-module -emit-library -enable-library-evolution -module-name LibA -o %t/%target-library-name(LibA) -emit-module-interface-path %t/LibA.swiftinterface

// Build LibB
// RUN: %target-build-swift %t/src/LibB.swift -I %t -L %t -l LibA -swift-version 5 -enable-experimental-feature KeyPathWithMethodMembers -emit-module -emit-library -module-name LibB -o %t/%target-library-name(LibB)

// Build LibC
// RUN: %target-build-swift %t/src/LibC.swift -I %t -L %t -l LibA -swift-version 5 -enable-experimental-feature KeyPathWithMethodMembers -emit-module -emit-library -module-name LibC -o %t/%target-library-name(LibC)

// Build & run main.swift
// RUN: %target-build-swift -I %t -L %t -l LibA -l LibB -l LibC %t/src/main.swift -o %t/a.out
// RUN: %target-codesign %t/%target-library-name(LibA)
// RUN: %target-codesign %t/%target-library-name(LibB)
// RUN: %target-codesign %t/%target-library-name(LibC)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_KeyPathWithMethodMembers

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

//--- LibA.swift
public struct AStruct {
  public static var property1: Int = 1
  public static var property2: Int = 2
  private(set) public static var property3: Int = 1
  private(set) public static var property4: Int = 4
  public static func x(val value: Int) -> Int { return value }
  public static func y(val value: Int) -> Int { return value }
  
  public init(val value: Int = 2024) {
    year = value
  }
  public var year: Int
}

//--- LibB.swift
import LibA

public let keyPath1FromLibB = \AStruct.Type.property1
public let keyPath2FromLibB = \AStruct.Type.property2
public let keyPath3FromLibB = \AStruct.Type.property3
public let keyPath4FromLibB = \AStruct.Type.property4
public var keyPath5FromLibB = \AStruct.Type.property1 // WritableKeyPath with public setter
public var keyPath6FromLibB = \Int.Type.zero
public let keyPath7FromLibB = \AStruct.Type.x(val: 10)
public let keyPath8FromLibB = \AStruct.Type.y(val: 10)
public let keyPath9FromLibB = \AStruct.Type.init
public let keyPath10FromLibB = \AStruct.Type.init(val: 2025)

//--- LibC.swift
import LibA

public let keyPath1FromLibC = \AStruct.Type.property1
public let keyPath2FromLibC = \AStruct.Type.property2
public let keyPath3FromLibC = \AStruct.Type.property3 // Read-only with private setter
public let keyPath4FromLibC = \AStruct.Type.property4
public var keyPath5FromLibC = \Int.Type.zero
public var keyPath6FromLibC = \Int.Type.max
public let keyPath7FromLibC = \AStruct.Type.x(val: 10)
public let keyPath8FromLibC = \AStruct.Type.init
public let keyPath9FromLibC = \AStruct.Type.init(val: 2026)

//--- main.swift
import LibB
import LibC

// CHECK: true
print(keyPath1FromLibB == keyPath1FromLibC)
// CHECK: true
print(keyPath1FromLibB != keyPath2FromLibC)

// CHECK: true
print(keyPath3FromLibB == keyPath3FromLibC)
// CHECK: true
print(keyPath3FromLibB != keyPath4FromLibC)

// CHECK: false
print(keyPath5FromLibB == keyPath3FromLibC)

// Check: true
print(keyPath6FromLibB == keyPath5FromLibC)
// Check: false
print(keyPath6FromLibB == keyPath6FromLibC)

// CHECK: true
print(keyPath7FromLibB == keyPath7FromLibC)
// CHECK: false
print(keyPath8FromLibB == keyPath7FromLibC)
// CHECK: true
print(keyPath9FromLibB == keyPath8FromLibC)
// CHECK: true
print(keyPath10FromLibB != keyPath9FromLibC)
