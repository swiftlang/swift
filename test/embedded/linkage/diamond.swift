// This test makes sure that we can build, link, and execute an Embedded Swift
// application that's composed of multiple libraries. It tests various
// combinations of "leaf" and "non-leaf" modules along the way to ensure that
// every combination produces a working binary that is free of either missing or
// (incorrectly) duplicated symbols. All of the modules implicitly depend on the
// Swift standard library.
//
// The overall shape of the module dependencies looks like this:
//
//
//                           Swift
//                             |
//                             |
//                           Root
//                          /    \
//                         /      \
//                     ClientA  ClientB
//                        \        /
//                         \      /
//                        Application
//

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Test #1: Defaults for everything
// RUN: %target-swift-frontend -c -emit-module -o %t/Root.o %t/Root.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientA.o %t/ClientA.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientB.o %t/ClientB.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-clang %target-clang-resource-dir-opt %t/Root.o %t/ClientA.o %t/ClientB.o %t/Application.o -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

// Test #2: Root is an "intermediate" library for everything
// RUN: %target-swift-frontend -c -emit-module -o %t/Root.o %t/Root.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature IntermediateLibrary -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientA.o %t/ClientA.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientB.o %t/ClientB.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-clang %target-clang-resource-dir-opt %t/Root.o %t/ClientA.o %t/ClientB.o %t/Application.o -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

// Test #3: ClientA as an "intermediate" library
// RUN: %target-swift-frontend -c -emit-module -o %t/Root.o %t/Root.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientA.o %t/ClientA.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature IntermediateLibrary -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientB.o %t/ClientB.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-clang %target-clang-resource-dir-opt %t/Root.o %t/ClientA.o %t/ClientB.o %t/Application.o -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

// Test #4: Root and ClientA as "intermediate" libraries
// RUN: %target-swift-frontend -c -emit-module -o %t/Root.o %t/Root.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature IntermediateLibrary -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientA.o %t/ClientA.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature IntermediateLibrary -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientB.o %t/ClientB.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -parse-as-library
// RUN: %target-clang %target-clang-resource-dir-opt %t/Root.o %t/ClientA.o %t/ClientB.o %t/Application.o -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

// Test #%: All "intermediate", all the time. Main drives code generation
// RUN: %target-swift-frontend -c -emit-module -o %t/Root.o %t/Root.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature IntermediateLibrary -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientA.o %t/ClientA.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature IntermediateLibrary -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientB.o %t/ClientB.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature IntermediateLibrary -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedLinkageModel -enable-experimental-feature IntermediateLibrary -parse-as-library
// RUN: %target-clang %target-clang-resource-dir-opt %t/Root.o %t/ClientA.o %t/ClientB.o %t/Application.o -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s


// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_IntermediateLibrary
// REQUIRES: swift_feature_EmbeddedLinkageModel

//--- Root.swift
struct Point {
  var x, y: Int
}

public func enumerateByteOffsets<T>(_: T.Type) -> [Int] {
  var array: [Int] = []
  for i in 0..<MemoryLayout<T>.size {
    array.append(i)
  }
  return array
}

public func getPointOffsets() -> [Int] {
  enumerateByteOffsets(Point.self)
}

//--- ClientA.swift
import Root

public struct Color {
  var red: UInt8
  var green: UInt8
  var blue: UInt8
}

public func getPointAndColorOffsets() -> [Int] {
  getPointOffsets() + enumerateByteOffsets(Color.self)
}

//--- ClientB.swift
import Root

struct Point3D {
  var x, y, z: Double
}

public func getExtraPoint3DOffsets() -> [Int] {
  let pointOffsets = getPointOffsets()
  let point3DOffsets = enumerateByteOffsets(Point3D.self)
  return Array(point3DOffsets[pointOffsets.count...])
}

//--- Application.swift
import ClientA
import ClientB

@main
struct Main {
  static func main() {
    let pointAndColorOffsets = getPointAndColorOffsets()
    let extraColor3DOffsets = getExtraPoint3DOffsets()
    print(pointAndColorOffsets.count)
    print(extraColor3DOffsets.count)

    // CHECK: DONE
    print("DONE")
  }
}
