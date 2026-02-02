// RUN: %empty-directory(%t)
// RUN: split-file %S/diamond.swift %t

// Test:  Main drives code generation
// RUN: %target-swift-frontend -c -emit-module -o %t/Root.o %t/Root.swift -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library -enable-experimental-feature EmbeddedExistentials
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientA.o %t/ClientA.swift -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library -enable-experimental-feature EmbeddedExistentials
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientB.o %t/ClientB.swift -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library -enable-experimental-feature EmbeddedExistentials
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library -enable-experimental-feature EmbeddedExistentials
// RUN: %target-clang %target-clang-resource-dir-opt %t/Root.o %t/ClientA.o %t/ClientB.o %t/Application.o -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

// But use this file's Root.swift
// RUN: %empty-directory(%t)
// RUN: split-file %S/diamond.swift %t
// RUN: split-file %s %t

// Test:  Main drives most code generation but Root.swift references PointClass.
// RUN: %target-swift-frontend -c -emit-module -o %t/Root.o %t/Root.swift -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library -enable-experimental-feature EmbeddedExistentials
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientA.o %t/ClientA.swift -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library -enable-experimental-feature EmbeddedExistentials
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/ClientB.o %t/ClientB.swift -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library -enable-experimental-feature EmbeddedExistentials
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -parse-as-library -enable-experimental-feature EmbeddedExistentials
// RUN: %target-clang %target-clang-resource-dir-opt %t/Root.o %t/ClientA.o %t/ClientB.o %t/Application.o -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials
// REQUIRES: swift_feature_DeferredCodeGen

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

public class PointClass {
  public var x, y: Int

  public init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }
}

public protocol Reflectable: AnyObject {
  func reflect()
}

extension PointClass: Reflectable {
  public func reflect() {
    swap(&x, &y)
  }
}

@used
public func getPointClass() -> PointClass {
    return PointClass(x: 1, y: 2)
}

// CHECK: DONE
