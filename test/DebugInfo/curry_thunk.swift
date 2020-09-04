// RUN: %target-swift-frontend -primary-file %s %S/Inputs/curry_thunk_other.swift -emit-ir -g -o - | %FileCheck %s

public func testCurryThunk() -> [HTTPMethod] {
  return ["asdf"].map(HTTPMethod.init)
}

// CHECK: [[FILE:![0-9]+]] = !DIFile(filename: "{{.*[/\\]}}curry_thunk.swift", directory: "{{.*}}")
// CHECK: {{![0-9]+}} = !DILocalVariable(name: "rawValue", arg: 1, scope: {{![0-9]+}}, file: {{![0-9]+}}, type: {{![0-9]+}}, flags: DIFlagArtificial)
