// swift-tools-version:4.2

import PackageDescription
import Foundation

// This is a stop gap hack so we can edit benchmarks in Xcode.
let singleSourceLibraries: [String] = {
  let f = FileManager.`default`
  let dirURL = URL(fileURLWithPath: "single-source").absoluteURL
  let fileURLs = try! f.contentsOfDirectory(at: dirURL,
                                            includingPropertiesForKeys: nil)
  return fileURLs.flatMap { (path: URL) -> String? in
    let c = path.lastPathComponent.split(separator: ".")
    // Too many components. Must be a gyb file.
    if c.count > 2 {
      return nil
    }
    if c[1] != "swift" {
      return nil
    }

    // We do not support this test.
    if c[0] == "ObjectiveCNoBridgingStubs" {
      return nil
    }

    assert(c[0] != "PrimsSplit")
    return String(c[0])
  }
}()

let multiSourceLibraries: [String] = {
  let f = FileManager.`default`
  let dirURL = URL(fileURLWithPath: "multi-source").absoluteURL
  let fileURLs = try! f.contentsOfDirectory(at: dirURL,
                                            includingPropertiesForKeys: nil)
  return fileURLs.map { (path: URL) -> String in
    return path.lastPathComponent
  }
}()

var products: [Product] = []
products.append(.library(name: "TestsUtils", type: .static, targets: ["TestsUtils"]))
products.append(.library(name: "DriverUtils", type: .static, targets: ["DriverUtils"]))
products.append(.library(name: "ObjectiveCTests", type: .static, targets: ["ObjectiveCTests"]))
products.append(.executable(name: "SwiftBench", targets: ["SwiftBench"]))
products.append(.library(name: "PrimsSplit", type: .static, targets: ["PrimsSplit"]))
products += singleSourceLibraries.map { .library(name: $0, type: .static, targets: [$0]) }
products += multiSourceLibraries.map { .library(name: $0, type: .static, targets: [$0]) }

var targets: [Target] = []
targets.append(.target(name: "TestsUtils", path: "utils", sources: ["TestsUtils.swift"]))
targets.append(.systemLibrary(name: "LibProc", path: "utils/LibProc"))
targets.append(
  .target(name: "DriverUtils",
    dependencies: [.target(name: "TestsUtils"), "LibProc"],
    path: "utils",
    sources: ["DriverUtils.swift", "ArgParse.swift"]))
targets.append(
    .target(name: "SwiftBench",
    dependencies: [
      .target(name: "TestsUtils"),
      .target(name: "ObjectiveCTests"),
      .target(name: "DriverUtils"),
    ] + singleSourceLibraries.map { .target(name: $0) }
    + multiSourceLibraries.map { .target(name: $0) },
    path: "utils",
    sources: ["main.swift"]))
targets.append(
  .target(name: "ObjectiveCTests",
    path: "utils/ObjectiveCTests",
    publicHeadersPath: "."))
targets += singleSourceLibraries.map { x in
    return .target(name: x,
      dependencies: [
        .target(name: "TestsUtils"),
        .target(name: "ObjectiveCTests"),
      ],
      path: "single-source",
      sources: ["\(x).swift"])
}
targets += multiSourceLibraries.map { x in
  return .target(name: x,
    dependencies: [
      .target(name: "TestsUtils")
    ],
    path: "multi-source/\(x)")
}

let p = Package(
  name: "swiftbench",
  products: products,
  targets: targets,
  swiftLanguageVersions: [.v4]
)
