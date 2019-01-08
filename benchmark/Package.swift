// swift-tools-version:4.2

import PackageDescription
import Foundation

var unsupportedTests: Set<String> = ["ObjectiveCNoBridgingStubs"]
#if !os(macOS) && !os(iOS) && !os(watchOS) && !os(tvOS)
unsupportedTests.insert("ObjectiveCBridging")
unsupportedTests.insert("ObjectiveCBridgingStubs")
#endif

// This is a stop gap hack so we can edit benchmarks in Xcode.
let singleSourceLibraries: [String] = {
  let f = FileManager.`default`
  let dirURL = URL(fileURLWithPath: "single-source").absoluteURL
  let fileURLs = try! f.contentsOfDirectory(at: dirURL,
                                            includingPropertiesForKeys: nil)
  return fileURLs.compactMap { (path: URL) -> String? in
    let c = path.lastPathComponent.split(separator: ".")
    // Too many components. Must be a gyb file.
    if c.count > 2 {
      return nil
    }
    if c[1] != "swift" {
      return nil
    }

    let s = String(c[0])

    // We do not support this test.
    if unsupportedTests.contains(s) {
      return nil
    }

    assert(s != "PrimsSplit")
    return s
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
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
products.append(.library(name: "ObjectiveCTests", type: .static, targets: ["ObjectiveCTests"]))
#endif
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

var swiftBenchDeps: [Target.Dependency] = [.target(name: "TestsUtils")]
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
swiftBenchDeps.append(.target(name: "ObjectiveCTests"))
#endif
swiftBenchDeps.append(.target(name: "DriverUtils"))
swiftBenchDeps += singleSourceLibraries.map { .target(name: $0) }
swiftBenchDeps += multiSourceLibraries.map { .target(name: $0) }

targets.append(
    .target(name: "SwiftBench",
    dependencies: swiftBenchDeps,
    path: "utils",
    sources: ["main.swift"]))

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
targets.append(
  .target(name: "ObjectiveCTests",
    path: "utils/ObjectiveCTests",
    publicHeadersPath: "."))
#endif

var singleSourceDeps: [Target.Dependency] = [.target(name: "TestsUtils")]
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
singleSourceDeps.append(.target(name: "ObjectiveCTests"))
#endif
targets += singleSourceLibraries.map { x in
    return .target(name: x,
      dependencies: singleSourceDeps,
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
