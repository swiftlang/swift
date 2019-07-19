// swift-tools-version:5.0

import PackageDescription
import Foundation

var unsupportedTests: Set<String> = ["ObjectiveCNoBridgingStubs"]
#if !os(macOS) && !os(iOS) && !os(watchOS) && !os(tvOS)
unsupportedTests.insert("ObjectiveCBridging")
unsupportedTests.insert("ObjectiveCBridgingStubs")
#endif

//===---
// Single Source Libraries
//

/// Return the source files in subDirectory that we will translate into
/// libraries. Each source library will be compiled as its own module.
func getSingleSourceLibraries(subDirectory: String) -> [String] {
  let f = FileManager.`default`
  let dirURL = URL(fileURLWithPath: subDirectory)
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

    let name = String(c[0])

    // We do not support this test.
    if unsupportedTests.contains(name) {
      return nil
    }

    return name
  }
}

var singleSourceLibraryDirs: [String] = []
singleSourceLibraryDirs.append("single-source")

var singleSourceLibraries: [String] = singleSourceLibraryDirs.flatMap {
  getSingleSourceLibraries(subDirectory: $0)
}

//===---
// Multi Source Libraries
//

func getMultiSourceLibraries(subDirectory: String) -> [(String, String)] {
  let f = FileManager.`default`
  let dirURL = URL(string: subDirectory)!
  let subDirs = try! f.contentsOfDirectory(at: dirURL, includingPropertiesForKeys: nil)
  return subDirs.map { (subDirectory, $0.lastPathComponent) }
}

var multiSourceLibraryDirs: [String] = []
multiSourceLibraryDirs.append("multi-source")

var multiSourceLibraries: [(parentSubDir: String, name: String)] = multiSourceLibraryDirs.flatMap {
  getMultiSourceLibraries(subDirectory: $0)
}

//===---
// Products
//

var products: [Product] = []
products.append(.library(name: "TestsUtils", type: .static, targets: ["TestsUtils"]))
products.append(.library(name: "DriverUtils", type: .static, targets: ["DriverUtils"]))
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
products.append(.library(name: "ObjectiveCTests", type: .static, targets: ["ObjectiveCTests"]))
#endif
products.append(.executable(name: "SwiftBench", targets: ["SwiftBench"]))

products += singleSourceLibraries.map { .library(name: $0, type: .static, targets: [$0]) }
products += multiSourceLibraries.map {
  return .library(name: $0.name, type: .static, targets: [$0.name])
}

//===---
// Targets
//

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
swiftBenchDeps += multiSourceLibraries.map { .target(name: $0.name) }

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

targets += singleSourceLibraries.map { name in
  return .target(name: name,
      dependencies: singleSourceDeps,
      path: "single-source",
      sources: ["\(name).swift"])
}

targets += multiSourceLibraries.map { lib in
  return .target(
    name: lib.name,
    dependencies: [
      .target(name: "TestsUtils")
    ],
    path: lib.parentSubDir)
}

//===---
// Top Level Definition
//

let p = Package(
  name: "swiftbench",
  products: products,
  targets: targets,
  swiftLanguageVersions: [.v4]
)
