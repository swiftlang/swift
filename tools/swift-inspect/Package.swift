// swift-tools-version:5.3
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription
import class Foundation.ProcessInfo

let package = Package(
    name: "swift-inspect",
    products: [
        .library(name: "SwiftInspectClient", type: .dynamic, targets: ["SwiftInspectClient"]),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages which this package depends on.
        .target(
            name: "swift-inspect",
            dependencies: [
                "SymbolicationShims",
                .product(name: "ArgumentParser", package: "swift-argument-parser"),
                .target(name: "SwiftInspectClient", condition: .when(platforms: [.windows])),
                .target(name: "SwiftInspectClientInterface", condition: .when(platforms: [.windows])),
            ],
            swiftSettings: [.unsafeFlags(["-parse-as-library"])]),
        .target(
            name: "SwiftInspectClient",
            // Workaround https://github.com/llvm/llvm-project/issues/40056
            cxxSettings: [.unsafeFlags(["-Xclang", "-fno-split-cold-code"])]),
        .systemLibrary(
            name: "SwiftInspectClientInterface"),
        .testTarget(
            name: "swiftInspectTests",
            dependencies: ["swift-inspect"]),
        .systemLibrary(
            name: "SymbolicationShims")
    ]
)

if ProcessInfo.processInfo.environment["SWIFTCI_USE_LOCAL_DEPS"] == nil {
    package.dependencies += [
        .package(url: "https://github.com/apple/swift-argument-parser.git", from: "1.2.2"),
    ]
} else {
    package.dependencies += [.package(path: "../../../swift-argument-parser")]
}
