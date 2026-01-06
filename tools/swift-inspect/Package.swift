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
                .target(name: "SwiftInspectLinux", condition: .when(platforms: [.linux, .android])),
                .target(name: "AndroidCLib", condition: .when(platforms: [.android])),
            ],
            swiftSettings: [.unsafeFlags(["-parse-as-library"])]),
        .target(name: "SwiftInspectClient"),
        .target(
            name: "SwiftInspectLinux",
            dependencies: ["LinuxSystemHeaders"],
            path: "Sources/SwiftInspectLinux",
            exclude: ["SystemHeaders"],
            cSettings: [.define("_GNU_SOURCE", to: "1")]),
        .systemLibrary(
            name: "LinuxSystemHeaders",
            path: "Sources/SwiftInspectLinux/SystemHeaders"),
        .target(
            name: "AndroidCLib",
            path: "Sources/AndroidCLib",
            publicHeadersPath: "include",
            cSettings: [.unsafeFlags(["-fPIC"])]),
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
        .package(url: "https://github.com/apple/swift-argument-parser.git", from: "1.5.0"),
    ]
} else {
    package.dependencies += [.package(path: "../../../swift-argument-parser")]
}
