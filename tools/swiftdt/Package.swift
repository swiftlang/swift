// swift-tools-version:5.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "swiftdt",
    dependencies: [
        // Dependencies declare other packages that this package depends on.
//        .package(path: "../SymbolicationShims"),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages which this package depends on.
        .target(
            name: "swiftdt",
            dependencies: ["SymbolicationShims"]),
        .testTarget(
            name: "swiftdtTests",
            dependencies: ["swiftdt"]),
        .systemLibrary(
            name: "SymbolicationShims")
    ]
)
