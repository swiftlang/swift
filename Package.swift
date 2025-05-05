// swift-tools-version:5.5
import PackageDescription

let package = Package(
    name: "PropertyWrapperTests",
    products: [
        .library(
            name: "PropertyWrappers",
            targets: ["PropertyWrappers"]),
    ],
    dependencies: [],
    targets: [
        .target(
            name: "PropertyWrappers",
            dependencies: []),
        .testTarget(
            name: "PropertyWrapperTests",
            dependencies: ["PropertyWrappers"]),
    ]
)
