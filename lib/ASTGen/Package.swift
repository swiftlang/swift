// swift-tools-version: 5.6

import PackageDescription

let package = Package(
    name: "ASTGen",
    products: [
        .library(name: "ASTGen", targets: ["ASTGen"]),
    ],
    dependencies: [
         .package(path: "../../../swift-syntax"),
    ],
    targets: [
        .target(
            name: "ASTGen",
            dependencies: ["SwiftSyntax"]),
    ]
)
