// swift-tools-version: 6.4

internal import PackageDescription

let package =
    Package(name: "sxs-audit",
            products: [
              .executable(name: "sxs-audit",
                          targets: ["SxSAudit"]),
            ],
            dependencies: [
              .package(url: "https://github.com/apple/swift-argument-parser",
                       from: "1.8.2"),
            ],
            targets: [
              .executableTarget(name: "SxSAudit",
                                dependencies: [
                                  .product(name: "ArgumentParser",
                                           package: "swift-argument-parser"),
                                ],
                                linkerSettings: [
                                  .linkedLibrary("Dbghelp",
                                                 .when(platforms: [.windows])),
                                ]),
              .testTarget(name: "SxSAuditTests",
                          dependencies: ["SxSAudit"]),
            ])
