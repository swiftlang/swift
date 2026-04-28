// swift-tools-version:5.3

import PackageDescription

let SwiftCOM = Package(
  name: "SwiftCOM",
  products: [
    .library(name: "SwiftCOM", type: .dynamic, targets: ["SwiftCOM"]),
  ],
  targets: [
    .target(
      name: "SwiftCOM",
      linkerSettings: [
        .linkedLibrary("Ole32"),
        .linkedLibrary("PortableDeviceGuids"),
      ]
    )
  ]
)
