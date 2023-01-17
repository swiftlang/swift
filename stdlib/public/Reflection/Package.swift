// swift-tools-version: 5.7

import PackageDescription

let commonFlags = [
  "-I", "\(Context.packageDirectory)/Sources/_SwiftRuntimeShims",
  
  "-Xfrontend" ,"-define-availability",
  "-Xfrontend", "SwiftStdlib 9999:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999"
]

let libraryFlags = [
  "-parse-stdlib",
  "-enable-library-evolution",
  "-Xfrontend", "-disable-implicit-concurrency-module-import",
  "-Xfrontend", "-disable-implicit-string-processing-module-import",
  "-assert-config", "Release",
]

let testFlags = [
  "-Xfrontend", "-disable-availability-checking"
]

let package = Package(
  name: "Reflection",
  platforms: [
    .macOS(.v13)
  ],
  products: [
    .library(
      name: "_Runtime",
      type: .dynamic,
      targets: ["_Runtime"]
    ),
    .library(
      name: "Reflection",
      type: .dynamic,
      targets: ["Reflection"]
    ),
  ],
  targets: [
    .target(
      name: "_Runtime",
      exclude: [
        "CMakeLists.txt"
      ],
      swiftSettings: [
        .unsafeFlags(commonFlags + libraryFlags)
      ]
    ),
    .target(
      name: "Reflection",
      dependencies: ["_Runtime"],
      exclude: [
        "CMakeLists.txt"
      ],
      swiftSettings: [
        .unsafeFlags(commonFlags + libraryFlags)
      ]
    ),
    
    .testTarget(
      name: "RuntimeTests",
      dependencies: ["_Runtime"],
      swiftSettings: [
        .unsafeFlags(commonFlags + testFlags)
      ]
    ),
    .testTarget(
      name: "ReflectionTests",
      dependencies: ["_Runtime", "Reflection"],
      swiftSettings: [
        .unsafeFlags(commonFlags + testFlags)
      ]
    ),
  ]
)
