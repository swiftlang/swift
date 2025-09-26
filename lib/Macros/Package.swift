// swift-tools-version: 6.2

// Although this could in theory be used to build macros, it's not actually wired up to CMake.
// This is here purely to provide LSP IDE features when working on Swift macros.

import PackageDescription
import CompilerPluginSupport

let package = Package(
	name: "SwiftifyImport",
	platforms: [.macOS(.v11), .iOS(.v15), .tvOS(.v15), .watchOS(.v8)],
	products: [
		.library(
			name: "SwiftifyImport",
			targets: [
				"SwiftifyImport"
			]
		),
		.library(
			name: "SwiftifyImportMacro",
			targets: [
				"SwiftifyImportMacro"
			]
		)
	],
	dependencies: [
		.package(path: "../../../swift-syntax")
	],
	targets: [
		.macro(
			name: "SwiftifyImportMacro",
			dependencies: [
				.product(name: "SwiftCompilerPlugin", package: "swift-syntax"),
                .product(name: "SwiftDiagnostics", package: "swift-syntax"),
                .product(name: "SwiftParser", package: "swift-syntax"),
                .product(name: "SwiftSyntax", package: "swift-syntax"),
                .product(name: "SwiftSyntaxBuilder", package: "swift-syntax"),
                .product(name: "SwiftSyntaxMacro", package: "swift-syntax")
			]
		),
		.target(
			name: "SwiftifyImport",
			dependencies: ["SwiftifyImportMacro"]
		)
	]
)
