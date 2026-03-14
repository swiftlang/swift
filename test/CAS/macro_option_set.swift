// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %s -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -plugin-path %swift-plugin-dir

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json MyApp casFSRootID > %t/fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/fs.casid | %FileCheck %s --check-prefix=FS

// FS: SwiftMacros

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %target-swift-frontend-plain \
// RUN:   -typecheck -verify -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name MyApp -O \
// RUN:   -plugin-path %swift-plugin-dir \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %s @%t/MyApp.cmd

import Swift

@attached(member, names: named(RawValue), named(rawValue), named(`init`), arbitrary)
@attached(extension, conformances: OptionSet)
public macro OptionSet<RawType>() =
  #externalMacro(module: "SwiftMacros", type: "OptionSetMacro")

@OptionSet<UInt8>
struct ShippingOptions {
  private enum Options: Int {
    case nextDay
    case secondDay
    case priority
    case standard
  }

  static let express: ShippingOptions = [.nextDay, .secondDay]
  static let all: ShippingOptions = [.express, .priority, .standard]
}

let options = ShippingOptions.express
assert(options.contains(.nextDay))
assert(options.contains(.secondDay))
assert(!options.contains(.standard))

