//===--- KnownCommand.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@preconcurrency import SwiftOptions

enum KnownCommand {
  case clang, swiftc, swiftFrontend
}

extension PathProtocol {
  var knownCommand: KnownCommand? {
    switch fileName {
    case "clang", "clang++", "c++", "cc", "clang-cache":
      .clang
    case "swiftc":
      .swiftc
    case "swift-frontend":
      .swiftFrontend
    default:
      nil
    }
  }
}

extension Command.Flag {
  /// If this spec is for a suboption, returns the command that it is for, or
  /// `nil` if it is not for a suboption
  var subOptionCommand: KnownCommand? {
    switch self {
    case .Xcc:
      .clang
    case .Xfrontend:
      .swiftFrontend
    default:
      nil
    }
  }
}

extension Command.Flag {
  struct Name: Hashable {
    let rawValue: String

    // Fileprivate because definitions should be added below.
    fileprivate init(_ rawValue: String) {
      self.rawValue = rawValue
    }
  }

  fileprivate static func dash(_ name: String) -> Self {
    dash(.init(name))
  }

  fileprivate static func doubleDash(_ name: String) -> Self {
    doubleDash(.init(name))
  }

  fileprivate static func swiftc(_ opt: SwiftOptions.Option) -> Self {
    let dashes = opt.spelling.prefix(while: { $0 == "-" }).count
    guard let dash = Command.Flag.Dash(numDashes: dashes) else {
      fatalError("Dash count not handled")
    }
    let name = String(opt.spelling.dropFirst(dashes))
    return .init(dash: dash, name: .init(name))
  }
}

extension SwiftOptions.Option {
  fileprivate static let optionsWithJoinedEquals: Set<String> = {
    // Make a note of all flags that are equal joined.
    var result = Set<String>()
    for opt in SwiftOptions.Option.allOptions {
      switch opt.kind {
      case .separate, .input, .flag, .remaining, .multiArg:
        continue
      case .joined, .joinedOrSeparate, .commaJoined:
        if opt.spelling.hasSuffix("=") {
          result.insert(String(opt.spelling.dropLast()))
        }
      }
    }
    return result
  }()

  fileprivate var spacingSpec: Command.FlagSpec.OptionSpacingSpec {
    var spacing = Command.OptionSpacingSpec()
    switch kind {
    case .input, .remaining:
      fatalError("Not handled")
    case .flag:
      break
    case .joined, .commaJoined:
      spacing.insert(.unspaced)
    case .separate, .multiArg:
      spacing.insert(.spaced)
    case .joinedOrSeparate:
      spacing.insert([.unspaced, .spaced])
    }
    if Self.optionsWithJoinedEquals.contains(spelling) {
      spacing.insert(.equals)
    }
    return spacing
  }
}

extension Command.FlagSpec {
  fileprivate init(_ options: [SwiftOptions.Option]) {
    self.init(options.map { .init(.swiftc($0), option: $0.spacingSpec) })
  }
}

extension Command.Flag {
  // Swift + Clang
  static let D = dash("D")
  static let I = dash("I")
  static let target = dash("target")

  // Clang
  static let isystem = dash("isystem")
  static let isysroot = dash("isysroot")
  static let f = dash("f")
  static let fDiagnosticsColor = dash("fdiagnostics-color")
  static let U = dash("U")
  static let W = dash("W")
  static let std = dash("std")

  // Swift
  static let cxxInteroperabilityMode = 
    swiftc(.cxxInteroperabilityMode)
  static let enableExperimentalCxxInterop = 
    swiftc(.enableExperimentalCxxInterop)
  static let enableExperimentalFeature =
    swiftc(.enableExperimentalFeature)
  static let enableLibraryEvolution =
    swiftc(.enableLibraryEvolution)
  static let experimentalSkipAllFunctionBodies =
    swiftc(.experimentalSkipAllFunctionBodies)
  static let experimentalSkipNonInlinableFunctionBodies =
    swiftc(.experimentalSkipNonInlinableFunctionBodies)
  static let experimentalSkipNonInlinableFunctionBodiesWithoutTypes = 
    swiftc(.experimentalSkipNonInlinableFunctionBodiesWithoutTypes)
  static let enableUpcomingFeature =
    swiftc(.enableUpcomingFeature)
  static let disableExperimentalFeature =
    swiftc(.disableExperimentalFeature)
  static let disableUpcomingFeature =
    swiftc(.disableUpcomingFeature)
  static let F =
    swiftc(.F)
  static let Fsystem =
    swiftc(.Fsystem)
  static let libraryLevel =
    swiftc(.libraryLevel)
  static let moduleName =
    swiftc(.moduleName)
  static let moduleAbiName =
    swiftc(.moduleAbiName)
  static let moduleLinkName =
    swiftc(.moduleLinkName)
  static let nostdimport =
    swiftc(.nostdimport)
  static let O =
    swiftc(.O)
  static let Onone =
    swiftc(.Onone)
  static let packageName =
    swiftc(.packageName)
  static let parseAsLibrary =
    swiftc(.parseAsLibrary)
  static let parseStdlib =
    swiftc(.parseStdlib)
  static let runtimeCompatibilityVersion =
    swiftc(.runtimeCompatibilityVersion)
  static let sdk =
    swiftc(.sdk)
  static let strictMemorySafety =
    swiftc(.strictMemorySafety)
  static let swiftVersion =
    swiftc(.swiftVersion)
  static let warnImplicitOverrides =
    swiftc(.warnImplicitOverrides)
  static let wholeModuleOptimization =
    swiftc(.wholeModuleOptimization)
  static let wmo =
    swiftc(.wmo)
  static let Xcc =
    swiftc(.Xcc)
  static let Xfrontend =
    swiftc(.Xfrontend)
  static let Xllvm =
    swiftc(.Xllvm)
}

extension KnownCommand {
  private static let clangSpec = Command.FlagSpec([
    .init(.I, option: .equals, .unspaced, .spaced),
    .init(.D, option: .unspaced, .spaced),
    .init(.U, option: .unspaced, .spaced),
    .init(.W, option: .unspaced),

    // This isn't an actual Clang flag, but it allows us to scoop up all the
    // -f[...] flags.
    // FIXME: We ought to see if we can get away with preserving unknown flags.
    .init(.f, option: .unspaced),

    .init(.fDiagnosticsColor),

    // FIXME: Really we ought to map to Xcode's SDK
    .init(.isystem, option: .unspaced, .spaced),
    .init(.isysroot, option: .unspaced, .spaced),

    .init(.std, option: .equals),
    .init(.target, option: .spaced),
  ])

  // FIXME: We currently only parse a small subset of the supported driver
  // options. This is because:
  //
  // - It avoids including incompatible options (e.g options only suitable when
  //   emitting modules when we want to do a regular build).
  // - It avoids including options that produce unnecessary outputs (e.g
  //   dependencies, object files), especially as they would be outputting into
  //   the Ninja build, which needs to be left untouched (maybe we could filter
  //   out options that have paths that point into the build dir?).
  // - It avoids including options that do unnecessary work (e.g emitting debug
  //   info, code coverage).
  // - It's quicker.
  //
  // This isn't great though, and we probably ought to revisit this, especially
  // if the driver starts categorizing its options such that we can better
  // reason about which we want to use. It should also be noted that we
  // currently allow arbitrary options to be passed through -Xfrontend, we may
  // want to reconsider that.
  // NOTE: You can pass '--log-level debug' to see the options that are
  // currently being missed.
  private static let swiftOptions: [SwiftOptions.Option] = [
    .cxxInteroperabilityMode,
    .D,
    .disableAutolinkingRuntimeCompatibilityDynamicReplacements,
    .enableBuiltinModule,
    .enableExperimentalCxxInterop,
    .enableExperimentalFeature,
    .enableLibraryEvolution,
    .experimentalSkipAllFunctionBodies,
    .experimentalSkipNonInlinableFunctionBodies,
    .experimentalSkipNonInlinableFunctionBodiesWithoutTypes,
    .enableUpcomingFeature,
    .disableExperimentalFeature,
    .disableUpcomingFeature,
    .F,
    .Fsystem,
    .I,
    .nostdimport,
    .O,
    .Onone,
    .libraryLevel,
    .moduleName,
    .moduleAbiName,
    .moduleLinkName,
    .packageName,
    .parseAsLibrary,
    .parseStdlib,
    .runtimeCompatibilityVersion,
    .target,
    .sdk,
    .strictMemorySafety,
    .swiftVersion,
    .warnImplicitOverrides,
    .wholeModuleOptimization,
    .wmo,
    .Xcc,
    .Xfrontend,
    .Xllvm,
  ]

  private static let swiftcSpec = Command.FlagSpec(
    swiftOptions.filter { !$0.attributes.contains(.noDriver) } + [
      // Not currently listed as a driver option, but it used to be. Include
      // for better compatibility.
      .enableExperimentalCxxInterop
    ]
  )

  private static let swiftFrontendSpec = Command.FlagSpec(
    swiftOptions.filter { $0.attributes.contains(.frontend) }
  )

  var flagSpec: Command.FlagSpec {
    switch self {
    case .clang:
      Self.clangSpec
    case .swiftc:
      Self.swiftcSpec
    case .swiftFrontend:
      Self.swiftFrontendSpec
    }
  }
}
