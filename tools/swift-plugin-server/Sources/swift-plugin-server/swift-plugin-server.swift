//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_spi(PluginMessage) import SwiftCompilerPluginMessageHandling
import SwiftSyntaxMacros
import CSwiftPluginServer

@main
final class SwiftPluginServer {
  struct MacroRef: Hashable {
    var moduleName: String
    var typeName: String
    init(_ moduleName: String, _ typeName: String) {
      self.moduleName = moduleName
      self.typeName = typeName
    }
  }

  struct LoadedLibraryPlugin {
    var libraryPath: String
    var handle: UnsafeMutableRawPointer
  }

  /// Loaded dylib handles associated with the module name.
  var loadedLibraryPlugins: [String: LoadedLibraryPlugin] = [:]

  /// Resolved cached macros.
  var resolvedMacros: [MacroRef: Macro.Type] = [:]

  /// @main entry point.
  static func main() throws {
    let connection = try StandardIOMessageConnection()
    let listener = CompilerPluginMessageListener(
      connection: connection,
      provider: self.init()
    )
    try listener.main()
  }
}

extension SwiftPluginServer: PluginProvider {
  /// Load a macro implementation from the dynamic link library.
  func loadPluginLibrary(libraryPath: String, moduleName: String) throws {
    var errorMessage: UnsafePointer<CChar>?
    guard let dlHandle = PluginServer_load(libraryPath, &errorMessage) else {
      throw PluginServerError(message: "loader error: " + String(cString: errorMessage!))
    }
    loadedLibraryPlugins[moduleName] = LoadedLibraryPlugin(
      libraryPath: libraryPath,
      handle: dlHandle
    )
  }

  /// Lookup a loaded macro by a pair of module name and type name.
  func resolveMacro(moduleName: String, typeName: String) throws -> Macro.Type {
    if let resolved = resolvedMacros[.init(moduleName, typeName)] {
      return resolved
    }

    // Find 'dlopen'ed library for the module name.
    guard let plugin = loadedLibraryPlugins[moduleName] else {
      // NOTE: This should be unreachable. Compiler should not use this server
      // unless the plugin loading succeeded.
      throw PluginServerError(message: "(plugin-server) plugin not loaded for module '\(moduleName)'")
    }

    // Lookup the type metadata.
    var errorMessage: UnsafePointer<CChar>?
    guard let macroTypePtr = PluginServer_lookupMacroTypeMetadataByExternalName(
      moduleName, typeName, plugin.handle, &errorMessage
    ) else {
      throw PluginServerError(message: "macro implementation type '\(moduleName).\(typeName)' could not be found in library plugin '\(plugin.libraryPath)'")
    }

    // THe type must be a 'Macro' type.
    let macroType = unsafeBitCast(macroTypePtr, to: Any.Type.self)
    guard let macro = macroType as? Macro.Type else {
      throw PluginServerError(message: "type '\(moduleName).\(typeName)' is not a valid macro implementation type in library plugin '\(plugin.libraryPath)'")
    }

    // Cache the resolved type.
    resolvedMacros[.init(moduleName, typeName)] = macro
    return macro
  }

  /// This 'PluginProvider' implements 'loadLibraryMacro()'.
  var features: [SwiftCompilerPluginMessageHandling.PluginFeature] {
    [.loadPluginLibrary]
  }
}

struct PluginServerError: Error, CustomStringConvertible {
  var description: String
  init(message: String) {
    self.description = message
  }
}
