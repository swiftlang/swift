//===--- CompilerPlugin.swift - Compiler plugin library support -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Do not modify without modifying swift::CompilerPlugin::Kind in
// include/swift/AST/CompilerPlugin.h.
@frozen
public struct _CompilerPluginKind {
  @usableFromInline
  enum RawValue: UInt32 {
    case expressionMacro
  }
  @usableFromInline var rawValue: RawValue

  public static var expressionMacro: Self {
    Self(rawValue: .expressionMacro)
  }
}

/// A compiler plugin that can be loaded by the Swift compiler for code rewrite
/// and custom compilation.
public protocol _CompilerPlugin {
  // Note:
  // - Do not modify the protocol layout without updating
  //   `CompilerPlugin::WitnessTableEntry`.
  // - Use C-compatible types and tuples thereof to ensure that we can invoke
  //   these methods from C.

  static func _name() -> (UnsafePointer<UInt8>, count: Int)

  static func _kind() -> _CompilerPluginKind

  /// Returns new source code by transforming the given source code, or nil if
  /// there's no transform.
  ///
  /// - Parameters:
  ///   - code: A buffer containing the source code in UTF-8.
  ///   - context: The compiler context.
  /// - Returns: A newly allocated, null-terminated buffer containing the
  ///   transformed source code. The caller is responsible for managing the
  ///   memory.
  static func _rewrite(
    targetModuleName: UnsafePointer<UInt8>,
    targetModuleNameCount: Int,
    filePath: UnsafePointer<UInt8>,
    filePathCount: Int,
    sourceFileText: UnsafePointer<UInt8>,
    sourceFileTextCount: Int,
    localSourceText: UnsafePointer<UInt8>,
    localSourceTextCount: Int
  ) -> (UnsafePointer<UInt8>?, count: Int)
}
