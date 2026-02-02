//===--- DeclContext.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging

public protocol DeclContext : AnyObject {
  var bridgedDeclContext: BridgedDeclContext { get }
}

extension DeclContext {
  public var astContext: ASTContext { bridgedDeclContext.astContext }
}

// Used for DeclContext classes which are not Decls and are not bridged, yet. E.g. `FileUnit`.
// TODO: once we have bridged those DeclContext classes, get rid of UnknownDeclContext
public class UnknownDeclContext : DeclContext {
  public var bridgedDeclContext: BridgedDeclContext
  public init(bridged: BridgedDeclContext) { bridgedDeclContext = bridged }
}
