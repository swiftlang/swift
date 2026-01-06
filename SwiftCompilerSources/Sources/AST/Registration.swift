//===--- Registration.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic
import ASTBridging

public func registerAST() {
  registerDecl(EnumDecl.self)
  registerDecl(StructDecl.self)
  registerDecl(ClassDecl.self)
  registerDecl(ProtocolDecl.self)
  registerDecl(BuiltinTupleDecl.self)
  registerDecl(OpaqueTypeDecl.self)
  registerDecl(TypeAliasDecl.self)
  registerDecl(GenericTypeParamDecl.self)
  registerDecl(AssociatedTypeDecl.self)
  registerDecl(ModuleDecl.self)
  registerDecl(VarDecl.self)
  registerDecl(ParamDecl.self)
  registerDecl(SubscriptDecl.self)
  registerDecl(ConstructorDecl.self)
  registerDecl(DestructorDecl.self)
  registerDecl(FuncDecl.self)
  registerDecl(AccessorDecl.self)
  registerDecl(MacroDecl.self)
  registerDecl(EnumElementDecl.self)
  registerDecl(ExtensionDecl.self)
  registerDecl(TopLevelCodeDecl.self)
  registerDecl(ImportDecl.self)
  registerDecl(UsingDecl.self)
  registerDecl(PrecedenceGroupDecl.self)
  registerDecl(MissingDecl.self)
  registerDecl(MissingMemberDecl.self)
  registerDecl(PatternBindingDecl.self)
  registerDecl(EnumCaseDecl.self)
  registerDecl(InfixOperatorDecl.self)
  registerDecl(PrefixOperatorDecl.self)
  registerDecl(PostfixOperatorDecl.self)
  registerDecl(MacroExpansionDecl.self)
}

private func registerDecl<T: AnyObject>(_ cl: T.Type) {
  "\(cl)"._withBridgedStringRef { nameStr in
    let metatype = unsafeBitCast(cl, to: SwiftMetatype.self)
    registerBridgedDecl(nameStr, metatype)
  }
}
