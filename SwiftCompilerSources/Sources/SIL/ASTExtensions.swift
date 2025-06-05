//===--- ASTExtensions.swift ----------------------------------------------===//
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

import AST
import SILBridging

extension TypeProperties {
  // Lowers the AST type to a SIL type - in a specific function.
  // In contrast to `silType` this always succeeds. Still, it's not allowed to do this for certain AST types
  // which are not present in SIL, like an `InOut` or LValue types.
  //
  // If `maximallyAbstracted` is true, the lowering is done with a completely opaque abstraction pattern
  // (see AbstractionPattern for details).
  public func loweredType(in function: Function, maximallyAbstracted: Bool = false) -> Type {
    function.bridged.getLoweredType(rawType.bridged, maximallyAbstracted).type.objectType
  }
}

extension CanonicalType {
  // This can yield nil if the AST type is not a lowered type.
  // For example, if the AST type is a `AnyFunctionType` for which the lowered type would be a `SILFunctionType`.
  public var silType: Type? {
    BridgedType.createSILType(bridged).typeOrNil
  }
}

extension Decl {
  public var location: Location { Location(bridged: BridgedLocation.fromNominalTypeDecl(bridged)) }
}

extension NominalTypeDecl {
  public func isResilient(in function: Function) -> Bool {
    function.bridged.isResilientNominalDecl(bridged)
  }
}

extension ClassDecl {
  public var superClassType: Type? {
    self.superClass?.canonical.silType!
  }
}

extension SubstitutionMap {
  /// Returns the substitutions to specialize a method.
  ///
  /// If this is a default witness methods (`selfType` != nil) it has generic self type. In this case
  /// the generic self parameter is at depth 0 and the actual generic parameters of the substitution map
  /// are at depth + 1, e.g:
  /// ```
  ///     @convention(witness_method: P) <τ_0_0><τ_1_0 where τ_0_0 : GenClass<τ_1_0>.T>
  ///                                       ^      ^
  ///                                    self      params of substitution map at depth + 1
  /// ```
  public func getMethodSubstitutions(for method: Function, selfType: CanonicalType? = nil) -> SubstitutionMap {
    return SubstitutionMap(bridged: method.bridged.getMethodSubstitutions(bridged,
                                                                          selfType?.bridged ?? BridgedCanType()))
  }
}

extension Conformance {
  /// Returns true if the conformance is not isolated or if its isolation matches
  /// the isolation in `function`.
  public func matchesActorIsolation(in function: Function) -> Bool {
    return function.bridged.conformanceMatchesActorIsolation(bridged)
  }
}

extension DiagnosticEngine {
  public func diagnose(_ id: DiagID, _ args: DiagnosticArgument..., at location: Location) {
    diagnose(id, args, at: location.getSourceLocation(diagnosticEngine: self))
  }

  public func diagnose(_ id: DiagID, _ args: [DiagnosticArgument], at location: Location) {
    diagnose(id, args, at: location.getSourceLocation(diagnosticEngine: self))
  }
}

extension Diagnostic where SourceLocation == Location {
  public init(_ id: DiagID, _ arguments: DiagnosticArgument..., at location: Location) {
    self.init(id, arguments, at: location)
  }
}
