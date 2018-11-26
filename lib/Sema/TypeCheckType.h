//===--- TypeCheckType.h - Type Resolution Code ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines utilities for resolving types.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_TYPE_CHECK_TYPE_H
#define SWIFT_SEMA_TYPE_CHECK_TYPE_H

#include "swift/AST/TypeResolutionStage.h"
#include "llvm/ADT/None.h"

namespace swift {

/// Flags that describe the context of type checking a pattern or
/// type.
enum class TypeResolutionFlags : uint16_t {
  /// Whether to allow unspecified types within a pattern.
  AllowUnspecifiedTypes = 1 << 0,

  /// Whether to allow unbound generic types.
  AllowUnboundGenerics = 1 << 1,

  /// Whether an unavailable protocol can be referenced.
  AllowUnavailableProtocol = 1 << 2,

  /// Whether we should allow references to unavailable types.
  AllowUnavailable = 1 << 3,

  /// Whether the given type can override the type of a typed pattern.
  OverrideType = 1 << 4,

  /// Whether we are validating the type for SIL.
  // FIXME: Move this flag to TypeResolverContext.
  SILType = 1 << 5,

  /// Whether we are parsing a SIL file.  Not the same as SILType,
  /// because the latter is not set if we're parsing an AST type.
  SILMode = 1 << 6,

  /// Whether this is a resolution based on a non-inferred type pattern.
  FromNonInferredPattern = 1 << 7,

  /// Whether this type resolution is guaranteed not to affect downstream files.
  KnownNonCascadingDependency = 1 << 8,

  /// Whether we are at the direct base of a type expression.
  Direct = 1 << 9,

  /// Whether we should not produce diagnostics if the type is invalid.
  SilenceErrors = 1 << 10,
};

/// Type resolution contexts that require special handling.
enum class TypeResolverContext : uint8_t {
  /// No special type handling is required.
  None,

  /// Whether we are checking the parameter list of a function.
  AbstractFunctionDecl,

  /// Whether we are checking the parameter list of a subscript.
  SubscriptDecl,

  /// Whether we are checking the parameter list of a closure.
  ClosureExpr,

  /// Whether we are in the input type of a function, or under one level of
  /// tuple type.  This is not set for multi-level tuple arguments.
  /// See also: TypeResolutionFlags::Direct
  FunctionInput,

  /// Whether this is a variadic function input.
  VariadicFunctionInput,

  /// Whether we are in the result type of a function, including multi-level
  /// tuple return values. See also: TypeResolutionFlags::Direct
  FunctionResult,

  /// Whether we are in the result type of a function body that is
  /// known to produce dynamic Self.
  DynamicSelfResult,

  /// Whether we are in a protocol's where clause
  ProtocolWhereClause,

  /// Whether this is a pattern binding entry.
  PatternBindingDecl,

  /// Whether we are the variable type in a for/in statement.
  ForEachStmt,

  /// Whether we are binding an extension declaration, which limits
  /// the lookup.
  ExtensionBinding,

  /// Whether this type is being used in an expression or local declaration.
  ///
  /// This affects what sort of dependencies are recorded when resolving the
  /// type.
  InExpression,

  /// Whether this type is being used in a cast or coercion expression.
  ExplicitCastExpr,

  /// Whether this type is the value carried in an enum case.
  EnumElementDecl,

  /// Whether this is the payload subpattern of an enum pattern.
  EnumPatternPayload,

  /// Whether we are checking the underlying type of a typealias.
  TypeAliasDecl,

  /// Whether we are in a requirement of a generic declaration
  GenericRequirement,

  /// Whether we are in a type argument for an optional
  ImmediateOptionalTypeArgument,

  /// Whether this is the type of an editor placeholder.
  EditorPlaceholderExpr,
};

/// Options that determine how type resolution should work.
class TypeResolutionOptions {
  using Context = TypeResolverContext;

  // The "base" type resolution context. This never changes.
  Context base = Context::None;
  // The current type resolution context.
  Context context = Context::None;
  // TypeResolutionFlags
  uint16_t flags = 0;
  static_assert(sizeof(flags) == sizeof(TypeResolutionFlags),
      "Flags size error");

public:
  ~TypeResolutionOptions() = default;
  TypeResolutionOptions(const TypeResolutionOptions &) = default;
  TypeResolutionOptions(TypeResolutionOptions &&) = default;
  TypeResolutionOptions &operator =(const TypeResolutionOptions &) = default;
  TypeResolutionOptions &operator =(TypeResolutionOptions &&) = default;

  // NOTE: Use either setContext() or explicit construction and assignment.
  void operator =(const Context &) = delete;
  void operator =(Context &&) = delete;

  // NOTE: "None" might be more permissive than one wants, therefore no
  // reasonable default context is possible.
  TypeResolutionOptions() = delete;

  TypeResolutionOptions(Context context) : base(context), context(context),
      flags(unsigned(TypeResolutionFlags::Direct)) {}
  // Helper forwarding constructors:
  TypeResolutionOptions(llvm::NoneType) : TypeResolutionOptions(Context::None){}

  /// Test the current type resolution base context.
  bool hasBase(Context context) const { return base == context; }

  /// Get the base type resolution context.
  Context getBaseContext() const { return base; }

  /// Test the current type resolution context.
  bool is(Context context) const { return this->context == context; }

  /// Get the current type resolution context.
  Context getContext() const { return context; }
  /// Set the current type resolution context.
  void setContext(Context newContext) {
    context = newContext;
    flags &= ~unsigned(TypeResolutionFlags::Direct);
  }
  void setContext(llvm::NoneType) { setContext(Context::None); }

  /// Get the current flags.
  TypeResolutionFlags getFlags() const { return TypeResolutionFlags(flags); }

  /// Is this type resolution context an expression.
  bool isAnyExpr() const {
    switch (base) {
    case Context::InExpression:
    case Context::ExplicitCastExpr:
    case Context::ForEachStmt:
    case Context::PatternBindingDecl:
    case Context::EditorPlaceholderExpr:
    case Context::ClosureExpr:
      return true;
    case Context::None:
    case Context::FunctionInput:
    case Context::VariadicFunctionInput:
    case Context::FunctionResult:
    case Context::DynamicSelfResult:
    case Context::ProtocolWhereClause:
    case Context::ExtensionBinding:
    case Context::SubscriptDecl:
    case Context::EnumElementDecl:
    case Context::EnumPatternPayload:
    case Context::TypeAliasDecl:
    case Context::GenericRequirement:
    case Context::ImmediateOptionalTypeArgument:
    case Context::AbstractFunctionDecl:
      return false;
    }
  }

  /// Determine whether all of the given options are set.
  bool contains(TypeResolutionFlags set) const {
    return !static_cast<bool>(unsigned(set) & ~unsigned(flags));
  }

  /// Produce type resolution options with additional flags.
  friend TypeResolutionOptions operator|(TypeResolutionOptions lhs,
                                         TypeResolutionFlags rhs) {
    return lhs |= rhs;
  }

  /// Merge additional flags into type resolution options.
  friend TypeResolutionOptions &operator|=(TypeResolutionOptions &lhs,
                                           TypeResolutionFlags rhs) {
    lhs.flags |= unsigned(rhs);
    return lhs;
  }

  /// Test whether any given flag is set in the type resolution options.
  friend bool operator&(TypeResolutionOptions lhs, TypeResolutionFlags rhs) {
    return lhs.flags & unsigned(rhs);
  }

  /// Produce type resolution options with removed flags.
  friend TypeResolutionOptions operator-(TypeResolutionOptions lhs,
                                         TypeResolutionFlags rhs) {
    return lhs -= rhs;
  }

  /// Remove the flags from the type resolution options.
  friend TypeResolutionOptions &operator-=(TypeResolutionOptions &lhs,
                                           TypeResolutionFlags rhs) {
    lhs.flags &= ~unsigned(rhs);
    return lhs;
  }
  /// Strip the contextual options from the given type resolution options.
  inline TypeResolutionOptions withoutContext(bool preserveSIL = false) const {
    auto copy = *this;
    copy.setContext(None);
    // FIXME: Move SILType to TypeResolverContext.
    if (!preserveSIL) copy -= TypeResolutionFlags::SILType;
    return copy;
  }
};

/// Handles the resolution of types within a given declaration context,
/// which might involve resolving generic parameters to a particular
/// stage.
class TypeResolution {
  DeclContext *dc;
  TypeResolutionStage stage;

  union {
    /// The generic environment used to map to archetypes.
    GenericEnvironment *genericEnv;

    /// The generic signature
    struct {
      /// The generic signature to use for type resolution.
      GenericSignature *genericSig;

      /// The generic signature builder that will answer queries about
      /// generic types.
      mutable GenericSignatureBuilder *builder;
    } complete;
  };

  TypeResolution(DeclContext *dc, TypeResolutionStage stage)
    : dc(dc), stage(stage) { }

  GenericSignatureBuilder *getGenericSignatureBuilder() const;

public:
  /// Form a type resolution for the structure of a type, which does not
  /// attempt to resolve member types of type parameters to a particular
  /// associated type.
  static TypeResolution forStructural(DeclContext *dc);

  /// Form a type resolution for an interface type, which is a complete
  /// description of the type using generic parameters.
  static TypeResolution forInterface(DeclContext *dc);

  /// Form a type resolution for an interface type, which is a complete
  /// description of the type using generic parameters.
  static TypeResolution forInterface(DeclContext *dc,
                                     GenericSignature *genericSig);

  /// Form a type resolution for a contextual type, which is a complete
  /// description of the type using the archetypes of the given declaration
  /// context.
  static TypeResolution forContextual(DeclContext *dc);

  /// Form a type resolution for a contextual type, which is a complete
  /// description of the type using the archetypes of the given generic
  /// environment.
  static TypeResolution forContextual(DeclContext *dc,
                                      GenericEnvironment *genericEnv);

  /// Retrieve the ASTContext in which this resolution occurs.
  ASTContext &getASTContext() const { return dc->getASTContext(); }

  /// Retrieve the declaration context in which type resolution will be
  /// performed.
  DeclContext *getDeclContext() const { return dc; }

  /// Retrieve the type resolution stage.
  TypeResolutionStage getStage() const { return stage; }

  /// Retrieves the generic signature for the context, or NULL if there is
  /// no generic signature to resolve types.
  GenericSignature *getGenericSignature() const;

  /// \brief Resolves a TypeRepr to a type.
  ///
  /// Performs name binding, checking of generic arguments, and so on in order
  /// to create a well-formed type.
  ///
  /// \param TyR The type representation to check.
  ///
  /// \param options Options that alter type resolution.
  ///
  /// \returns a well-formed type or an ErrorType in case of an error.
  Type resolveType(TypeRepr *TyR, TypeResolutionOptions options);

  /// Whether this type resolution uses archetypes (vs. generic parameters).
  bool usesArchetypes() const;

  /// Map the given type (that involves generic parameters)
  Type mapTypeIntoContext(Type type) const;

  /// Resolve a reference to a member type of the given (dependent) base and
  /// name.
  Type resolveDependentMemberType(Type baseTy, DeclContext *DC,
                                  SourceRange baseRange,
                                  ComponentIdentTypeRepr *ref) const;

  /// Determine whether the given two types are equivalent within this
  /// type resolution context.
  bool areSameType(Type type1, Type type2) const;
};

} // end namespace swift

#endif /* SWIFT_SEMA_TYPE_CHECK_TYPE_H */
