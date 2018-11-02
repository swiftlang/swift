//===--- DerivedConformanceCodable.cpp - Derived Codable ------------------===//
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
//
// This file implements explicit derivation of the Encodable and Decodable
// protocols for a struct or class.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;
using namespace DerivedConformance;

/// Returns whether the type represented by the given ClassDecl inherits from a
/// type which conforms to the given protocol.
///
/// \param target The \c ClassDecl whose superclass to look up.
///
/// \param proto The protocol to check conformance for.
static bool inheritsConformanceTo(ClassDecl *target, ProtocolDecl *proto) {
  if (!target->hasSuperclass())
    return false;

  auto *superclassDecl = target->getSuperclassDecl();
  auto *superclassModule = superclassDecl->getModuleContext();
  return (bool)superclassModule->lookupConformance(target->getSuperclass(),
                                                   proto);
}

/// Returns whether the superclass of the given class conforms to Encodable.
///
/// \param target The \c ClassDecl whose superclass to check.
static bool superclassIsEncodable(ClassDecl *target) {
  auto &C = target->getASTContext();
  return inheritsConformanceTo(target,
                               C.getProtocol(KnownProtocolKind::Encodable));
}

/// Returns whether the superclass of the given class conforms to Decodable.
///
/// \param target The \c ClassDecl whose superclass to check.
static bool superclassIsDecodable(ClassDecl *target) {
  auto &C = target->getASTContext();
  return inheritsConformanceTo(target,
                               C.getProtocol(KnownProtocolKind::Decodable));
}

/// Represents the possible outcomes of checking whether a decl conforms to
/// Encodable or Decodable.
enum CodableConformanceType {
  TypeNotValidated,
  DoesNotConform,
  Conforms
};

/// Returns whether the given type conforms to the given {En,De}codable
/// protocol.
///
/// \param tc The typechecker to use in validating {En,De}codable conformance.
///
/// \param context The \c DeclContext the var declarations belong to.
///
/// \param target The \c Type to validate.
///
/// \param proto The \c ProtocolDecl to check conformance to.
static CodableConformanceType typeConformsToCodable(TypeChecker &tc,
                                                    DeclContext *context,
                                                    Type target, bool isIUO,
                                                    ProtocolDecl *proto) {
  // Some generic types need to be introspected to get at their "true" Codable
  // conformance.
  if (auto referenceType = target->getAs<ReferenceStorageType>()) {
    // This is a weak/unowned/unmanaged var. Get the inner type before checking
    // conformance.
    target = referenceType->getReferentType();
  }

  if (isIUO)
    return typeConformsToCodable(tc, context, target->getOptionalObjectType(),
                                 false, proto);

  return tc.conformsToProtocol(target, proto, context,
                               ConformanceCheckFlags::Used) ? Conforms
                                                            : DoesNotConform;
}

/// Returns whether the given variable conforms to the given {En,De}codable
/// protocol.
///
/// \param tc The typechecker to use in validating {En,De}codable conformance.
///
/// \param context The \c DeclContext the var declarations belong to.
///
/// \param varDecl The \c VarDecl to validate.
///
/// \param proto The \c ProtocolDecl to check conformance to.
static CodableConformanceType varConformsToCodable(TypeChecker &tc,
                                                   DeclContext *context,
                                                   VarDecl *varDecl,
                                                   ProtocolDecl *proto) {
  // If the decl doesn't yet have a type, we may be seeing it before the type
  // checker has gotten around to evaluating its type. For example:
  //
  // func foo() {
  //   let b = Bar(from: decoder) // <- evaluates Bar conformance to Codable,
  //                              //    forcing derivation
  // }
  //
  // struct Bar : Codable {
  //   var x: Int // <- we get to valuate x's var decl here, but its type
  //              //    hasn't yet been evaluated
  // }
  //
  // Validate the decl eagerly.
  if (!varDecl->hasType())
    tc.validateDecl(varDecl);

  // If the var decl didn't validate, it may still not have a type; confirm it
  // has a type before ensuring the type conforms to Codable.
  if (!varDecl->hasType())
    return TypeNotValidated;

  bool isIUO =
      varDecl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
  return typeConformsToCodable(tc, context, varDecl->getType(), isIUO, proto);
}

/// Validates the given CodingKeys enum decl by ensuring its cases are a 1-to-1
/// match with the stored vars of the given type.
///
/// \param tc The typechecker to use in validating {En,De}codable conformance.
///
/// \param codingKeysDecl The \c CodingKeys enum decl to validate.
///
/// \param target The nominal type decl to validate the \c CodingKeys against.
///
/// \param proto The {En,De}codable protocol to validate all the keys conform
/// to.
static bool
validateCodingKeysEnum(TypeChecker &tc, EnumDecl *codingKeysDecl,
                       NominalTypeDecl *target, ProtocolDecl *proto) {
  // Look through all var decls in the given type.
  // * Filter out lazy/computed vars.
  // * Filter out ones which are present in the given decl (by name).
  //
  // If any of the entries in the CodingKeys decl are not present in the type
  // by name, then this decl doesn't match.
  // If there are any vars left in the type which don't have a default value
  // (for Decodable), then this decl doesn't match.

  // Here we'll hold on to properties by name -- when we've validated a property
  // against its CodingKey entry, it will get removed.
  llvm::SmallDenseMap<Identifier, VarDecl *, 8> properties;
  for (auto *varDecl : target->getStoredProperties(/*skipInaccessible=*/true)) {
    if (varDecl->getAttrs().hasAttribute<LazyAttr>())
      continue;

    properties[varDecl->getName()] = varDecl;
  }

  bool propertiesAreValid = true;
  for (auto elt : codingKeysDecl->getAllElements()) {
    auto it = properties.find(elt->getName());
    if (it == properties.end()) {
      tc.diagnose(elt->getLoc(), diag::codable_extraneous_codingkey_case_here,
                  elt->getName());
      // TODO: Investigate typo-correction here; perhaps the case name was
      //       misspelled and we can provide a fix-it.
      propertiesAreValid = false;
      continue;
    }

    // We have a property to map to. Ensure it's {En,De}codable.
    auto conformance = varConformsToCodable(tc, target->getDeclContext(),
                                            it->second, proto);
    switch (conformance) {
      case Conforms:
        // The property was valid. Remove it from the list.
        properties.erase(it);
        break;

      case DoesNotConform:
        tc.diagnose(it->second->getLoc(),
                    diag::codable_non_conforming_property_here,
                    proto->getDeclaredType(), it->second->getType());
        LLVM_FALLTHROUGH;

      case TypeNotValidated:
        // We don't produce a diagnostic for a type which failed to validate.
        // This will produce a diagnostic elsewhere anyway.
        propertiesAreValid = false;
        continue;
    }
  }

  if (!propertiesAreValid)
    return false;

  // If there are any remaining properties which the CodingKeys did not cover,
  // we can skip them on encode. On decode, though, we can only skip them if
  // they have a default value.
  if (!properties.empty() &&
      proto->isSpecificProtocol(KnownProtocolKind::Decodable)) {
    for (auto it = properties.begin(); it != properties.end(); ++it) {
      // If the var is default initializable, then it need not have an explicit
      // initial value.
      auto *varDecl = it->second;
      if (auto pbd = varDecl->getParentPatternBinding()) {
        if (pbd->isDefaultInitializable())
          continue;
      }

      if (varDecl->getParentInitializer())
        continue;

      // The var was not default initializable, and did not have an explicit
      // initial value.
      propertiesAreValid = false;
      tc.diagnose(it->second->getLoc(), diag::codable_non_decoded_property_here,
                  proto->getDeclaredType(), it->first);
    }
  }

  return propertiesAreValid;
}

/// A type which has information about the validity of an encountered
/// CodingKeys type.
struct CodingKeysValidity {
  bool hasType;
  bool isValid;
  CodingKeysValidity(bool ht, bool iv) : hasType(ht), isValid(iv) {}
};

/// Returns whether the given type has a valid nested \c CodingKeys enum.
///
/// If the type has an invalid \c CodingKeys entity, produces diagnostics to
/// complain about the error. In this case, the error result will be true -- in
/// the case where we don't have a valid CodingKeys enum and have produced
/// diagnostics here, we don't want to then attempt to synthesize a CodingKeys
/// enum.
///
/// \param tc The typechecker to use in validating {En,Decodable} conformance.
///
/// \param target The type decl whose nested \c CodingKeys type to validate.
///
/// \param proto The {En,De}codable protocol to ensure the properties matching
/// the keys conform to.
///
/// \returns A \c CodingKeysValidity value representing the result of the check.
static CodingKeysValidity hasValidCodingKeysEnum(TypeChecker &tc,
                                                 NominalTypeDecl *target,
                                                 ProtocolDecl *proto) {
  auto &C = tc.Context;
  auto codingKeysDecls = target->lookupDirect(DeclName(C.Id_CodingKeys));
  if (codingKeysDecls.empty())
    return CodingKeysValidity(/*hasType=*/false, /*isValid=*/true);

  // Only ill-formed code would produce multiple results for this lookup.
  // This would get diagnosed later anyway, so we're free to only look at the
  // first result here.
  auto result = codingKeysDecls.front();

  auto *codingKeysTypeDecl = dyn_cast<TypeDecl>(result);
  if (!codingKeysTypeDecl) {
    tc.diagnose(result->getLoc(),
                diag::codable_codingkeys_type_is_not_an_enum_here,
                proto->getDeclaredType());
    return CodingKeysValidity(/*hasType=*/true, /*isValid=*/false);
  }

  // If the decl hasn't been validated yet, do so.
  tc.validateDecl(codingKeysTypeDecl);

  // CodingKeys may be a typealias. If so, follow the alias to its canonical
  // type.
  auto codingKeysType = codingKeysTypeDecl->getDeclaredInterfaceType();
  if (isa<TypeAliasDecl>(codingKeysTypeDecl))
    codingKeysTypeDecl = codingKeysType->getAnyNominal();

  // Ensure that the type we found conforms to the CodingKey protocol.
  auto *codingKeyProto = C.getProtocol(KnownProtocolKind::CodingKey);
  if (!tc.conformsToProtocol(codingKeysType, codingKeyProto,
                             target->getDeclContext(),
                             ConformanceCheckFlags::Used)) {
    // If CodingKeys is a typealias which doesn't point to a valid nominal type,
    // codingKeysTypeDecl will be nullptr here. In that case, we need to warn on
    // the location of the usage, since there isn't an underlying type to
    // diagnose on.
    SourceLoc loc = codingKeysTypeDecl ?
                    codingKeysTypeDecl->getLoc() :
                    cast<TypeDecl>(result)->getLoc();

    tc.diagnose(loc, diag::codable_codingkeys_type_does_not_conform_here,
                proto->getDeclaredType());

    return CodingKeysValidity(/*hasType=*/true, /*isValid=*/false);
  }

  // CodingKeys must be an enum for synthesized conformance.
  auto *codingKeysEnum = dyn_cast<EnumDecl>(codingKeysTypeDecl);
  if (!codingKeysEnum) {
    tc.diagnose(codingKeysTypeDecl->getLoc(),
                diag::codable_codingkeys_type_is_not_an_enum_here,
                proto->getDeclaredType());
    return CodingKeysValidity(/*hasType=*/true, /*isValid=*/false);
  }

  bool valid = validateCodingKeysEnum(tc, codingKeysEnum, target, proto);
  return CodingKeysValidity(/*hasType=*/true, /*isValid=*/valid);
}

/// Synthesizes a new \c CodingKeys enum based on the {En,De}codable members of
/// the given type (\c nullptr if unable to synthesize).
///
/// If able to synthesize the enum, adds it directly to \c type.
///
/// \param tc The typechecker to use in validating {En,De}codable conformance.
///
/// \param target The nominal type decl whose nested \c CodingKeys type to
/// synthesize.
///
/// \param proto The {En,De}codable protocol to validate all the keys conform
/// to.
static EnumDecl *synthesizeCodingKeysEnum(TypeChecker &tc,
                                          NominalTypeDecl *target,
                                          ProtocolDecl *proto) {
  auto &C = tc.Context;

  // We want to look through all the var declarations of this type to create
  // enum cases based on those var names.
  auto *codingKeyProto = C.getProtocol(KnownProtocolKind::CodingKey);
  auto *codingKeyType = codingKeyProto->getDeclaredType();
  TypeLoc protoTypeLoc[1] = {TypeLoc::withoutLoc(codingKeyType)};
  MutableArrayRef<TypeLoc> inherited = C.AllocateCopy(protoTypeLoc);

  auto *enumDecl = new (C) EnumDecl(SourceLoc(), C.Id_CodingKeys, SourceLoc(),
                                    inherited, nullptr, target);
  enumDecl->setImplicit();
  enumDecl->setAccess(AccessLevel::Private);

  // For classes which inherit from something Encodable or Decodable, we
  // provide case `super` as the first key (to be used in encoding super).
  auto *classDecl = dyn_cast<ClassDecl>(target);
  if (classDecl &&
      (superclassIsEncodable(classDecl) || superclassIsDecodable(classDecl))) {
    // TODO: Ensure the class doesn't already have or inherit a variable named
    // "`super`"; otherwise we will generate an invalid enum. In that case,
    // diagnose and bail.
    auto *super = new (C) EnumElementDecl(SourceLoc(), C.Id_super, nullptr,
                                          SourceLoc(), nullptr, enumDecl);
    super->setImplicit();
    enumDecl->addMember(super);
  }

  // Each of these vars needs a case in the enum. For each var decl, if the type
  // conforms to {En,De}codable, add it to the enum.
  bool allConform = true;
  for (auto *varDecl : target->getStoredProperties(/*skipInaccessible=*/true)) {
    if (varDecl->getAttrs().hasAttribute<LazyAttr>())
      continue;

    auto conformance = varConformsToCodable(tc, target->getDeclContext(),
                                            varDecl, proto);
    switch (conformance) {
      case Conforms:
      {
        auto *elt = new (C) EnumElementDecl(SourceLoc(), varDecl->getName(),
                                            nullptr, SourceLoc(), nullptr,
                                            enumDecl);
        elt->setImplicit();
        enumDecl->addMember(elt);
        break;
      }

      case DoesNotConform:
        tc.diagnose(varDecl->getLoc(),
                    diag::codable_non_conforming_property_here,
                    proto->getDeclaredType(), varDecl->getType());
        LLVM_FALLTHROUGH;

      case TypeNotValidated:
        // We don't produce a diagnostic for a type which failed to validate.
        // This will produce a diagnostic elsewhere anyway.
        allConform = false;
        continue;
    }
  }

  if (!allConform)
    return nullptr;

  // Forcibly derive conformance to CodingKey.
  tc.checkConformancesInContext(enumDecl, enumDecl);

  // Add to the type.
  target->addMember(enumDecl);
  return enumDecl;
}

/// Fetches the \c CodingKeys enum nested in \c target, potentially reaching
/// through a typealias if the "CodingKeys" entity is a typealias.
///
/// This is only useful once a \c CodingKeys enum has been validated (via \c
/// hasValidCodingKeysEnum) or synthesized (via \c synthesizeCodingKeysEnum).
///
/// \param C The \c ASTContext to perform the lookup in.
///
/// \param target The target type to look in.
///
/// \return A retrieved canonical \c CodingKeys enum if \c target has a valid
/// one; \c nullptr otherwise.
static EnumDecl *lookupEvaluatedCodingKeysEnum(ASTContext &C,
                                               NominalTypeDecl *target) {
  auto codingKeyDecls = target->lookupDirect(DeclName(C.Id_CodingKeys));
  if (codingKeyDecls.empty())
    return nullptr;

  auto *codingKeysDecl = codingKeyDecls.front();
  if (auto *typealiasDecl = dyn_cast<TypeAliasDecl>(codingKeysDecl))
    codingKeysDecl = typealiasDecl->getDeclaredInterfaceType()->getAnyNominal();

  return dyn_cast<EnumDecl>(codingKeysDecl);
}

/// Creates a new var decl representing
///
///   var/let container : containerBase<keyType>
///
/// \c containerBase is the name of the type to use as the base (either
/// \c KeyedEncodingContainer or \c KeyedDecodingContainer).
///
/// \param C The AST context to create the decl in.
///
/// \param DC The \c DeclContext to create the decl in.
///
/// \param keyedContainerDecl The generic type to bind the key type in.
///
/// \param keyType The key type to bind to the container type.
///
/// \param spec Whether to declare the variable as immutable.
static VarDecl *createKeyedContainer(ASTContext &C, DeclContext *DC,
                                     NominalTypeDecl *keyedContainerDecl,
                                     Type keyType, VarDecl::Specifier spec) {
  // Bind Keyed*Container to Keyed*Container<KeyType>
  Type boundType[1] = {keyType};
  auto containerType = BoundGenericType::get(keyedContainerDecl, Type(),
                                             C.AllocateCopy(boundType));

  // let container : Keyed*Container<KeyType>
  auto *containerDecl = new (C) VarDecl(/*IsStatic=*/false, spec,
                                        /*IsCaptureList=*/false, SourceLoc(),
                                        C.Id_container, containerType, DC);
  containerDecl->setImplicit();
  containerDecl->setInterfaceType(containerType);
  return containerDecl;
}

/// Creates a new \c CallExpr representing
///
///   base.container(keyedBy: CodingKeys.self)
///
/// \param C The AST context to create the expression in.
///
/// \param DC The \c DeclContext to create any decls in.
///
/// \param base The base expression to make the call on.
///
/// \param returnType The return type of the call.
///
/// \param param The parameter to the call.
static CallExpr *createContainerKeyedByCall(ASTContext &C, DeclContext *DC,
                                            Expr *base, Type returnType,
                                            NominalTypeDecl *param) {
  // (keyedBy:)
  auto *keyedByDecl = new (C)
      ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                C.Id_keyedBy, SourceLoc(), C.Id_keyedBy, returnType, DC);
  keyedByDecl->setImplicit();
  keyedByDecl->setInterfaceType(returnType);

  // container(keyedBy:) method name
  auto *paramList = ParameterList::createWithoutLoc(keyedByDecl);
  DeclName callName(C, C.Id_container, paramList);

  // base.container(keyedBy:) expr
  auto *unboundCall = new (C) UnresolvedDotExpr(base, SourceLoc(), callName,
                                                DeclNameLoc(),
                                                /*Implicit=*/true);

  // CodingKeys.self expr
  auto *codingKeysExpr = TypeExpr::createForDecl(SourceLoc(),
                                                 param,
                                                 param->getDeclContext(),
                                                 /*Implicit=*/true);
  auto *codingKeysMetaTypeExpr = new (C) DotSelfExpr(codingKeysExpr,
                                                     SourceLoc(), SourceLoc());

  // Full bound base.container(keyedBy: CodingKeys.self) call
  Expr *args[1] = {codingKeysMetaTypeExpr};
  Identifier argLabels[1] = {C.Id_keyedBy};
  return CallExpr::createImplicit(C, unboundCall, C.AllocateCopy(args),
                                  C.AllocateCopy(argLabels));
}

/// Synthesizes the body for `func encode(to encoder: Encoder) throws`.
///
/// \param encodeDecl The function decl whose body to synthesize.
static void deriveBodyEncodable_encode(AbstractFunctionDecl *encodeDecl) {
  // struct Foo : Codable {
  //   var x: Int
  //   var y: String
  //
  //   // Already derived by this point if possible.
  //   @derived enum CodingKeys : CodingKey {
  //     case x
  //     case y
  //   }
  //
  //   @derived func encode(to encoder: Encoder) throws {
  //     var container = encoder.container(keyedBy: CodingKeys.self)
  //     try container.encode(x, forKey: .x)
  //     try container.encode(y, forKey: .y)
  //   }
  // }

  // The enclosing type decl.
  auto *targetDecl = cast<NominalTypeDecl>(encodeDecl->getDeclContext());

  auto *funcDC = cast<DeclContext>(encodeDecl);
  auto &C = funcDC->getASTContext();

  // We'll want the CodingKeys enum for this type, potentially looking through
  // a typealias.
  auto *codingKeysEnum = lookupEvaluatedCodingKeysEnum(C, targetDecl);
  // We should have bailed already if:
  // a) The type does not have CodingKeys
  // b) The type is not an enum
  assert(codingKeysEnum && "Missing CodingKeys decl.");

  SmallVector<ASTNode, 5> statements;

  // Generate a reference to containerExpr ahead of time in case there are no
  // properties to encode or decode, but the type is a class which inherits from
  // something Codable and needs to encode super.

  // let container : KeyedEncodingContainer<CodingKeys>
  auto codingKeysType = codingKeysEnum->getDeclaredType();
  auto *containerDecl = createKeyedContainer(C, funcDC,
                                             C.getKeyedEncodingContainerDecl(),
                                             codingKeysType,
                                             VarDecl::Specifier::Var);

  auto *containerExpr = new (C) DeclRefExpr(ConcreteDeclRef(containerDecl),
                                            DeclNameLoc(), /*Implicit=*/true,
                                            AccessSemantics::DirectToStorage);

  // Need to generate
  //   `let container = encoder.container(keyedBy: CodingKeys.self)`
  // This is unconditional because a type with no properties should encode as an
  // empty container.
  //
  // `let container` (containerExpr) is generated above.

  // encoder
  auto encoderParam = encodeDecl->getParameterList(1)->get(0);
  auto *encoderExpr = new (C) DeclRefExpr(ConcreteDeclRef(encoderParam),
                                          DeclNameLoc(), /*Implicit=*/true);

  // Bound encoder.container(keyedBy: CodingKeys.self) call
  auto containerType = containerDecl->getInterfaceType();
  auto *callExpr = createContainerKeyedByCall(C, funcDC, encoderExpr,
                                              containerType, codingKeysEnum);

  // Full `let container = encoder.container(keyedBy: CodingKeys.self)`
  // binding.
  auto *containerPattern = new (C) NamedPattern(containerDecl,
                                                /*implicit=*/true);
  auto *bindingDecl = PatternBindingDecl::create(C, SourceLoc(),
                                                 StaticSpellingKind::None,
                                                 SourceLoc(),
                                                 containerPattern, callExpr,
                                                 funcDC);
  statements.push_back(bindingDecl);
  statements.push_back(containerDecl);

  // Now need to generate `try container.encode(x, forKey: .x)` for all
  // existing properties. Optional properties get `encodeIfPresent`.
  for (auto *elt : codingKeysEnum->getAllElements()) {
    VarDecl *varDecl = nullptr;
    for (auto decl : targetDecl->lookupDirect(DeclName(elt->getName()))) {
      if (auto *vd = dyn_cast<VarDecl>(decl)) {
        if (!vd->isStatic()) {
          varDecl = vd;
          break;
        }
      }
    }
    assert(varDecl && "Should have found at least 1 var decl");

    // self.x
    auto *selfRef = createSelfDeclRef(encodeDecl);
    auto *varExpr = new (C) MemberRefExpr(selfRef, SourceLoc(),
                                          ConcreteDeclRef(varDecl),
                                          DeclNameLoc(), /*Implicit=*/true);

    // CodingKeys.x
    auto *eltRef = new (C) DeclRefExpr(elt, DeclNameLoc(), /*implicit=*/true);
    auto *metaTyRef = TypeExpr::createImplicit(codingKeysType, C);
    auto *keyExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);

    // encode(_:forKey:)/encodeIfPresent(_:forKey:)
    auto methodName = C.Id_encode;
    auto varType = varDecl->getType();
    if (auto referenceType = varType->getAs<ReferenceStorageType>()) {
      // This is a weak/unowned/unmanaged var. Get the inner type before
      // checking optionality.
      varType = referenceType->getReferentType();
    }

    if (varType->getAnyNominal() == C.getOptionalDecl())
      methodName = C.Id_encodeIfPresent;

    SmallVector<Identifier, 2> argNames{Identifier(), C.Id_forKey};
    DeclName name(C, methodName, argNames);
    auto *encodeCall = new (C) UnresolvedDotExpr(containerExpr, SourceLoc(),
                                                 name, DeclNameLoc(),
                                                 /*Implicit=*/true);

    // container.encode(self.x, forKey: CodingKeys.x)
    Expr *args[2] = {varExpr, keyExpr};
    auto *callExpr = CallExpr::createImplicit(C, encodeCall,
                                              C.AllocateCopy(args),
                                              C.AllocateCopy(argNames));

    // try container.encode(self.x, forKey: CodingKeys.x)
    auto *tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                    /*Implicit=*/true);
    statements.push_back(tryExpr);
  }

  // Classes which inherit from something Codable should encode super as well.
  auto *classDecl = dyn_cast<ClassDecl>(targetDecl);
  if (classDecl && superclassIsEncodable(classDecl)) {
    // Need to generate `try super.encode(to: container.superEncoder())`

    // superEncoder()
    auto *method = new (C) UnresolvedDeclRefExpr(DeclName(C.Id_superEncoder),
                                                 DeclRefKind::Ordinary,
                                                 DeclNameLoc());

    // container.superEncoder()
    auto *superEncoderRef = new (C) DotSyntaxCallExpr(containerExpr,
                                                      SourceLoc(), method);

    // encode(to:) expr
    auto *encodeDeclRef = new (C) DeclRefExpr(ConcreteDeclRef(encodeDecl),
                                              DeclNameLoc(), /*Implicit=*/true);

    // super
    auto *superRef = new (C) SuperRefExpr(encodeDecl->getImplicitSelfDecl(),
                                          SourceLoc(), /*Implicit=*/true);

    // super.encode(to:)
    auto *encodeCall = new (C) DotSyntaxCallExpr(superRef, SourceLoc(),
                                                 encodeDeclRef);

    // super.encode(to: container.superEncoder())
    Expr *args[1] = {superEncoderRef};
    Identifier argLabels[1] = {C.Id_to};
    auto *callExpr = CallExpr::createImplicit(C, encodeCall,
                                              C.AllocateCopy(args),
                                              C.AllocateCopy(argLabels));

    // try super.encode(to: container.superEncoder())
    auto *tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                    /*Implicit=*/true);
    statements.push_back(tryExpr);
  }

  auto *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
                                 /*implicit=*/true);
  encodeDecl->setBody(body);
}

/// Synthesizes a function declaration for `encode(to: Encoder) throws` with a
/// lazily synthesized body for the given type.
///
/// Adds the function declaration to the given type before returning it.
///
/// \param tc The type checker whose AST context to synthesize the decl in.
///
/// \param parentDecl The parent declaration of the type.
///
/// \param target The nominal type to synthesize the function for.
static FuncDecl *deriveEncodable_encode(TypeChecker &tc, Decl *parentDecl,
                                        NominalTypeDecl *target) {
  auto &C = tc.Context;

  // Expected type: (Self) -> (Encoder) throws -> ()
  // Constructed as: func type
  //                 input: Self
  //                 throws
  //                 output: function type
  //                         input: Encoder
  //                         output: ()
  // Create from the inside out:

  // (to: Encoder)
  auto encoderType = C.getEncoderDecl()->getDeclaredInterfaceType();
  auto inputTypeElt = TupleTypeElt(encoderType, C.Id_to);
  auto inputType = TupleType::get(ArrayRef<TupleTypeElt>(inputTypeElt), C);

  // throws
  auto extInfo = FunctionType::ExtInfo(FunctionTypeRepresentation::Swift,
                                       /*Throws=*/true);
  // ()
  auto returnType = TupleType::getEmpty(C);

  // (to: Encoder) throws -> ()
  auto innerType = FunctionType::get(inputType, returnType, extInfo);

  // Params: (self [implicit], Encoder)
  auto *selfDecl = ParamDecl::createSelf(SourceLoc(), target);
  auto *encoderParam = new (C)
      ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(), C.Id_to,
                SourceLoc(), C.Id_encoder, encoderType, target);
  encoderParam->setInterfaceType(encoderType);

  ParameterList *params[] = {ParameterList::createWithoutLoc(selfDecl),
                             ParameterList::createWithoutLoc(encoderParam)};

  // Func name: encode(to: Encoder)
  DeclName name(C, C.Id_encode, params[1]);
  auto *encodeDecl = FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None,
                                      SourceLoc(), name, SourceLoc(),
                                      /*Throws=*/true, SourceLoc(),
                                      nullptr, params,
                                      TypeLoc::withoutLoc(returnType),
                                      target);
  encodeDecl->setImplicit();
  encodeDecl->setSynthesized();
  encodeDecl->setBodySynthesizer(deriveBodyEncodable_encode);

  // This method should be marked as 'override' for classes inheriting Encodable
  // conformance from a parent class.
  auto *classDecl = dyn_cast<ClassDecl>(target);
  if (classDecl && superclassIsEncodable(classDecl)) {
    auto *attr = new (C) OverrideAttr(/*IsImplicit=*/true);
    encodeDecl->getAttrs().add(attr);
  }

  // Evaluate the type of Self in (Self) -> (Encoder) throws -> ().
  Type selfType = target->getDeclaredInterfaceType();
  Type interfaceType;
  if (auto sig = target->getGenericSignatureOfContext()) {
    // Evaluate the below, but in a generic environment (if Self is generic).
    encodeDecl->setGenericEnvironment(target->getGenericEnvironmentOfContext());
    interfaceType = GenericFunctionType::get(sig, selfType, innerType,
                                             FunctionType::ExtInfo());
  } else {
    // (Self) -> innerType == (Encoder) throws -> ()
    interfaceType = FunctionType::get(selfType, innerType);
  }

  encodeDecl->setInterfaceType(interfaceType);
  encodeDecl->setValidationStarted();
  encodeDecl->copyFormalAccessFrom(target, /*sourceIsParentContext*/true);

  tc.Context.addSynthesizedDecl(encodeDecl);

  target->addMember(encodeDecl);
  return encodeDecl;
}

/// Synthesizes the body for `init(from decoder: Decoder) throws`.
///
/// \param initDecl The function decl whose body to synthesize.
static void deriveBodyDecodable_init(AbstractFunctionDecl *initDecl) {
  // struct Foo : Codable {
  //   var x: Int
  //   var y: String
  //
  //   // Already derived by this point if possible.
  //   @derived enum CodingKeys : CodingKey {
  //     case x
  //     case y
  //   }
  //
  //   @derived init(from decoder: Decoder) throws {
  //     let container = try decoder.container(keyedBy: CodingKeys.self)
  //     x = try container.decode(Type.self, forKey: .x)
  //     y = try container.decode(Type.self, forKey: .y)
  //   }
  // }

  // The enclosing type decl.
  auto *targetDecl = cast<NominalTypeDecl>(initDecl->getDeclContext());

  auto *funcDC = cast<DeclContext>(initDecl);
  auto &C = funcDC->getASTContext();

  // We'll want the CodingKeys enum for this type, potentially looking through
  // a typealias.
  auto *codingKeysEnum = lookupEvaluatedCodingKeysEnum(C, targetDecl);
  // We should have bailed already if:
  // a) The type does not have CodingKeys
  // b) The type is not an enum
  assert(codingKeysEnum && "Missing CodingKeys decl.");

  // Generate a reference to containerExpr ahead of time in case there are no
  // properties to encode or decode, but the type is a class which inherits from
  // something Codable and needs to decode super.

  // let container : KeyedDecodingContainer<CodingKeys>
  auto codingKeysType = codingKeysEnum->getDeclaredType();
  auto *containerDecl = createKeyedContainer(C, funcDC,
                                             C.getKeyedDecodingContainerDecl(),
                                             codingKeysType,
                                             VarDecl::Specifier::Let);

  auto *containerExpr = new (C) DeclRefExpr(ConcreteDeclRef(containerDecl),
                                            DeclNameLoc(), /*Implicit=*/true,
                                            AccessSemantics::DirectToStorage);

  SmallVector<ASTNode, 5> statements;
  auto enumElements = codingKeysEnum->getAllElements();
  if (!enumElements.empty()) {
    // Need to generate
    //   `let container = try decoder.container(keyedBy: CodingKeys.self)`
    // `let container` (containerExpr) is generated above.

    // decoder
    auto decoderParam = initDecl->getParameterList(1)->get(0);
    auto *decoderExpr = new (C) DeclRefExpr(ConcreteDeclRef(decoderParam),
                                            DeclNameLoc(), /*Implicit=*/true);

    // Bound decoder.container(keyedBy: CodingKeys.self) call
    auto containerType = containerDecl->getInterfaceType();
    auto *callExpr = createContainerKeyedByCall(C, funcDC, decoderExpr,
                                                containerType, codingKeysEnum);

    // try decoder.container(keyedBy: CodingKeys.self)
    auto *tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                    /*implicit=*/true);

    // Full `let container = decoder.container(keyedBy: CodingKeys.self)`
    // binding.
    auto *containerPattern = new (C) NamedPattern(containerDecl,
                                                  /*implicit=*/true);
    auto *bindingDecl = PatternBindingDecl::create(C, SourceLoc(),
                                                   StaticSpellingKind::None,
                                                   SourceLoc(),
                                                   containerPattern, tryExpr,
                                                   funcDC);
    statements.push_back(bindingDecl);
    statements.push_back(containerDecl);

    // Now need to generate `x = try container.decode(Type.self, forKey: .x)`
    // for all existing properties. Optional properties get `decodeIfPresent`.
    for (auto *elt : enumElements) {
      VarDecl *varDecl;
      for (auto decl : targetDecl->lookupDirect(DeclName(elt->getName())))
        if ((varDecl = dyn_cast<VarDecl>(decl)))
          break;

      // Don't output a decode statement for a var let with a default value.
      if (varDecl->isLet() && varDecl->getParentInitializer() != nullptr)
        continue;

      // Potentially unwrap a layer of optionality from the var type. If the var
      // is Optional<T>, we want to decodeIfPresent(T.self, forKey: ...);
      // otherwise, we can just decode(T.self, forKey: ...).
      // This is also true if the type is an ImplicitlyUnwrappedOptional.
      auto varType = varDecl->getType();
      auto methodName = C.Id_decode;
      if (auto referenceType = varType->getAs<ReferenceStorageType>()) {
        // This is a weak/unowned/unmanaged var. Get the inner type before
        // checking optionality.
        varType = referenceType->getReferentType();
      }

      if (varType->getAnyNominal() == C.getOptionalDecl()) {
        methodName = C.Id_decodeIfPresent;

        // The type we request out of decodeIfPresent needs to be unwrapped
        // one level.
        // e.g. String? => decodeIfPresent(String.self, forKey: ...), not
        //                 decodeIfPresent(String?.self, forKey: ...)
        auto boundOptionalType =
          dyn_cast<BoundGenericType>(varType->getCanonicalType());
        varType = boundOptionalType->getGenericArgs()[0];
      }

      // Type.self (where Type === type(of: x))
      // Calculating the metatype needs to happen after potential Optional
      // unwrapping above.
      auto *metaTyRef = TypeExpr::createImplicit(varType, C);
      auto *targetExpr = new (C) DotSelfExpr(metaTyRef, SourceLoc(),
                                             SourceLoc(), varType);

      // CodingKeys.x
      auto *eltRef = new (C) DeclRefExpr(elt, DeclNameLoc(), /*implicit=*/true);
      metaTyRef = TypeExpr::createImplicit(codingKeysType, C);
      auto *keyExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);

      // decode(_:forKey:)/decodeIfPresent(_:forKey:)
      SmallVector<Identifier, 2> argNames{Identifier(), C.Id_forKey};
      DeclName name(C, methodName, argNames);
      auto *decodeCall = new (C) UnresolvedDotExpr(containerExpr, SourceLoc(),
                                                   name, DeclNameLoc(),
                                                   /*Implicit=*/true);

      // container.decode(Type.self, forKey: CodingKeys.x)
      Expr *args[2] = {targetExpr, keyExpr};
      auto *callExpr = CallExpr::createImplicit(C, decodeCall,
                                                C.AllocateCopy(args),
                                                C.AllocateCopy(argNames));

      // try container.decode(Type.self, forKey: CodingKeys.x)
      auto *tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                      /*Implicit=*/true);

      auto *selfRef = createSelfDeclRef(initDecl);
      auto *varExpr = new (C) UnresolvedDotExpr(selfRef, SourceLoc(),
                                                DeclName(varDecl->getName()),
                                                DeclNameLoc(),
                                                /*implicit=*/true);
      auto *assignExpr = new (C) AssignExpr(varExpr, SourceLoc(), tryExpr,
                                            /*Implicit=*/true);
      statements.push_back(assignExpr);
    }
  }

  // Classes which have a superclass must call super.init(from:) if the
  // superclass is Decodable, or super.init() if it is not.
  if (auto *classDecl = dyn_cast<ClassDecl>(targetDecl)) {
    if (auto *superclassDecl = classDecl->getSuperclassDecl()) {
      if (superclassIsDecodable(classDecl)) {
        // Need to generate `try super.init(from: container.superDecoder())`

        // container.superDecoder
        auto *superDecoderRef =
          new (C) UnresolvedDotExpr(containerExpr, SourceLoc(),
                                    DeclName(C.Id_superDecoder),
                                    DeclNameLoc(), /*Implicit=*/true);

        // container.superDecoder()
        auto *superDecoderCall =
          CallExpr::createImplicit(C, superDecoderRef, ArrayRef<Expr *>(),
                                   ArrayRef<Identifier>());

        // super
        auto *superRef = new (C) SuperRefExpr(initDecl->getImplicitSelfDecl(),
                                              SourceLoc(), /*Implicit=*/true);

        // super.init(from:)
        auto initName = DeclName(C, DeclBaseName::createConstructor(), C.Id_from);
        auto *initCall = new (C) UnresolvedDotExpr(superRef, SourceLoc(),
                                                   initName, DeclNameLoc(),
                                                   /*Implicit=*/true);

        // super.decode(from: container.superDecoder())
        Expr *args[1] = {superDecoderCall};
        Identifier argLabels[1] = {C.Id_from};
        auto *callExpr = CallExpr::createImplicit(C, initCall,
                                                  C.AllocateCopy(args),
                                                  C.AllocateCopy(argLabels));

        // try super.init(from: container.superDecoder())
        auto *tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                        /*Implicit=*/true);
        statements.push_back(tryExpr);
      } else {
        // The explicit constructor name is a compound name taking no arguments.
        DeclName initName(C, DeclBaseName::createConstructor(), ArrayRef<Identifier>());

        // We need to look this up in the superclass to see if it throws.
        auto result = superclassDecl->lookupDirect(initName);

        // We should have bailed one level up if this were not available.
        assert(!result.empty());

        // If the init is failable, we should have already bailed one level
        // above.
        ConstructorDecl *superInitDecl = cast<ConstructorDecl>(result.front());
        assert(superInitDecl->getFailability() == OTK_None);

        // super
        auto *superRef = new (C) SuperRefExpr(initDecl->getImplicitSelfDecl(),
                                              SourceLoc(), /*Implicit=*/true);

        // super.init()
        auto *superInitRef = new (C) UnresolvedDotExpr(superRef, SourceLoc(),
                                                       initName, DeclNameLoc(),
                                                       /*Implicit=*/true);
        // super.init() call
        Expr *callExpr = CallExpr::createImplicit(C, superInitRef,
                                                  ArrayRef<Expr *>(),
                                                  ArrayRef<Identifier>());

        // If super.init throws, try super.init()
        if (superInitDecl->hasThrows())
          callExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                     /*Implicit=*/true);

        statements.push_back(callExpr);
      }
    }
  }

  auto *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
                                 /*implicit=*/true);
  initDecl->setBody(body);
}

/// Synthesizes a function declaration for `init(from: Decoder) throws` with a
/// lazily synthesized body for the given type.
///
/// Adds the function declaration to the given type before returning it.
///
/// \param tc The type checker whose AST context to synthesize the decl in.
///
/// \param parentDecl The parent declaration of the type.
///
/// \param target The nominal type to synthesize the function for.
static ValueDecl *deriveDecodable_init(TypeChecker &tc, Decl *parentDecl,
                                       NominalTypeDecl *target) {
  auto &C = tc.Context;

  // Expected type: (Self) -> (Decoder) throws -> (Self)
  // Constructed as: func type
  //                 input: Self
  //                 throws
  //                 output: function type
  //                         input: Encoder
  //                         output: Self
  // Compute from the inside out:

  // (from: Decoder)
  auto decoderType = C.getDecoderDecl()->getDeclaredInterfaceType();
  auto inputTypeElt = TupleTypeElt(decoderType, C.Id_from);
  auto inputType = TupleType::get(ArrayRef<TupleTypeElt>(inputTypeElt), C);

  // throws
  auto extInfo = FunctionType::ExtInfo(FunctionTypeRepresentation::Swift,
                                       /*Throws=*/true);

  // (Self)
  auto returnType = target->getDeclaredInterfaceType();

  // (from: Decoder) throws -> (Self)
  Type innerType = FunctionType::get(inputType, returnType, extInfo);

  // Params: (self [implicit], Decoder)
  // self should be inout if the type is a value type; not inout otherwise.
  auto inOut = !isa<ClassDecl>(target);
  auto *selfDecl = ParamDecl::createSelf(SourceLoc(), target,
                                         /*isStatic=*/false,
                                         /*isInOut=*/inOut);
  auto *decoderParamDecl = new (C)
      ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                C.Id_from, SourceLoc(), C.Id_decoder, decoderType, target);
  decoderParamDecl->setImplicit();
  decoderParamDecl->setInterfaceType(decoderType);

  auto *paramList = ParameterList::createWithoutLoc(decoderParamDecl);

  // Func name: init(from: Decoder)
  DeclName name(C, DeclBaseName::createConstructor(), paramList);

  auto *initDecl = new (C) ConstructorDecl(name, SourceLoc(), OTK_None,
                                           SourceLoc(), /*Throws=*/true,
                                           SourceLoc(), selfDecl, paramList,
                                           /*GenericParams=*/nullptr, target);
  initDecl->setImplicit();
  initDecl->setSynthesized();
  initDecl->setBodySynthesizer(deriveBodyDecodable_init);

  // This constructor should be marked as `required` for non-final classes.
  if (isa<ClassDecl>(target) && !target->getAttrs().hasAttribute<FinalAttr>()) {
    auto *reqAttr = new (C) RequiredAttr(/*IsImplicit=*/true);
    initDecl->getAttrs().add(reqAttr);
  }

  auto selfParam = computeSelfParam(initDecl);
  auto initSelfParam = computeSelfParam(initDecl, /*init=*/true);
  Type interfaceType;
  Type initializerType;
  if (auto sig = target->getGenericSignatureOfContext()) {
    // Evaluate the below, but in a generic environment (if Self is generic).
    initDecl->setGenericEnvironment(target->getGenericEnvironmentOfContext());
    interfaceType = GenericFunctionType::get(sig, {selfParam}, innerType,
                                             FunctionType::ExtInfo());
    initializerType = GenericFunctionType::get(sig, {initSelfParam}, innerType,
                                               FunctionType::ExtInfo());
  } else {
    // (Self) -> (Decoder) throws -> (Self)
    interfaceType = FunctionType::get({selfParam}, innerType,
                                      FunctionType::ExtInfo());
    initializerType = FunctionType::get({initSelfParam}, innerType,
                                        FunctionType::ExtInfo());
  }

  initDecl->setInterfaceType(interfaceType);
  initDecl->setValidationStarted();
  initDecl->setInitializerInterfaceType(initializerType);
  initDecl->copyFormalAccessFrom(target, /*sourceIsParentContext*/true);

  tc.Context.addSynthesizedDecl(initDecl);

  target->addMember(initDecl);
  return initDecl;
}

/// Returns whether the given type is valid for synthesizing {En,De}codable.
///
/// Checks to see whether the given type has a valid \c CodingKeys enum, and if
/// not, attempts to synthesize one for it.
///
/// \param tc The typechecker to use in validating {En,Decodable} conformance.
///
/// \param target The type to validate.
///
/// \param requirement The requirement we want to synthesize.
///
/// \param proto The *codable protocol to check for validity.
static bool canSynthesize(TypeChecker &tc, NominalTypeDecl *target,
                          ValueDecl *requirement, ProtocolDecl *proto) {
  // Before we attempt to look up (or more importantly, synthesize) a CodingKeys
  // entity on target, we need to make sure the type is otherwise valid.
  //
  // If we are synthesizing Decodable and the target is a class with a
  // superclass, our synthesized init(from:) will need to call either
  // super.init(from:) or super.init() depending on whether the superclass is
  // Decodable itself.
  //
  // If the required initializer is not available, we shouldn't attempt to
  // synthesize CodingKeys.
  ASTContext &C = tc.Context;
  auto *classDecl = dyn_cast<ClassDecl>(target);
  if (proto->isSpecificProtocol(KnownProtocolKind::Decodable) && classDecl) {
    if (auto *superclassDecl = classDecl->getSuperclassDecl()) {
      DeclName memberName;
      auto superType = superclassDecl->getDeclaredInterfaceType();
      if (tc.conformsToProtocol(superType, proto, superclassDecl,
                                ConformanceCheckFlags::Used)) {
        // super.init(from:) must be accessible.
        memberName = cast<ConstructorDecl>(requirement)->getFullName();
      } else {
        // super.init() must be accessible.
        // Passing an empty params array constructs a compound name with no
        // arguments (as opposed to a simple name when omitted).
        memberName = DeclName(C, DeclBaseName::createConstructor(),
                              ArrayRef<Identifier>());
      }

      auto result = tc.lookupMember(superclassDecl, superType, memberName);

      if (result.empty()) {
        // No super initializer for us to call.
        tc.diagnose(superclassDecl, diag::decodable_no_super_init_here,
                    requirement->getFullName(), memberName);
        return false;
      } else if (result.size() > 1) {
        // There are multiple results for this lookup. We'll end up producing a
        // diagnostic later complaining about duplicate methods (if we haven't
        // already), so just bail with a general error.
        return false;
      } else {
        auto *initializer =
          cast<ConstructorDecl>(result.front().getValueDecl());
        if (!initializer->isDesignatedInit()) {
          // We must call a superclass's designated initializer.
          tc.diagnose(initializer,
                      diag::decodable_super_init_not_designated_here,
                      requirement->getFullName(), memberName);
          return false;
        } else if (!initializer->isAccessibleFrom(target)) {
          // Cannot call an inaccessible method.
          auto accessScope = initializer->getFormalAccessScope(target);
          tc.diagnose(initializer, diag::decodable_inaccessible_super_init_here,
                      requirement->getFullName(), memberName,
                      accessScope.accessLevelForDiagnostics());
          return false;
        } else if (initializer->getFailability() != OTK_None) {
          // We can't call super.init() if it's failable, since init(from:)
          // isn't failable.
          tc.diagnose(initializer, diag::decodable_super_init_is_failable_here,
                      requirement->getFullName(), memberName);
          return false;
        }
      }
    }
  }

  // If the target already has a valid CodingKeys enum, we won't need to
  // synthesize one.
  auto validity = hasValidCodingKeysEnum(tc, target, proto);

  // We found a type, but it wasn't valid.
  if (!validity.isValid)
    return false;

  // We can try to synthesize a type here.
  if (!validity.hasType) {
    auto *synthesizedEnum = synthesizeCodingKeysEnum(tc, target, proto);
    if (!synthesizedEnum)
      return false;
  }

  return true;
}

ValueDecl *DerivedConformance::deriveEncodable(TypeChecker &tc,
                                               Decl *parentDecl,
                                               NominalTypeDecl *target,
                                               ValueDecl *requirement) {
  // We can only synthesize Encodable for structs and classes.
  if (!isa<StructDecl>(target) && !isa<ClassDecl>(target))
    return nullptr;

  if (requirement->getBaseName() != tc.Context.Id_encode) {
    // Unknown requirement.
    tc.diagnose(requirement->getLoc(), diag::broken_encodable_requirement);
    return nullptr;
  }

  // Conformance can't be synthesized in an extension.
  auto encodableProto = tc.Context.getProtocol(KnownProtocolKind::Encodable);
  auto encodableType = encodableProto->getDeclaredType();
  if (target != parentDecl) {
    tc.diagnose(parentDecl->getLoc(), diag::cannot_synthesize_in_extension,
                encodableType);
    return nullptr;
  }

  // We're about to try to synthesize Encodable. If something goes wrong,
  // we'll have to output at least one error diagnostic because we returned
  // true from NominalTypeDecl::derivesProtocolConformance; if we don't, we're
  // expected to return a witness here later (and we crash on an assertion).
  // Producing a diagnostic stops compilation before then.
  //
  // A synthesis attempt will produce NOTE diagnostics throughout, but we'll
  // want to collect them before displaying -- we want NOTEs to display
  // _after_ a main diagnostic so we don't get a NOTE before the error it
  // relates to.
  //
  // We can do this with a diagnostic transaction -- first collect failure
  // diagnostics, then potentially collect notes. If we succeed in
  // synthesizing Encodable, we can cancel the transaction and get rid of the
  // fake failures.
  auto diagnosticTransaction = DiagnosticTransaction(tc.Context.Diags);
  tc.diagnose(target, diag::type_does_not_conform, target->getDeclaredType(),
              encodableType);
  tc.diagnose(requirement, diag::no_witnesses, diag::RequirementKind::Func,
              requirement->getFullName(), encodableType, /*AddFixIt=*/false);

  // Check other preconditions for synthesized conformance.
  // This synthesizes a CodingKeys enum if possible.
  if (canSynthesize(tc, target, requirement, encodableProto)) {
    diagnosticTransaction.abort();
    return deriveEncodable_encode(tc, parentDecl, target);
  }

  return nullptr;
}

ValueDecl *DerivedConformance::deriveDecodable(TypeChecker &tc,
                                               Decl *parentDecl,
                                               NominalTypeDecl *target,
                                               ValueDecl *requirement) {
  // We can only synthesize Encodable for structs and classes.
  if (!isa<StructDecl>(target) && !isa<ClassDecl>(target))
    return nullptr;

  if (requirement->getBaseName() != DeclBaseName::createConstructor()) {
    // Unknown requirement.
    tc.diagnose(requirement->getLoc(), diag::broken_decodable_requirement);
    return nullptr;
  }

  // Conformance can't be synthesized in an extension.
  auto decodableProto = tc.Context.getProtocol(KnownProtocolKind::Decodable);
  auto decodableType = decodableProto->getDeclaredType();
  if (target != parentDecl) {
    tc.diagnose(parentDecl->getLoc(), diag::cannot_synthesize_in_extension,
                decodableType);
    return nullptr;
  }

  // We're about to try to synthesize Decodable. If something goes wrong,
  // we'll have to output at least one error diagnostic. We need to collate
  // diagnostics produced by canSynthesize and deriveDecodable_init to produce
  // them in the right order -- see the comment in deriveEncodable for
  // background on this transaction.
  auto diagnosticTransaction = DiagnosticTransaction(tc.Context.Diags);
  tc.diagnose(target, diag::type_does_not_conform, target->getDeclaredType(),
              decodableType);
  tc.diagnose(requirement, diag::no_witnesses,
              diag::RequirementKind::Constructor, requirement->getFullName(),
              decodableType, /*AddFixIt=*/false);

  // Check other preconditions for synthesized conformance.
  // This synthesizes a CodingKeys enum if possible.
  if (canSynthesize(tc, target, requirement, decodableProto)) {
    diagnosticTransaction.abort();
    return deriveDecodable_init(tc, parentDecl, target);
  }

  return nullptr;
}
