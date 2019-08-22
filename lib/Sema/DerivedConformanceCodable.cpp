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
/// \param context The \c DeclContext the var declarations belong to.
///
/// \param target The \c Type to validate.
///
/// \param proto The \c ProtocolDecl to check conformance to.
static CodableConformanceType typeConformsToCodable(DeclContext *context,
                                                    Type target, bool isIUO,
                                                    ProtocolDecl *proto) {
  target = context->mapTypeIntoContext(target);

  if (isIUO)
    return typeConformsToCodable(context, target->getOptionalObjectType(),
                                 false, proto);

  return (TypeChecker::conformsToProtocol(target, proto, context, None)
          ? Conforms
          : DoesNotConform);
}

/// Returns whether the given variable conforms to the given {En,De}codable
/// protocol.
///
/// \param tc The typechecker to use in validating {En,De}codable conformance.
///
/// \param context The \c DeclContext in which to check conformance.
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
  if (!varDecl->hasInterfaceType())
    tc.validateDecl(varDecl);

  // If the var decl didn't validate, it may still not have a type; confirm it
  // has a type before ensuring the type conforms to Codable.
  if (!varDecl->hasInterfaceType())
    return TypeNotValidated;

  bool isIUO = varDecl->isImplicitlyUnwrappedOptional();
  return typeConformsToCodable(context, varDecl->getValueInterfaceType(),
                               isIUO, proto);
}

/// Retrieve the variable name for the purposes of encoding/decoding.
static Identifier getVarNameForCoding(VarDecl *var) {
  if (auto originalVar = var->getOriginalWrappedProperty())
    return originalVar->getName();

  return var->getName();
}

/// Validates the given CodingKeys enum decl by ensuring its cases are a 1-to-1
/// match with the stored vars of the given type.
///
/// \param codingKeysDecl The \c CodingKeys enum decl to validate.
static bool validateCodingKeysEnum(DerivedConformance &derived,
                                   EnumDecl *codingKeysDecl) {
  auto &tc = derived.TC;
  auto conformanceDC = derived.getConformanceContext();

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
  for (auto *varDecl : derived.Nominal->getStoredProperties()) {
    if (!varDecl->isUserAccessible())
      continue;

    properties[getVarNameForCoding(varDecl)] = varDecl;
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
    auto conformance =
        varConformsToCodable(tc, conformanceDC, it->second, derived.Protocol);
    switch (conformance) {
      case Conforms:
        // The property was valid. Remove it from the list.
        properties.erase(it);
        break;

      case DoesNotConform:
        tc.diagnose(it->second->getLoc(),
                    diag::codable_non_conforming_property_here,
                    derived.getProtocolType(), it->second->getType());
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
      derived.Protocol->isSpecificProtocol(KnownProtocolKind::Decodable)) {
    for (auto it = properties.begin(); it != properties.end(); ++it) {
      // If the var is default initializable, then it need not have an explicit
      // initial value.
      auto *varDecl = it->second;
      if (auto pbd = varDecl->getParentPatternBinding()) {
        if (pbd->isDefaultInitializable())
          continue;
      }

      if (varDecl->isParentInitialized())
        continue;

      // The var was not default initializable, and did not have an explicit
      // initial value.
      propertiesAreValid = false;
      tc.diagnose(it->second->getLoc(), diag::codable_non_decoded_property_here,
                  derived.getProtocolType(), it->first);
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
/// \returns A \c CodingKeysValidity value representing the result of the check.
static CodingKeysValidity hasValidCodingKeysEnum(DerivedConformance &derived) {
  auto &tc = derived.TC;
  auto &C = tc.Context;
  auto codingKeysDecls =
      derived.Nominal->lookupDirect(DeclName(C.Id_CodingKeys));
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
                derived.getProtocolType());
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
  if (!TypeChecker::conformsToProtocol(codingKeysType, codingKeyProto,
                                       derived.getConformanceContext(),
                                       None)) {
    // If CodingKeys is a typealias which doesn't point to a valid nominal type,
    // codingKeysTypeDecl will be nullptr here. In that case, we need to warn on
    // the location of the usage, since there isn't an underlying type to
    // diagnose on.
    SourceLoc loc = codingKeysTypeDecl ?
                    codingKeysTypeDecl->getLoc() :
                    cast<TypeDecl>(result)->getLoc();

    tc.diagnose(loc, diag::codable_codingkeys_type_does_not_conform_here,
                derived.getProtocolType());

    return CodingKeysValidity(/*hasType=*/true, /*isValid=*/false);
  }

  // CodingKeys must be an enum for synthesized conformance.
  auto *codingKeysEnum = dyn_cast<EnumDecl>(codingKeysTypeDecl);
  if (!codingKeysEnum) {
    tc.diagnose(codingKeysTypeDecl->getLoc(),
                diag::codable_codingkeys_type_is_not_an_enum_here,
                derived.getProtocolType());
    return CodingKeysValidity(/*hasType=*/true, /*isValid=*/false);
  }

  bool valid = validateCodingKeysEnum(derived, codingKeysEnum);
  return CodingKeysValidity(/*hasType=*/true, /*isValid=*/valid);
}

/// Synthesizes a new \c CodingKeys enum based on the {En,De}codable members of
/// the given type (\c nullptr if unable to synthesize).
///
/// If able to synthesize the enum, adds it directly to \c derived.Nominal.
static EnumDecl *synthesizeCodingKeysEnum(DerivedConformance &derived) {
  auto &tc = derived.TC;
  auto &C = tc.Context;
  // Create CodingKeys in the parent type always, because both
  // Encodable and Decodable might want to use it, and they may have
  // different conditional bounds. CodingKeys is simple and can't
  // depend on those bounds.
  auto target = derived.Nominal;

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
  for (auto *varDecl : target->getStoredProperties()) {
    if (!varDecl->isUserAccessible())
      continue;

    // Despite creating the enum in the context of the type, we're
    // concurrently checking the variables for the current protocol
    // conformance being synthesized, for which we use the conformance
    // context, not the type.
    auto conformance = varConformsToCodable(tc, derived.getConformanceContext(),
                                            varDecl, derived.Protocol);
    switch (conformance) {
      case Conforms:
      {
        auto *elt = new (C) EnumElementDecl(SourceLoc(),
                                            getVarNameForCoding(varDecl),
                                            nullptr, SourceLoc(), nullptr,
                                            enumDecl);
        elt->setImplicit();
        enumDecl->addMember(elt);
        break;
      }

      case DoesNotConform:
        tc.diagnose(varDecl->getLoc(),
                    diag::codable_non_conforming_property_here,
                    derived.getProtocolType(), varDecl->getType());
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
/// \param introducer Whether to declare the variable as immutable.
static VarDecl *createKeyedContainer(ASTContext &C, DeclContext *DC,
                                     NominalTypeDecl *keyedContainerDecl,
                                     Type keyType,
                                     VarDecl::Introducer introducer) {
  // Bind Keyed*Container to Keyed*Container<KeyType>
  Type boundType[1] = {keyType};
  auto containerType = BoundGenericType::get(keyedContainerDecl, Type(),
                                             C.AllocateCopy(boundType));

  // let container : Keyed*Container<KeyType>
  auto *containerDecl = new (C) VarDecl(/*IsStatic=*/false, introducer,
                                        /*IsCaptureList=*/false, SourceLoc(),
                                        C.Id_container, DC);
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
      ParamDecl(ParamDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                C.Id_keyedBy, SourceLoc(), C.Id_keyedBy, DC);
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

/// Looks up the property corresponding to the indicated coding key.
///
/// \param conformanceDC The DeclContext we're generating code within.
/// \param elt The CodingKeys enum case.
/// \param targetDecl The type to look up properties in.
///
/// \return A tuple containing the \c VarDecl for the property, the type that
/// should be passed when decoding it, and a boolean which is true if
/// \c encodeIfPresent/\c decodeIfPresent should be used for this property.
static std::tuple<VarDecl *, Type, bool>
lookupVarDeclForCodingKeysCase(DeclContext *conformanceDC,
                               EnumElementDecl *elt,
                               NominalTypeDecl *targetDecl) {
  for (auto decl : targetDecl->lookupDirect(DeclName(elt->getName()))) {
    if (auto *vd = dyn_cast<VarDecl>(decl)) {
      // If we found a property with an attached wrapper, retrieve the
      // backing property.
      if (auto backingVar = vd->getPropertyWrapperBackingProperty())
        vd = backingVar;

      if (!vd->isStatic()) {
        // This is the VarDecl we're looking for.

        auto varType =
            conformanceDC->mapTypeIntoContext(vd->getValueInterfaceType());

        bool useIfPresentVariant = false;

        if (auto objType = varType->getOptionalObjectType()) {
          varType = objType;
          useIfPresentVariant = true;
        }

        return std::make_tuple(vd, varType, useIfPresentVariant);
      }
    }
  }

  llvm_unreachable("Should have found at least 1 var decl");
}

/// Synthesizes the body for `func encode(to encoder: Encoder) throws`.
///
/// \param encodeDecl The function decl whose body to synthesize.
static std::pair<BraceStmt *, bool>
deriveBodyEncodable_encode(AbstractFunctionDecl *encodeDecl, void *) {
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
  auto conformanceDC = encodeDecl->getDeclContext();
  auto *targetDecl = conformanceDC->getSelfNominalTypeDecl();

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
                                             VarDecl::Introducer::Var);

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
  auto encoderParam = encodeDecl->getParameters()->get(0);
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
  auto *bindingDecl = PatternBindingDecl::createImplicit(
      C, StaticSpellingKind::None, containerPattern, callExpr, funcDC);
  statements.push_back(bindingDecl);
  statements.push_back(containerDecl);

  // Now need to generate `try container.encode(x, forKey: .x)` for all
  // existing properties. Optional properties get `encodeIfPresent`.
  for (auto *elt : codingKeysEnum->getAllElements()) {
    VarDecl *varDecl;
    Type varType;                // not used in Encodable synthesis
    bool useIfPresentVariant;

    std::tie(varDecl, varType, useIfPresentVariant) =
        lookupVarDeclForCodingKeysCase(conformanceDC, elt, targetDecl);

    // self.x
    auto *selfRef = DerivedConformance::createSelfDeclRef(encodeDecl);
    auto *varExpr = new (C) MemberRefExpr(selfRef, SourceLoc(),
                                          ConcreteDeclRef(varDecl),
                                          DeclNameLoc(), /*Implicit=*/true);

    // CodingKeys.x
    auto *eltRef = new (C) DeclRefExpr(elt, DeclNameLoc(), /*implicit=*/true);
    auto *metaTyRef = TypeExpr::createImplicit(codingKeysType, C);
    auto *keyExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);

    // encode(_:forKey:)/encodeIfPresent(_:forKey:)
    auto methodName = useIfPresentVariant ? C.Id_encodeIfPresent : C.Id_encode;

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
  return { body, /*isTypeChecked=*/false };
}

/// Synthesizes a function declaration for `encode(to: Encoder) throws` with a
/// lazily synthesized body for the given type.
///
/// Adds the function declaration to the given type before returning it.
static FuncDecl *deriveEncodable_encode(DerivedConformance &derived) {
  auto &C = derived.TC.Context;
  auto conformanceDC = derived.getConformanceContext();

  // Expected type: (Self) -> (Encoder) throws -> ()
  // Constructed as: func type
  //                 input: Self
  //                 throws
  //                 output: function type
  //                         input: Encoder
  //                         output: ()
  // Create from the inside out:

  auto encoderType = C.getEncoderDecl()->getDeclaredInterfaceType();
  auto returnType = TupleType::getEmpty(C);

  // Params: (Encoder)
  auto *encoderParam = new (C)
      ParamDecl(ParamDecl::Specifier::Default, SourceLoc(), SourceLoc(), C.Id_to,
                SourceLoc(), C.Id_encoder, conformanceDC);
  encoderParam->setInterfaceType(encoderType);

  ParameterList *params = ParameterList::createWithoutLoc(encoderParam);

  // Func name: encode(to: Encoder)
  DeclName name(C, C.Id_encode, params);
  auto *encodeDecl = FuncDecl::create(
      C, SourceLoc(), StaticSpellingKind::None, SourceLoc(), name, SourceLoc(),
      /*Throws=*/true, SourceLoc(), nullptr, params,
      TypeLoc::withoutLoc(returnType), conformanceDC);
  encodeDecl->setImplicit();
  encodeDecl->setSynthesized();
  encodeDecl->setBodySynthesizer(deriveBodyEncodable_encode);

  // This method should be marked as 'override' for classes inheriting Encodable
  // conformance from a parent class.
  auto *classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  if (classDecl && superclassIsEncodable(classDecl)) {
    auto *attr = new (C) OverrideAttr(/*IsImplicit=*/true);
    encodeDecl->getAttrs().add(attr);
  }

  if (auto env = conformanceDC->getGenericEnvironmentOfContext())
    encodeDecl->setGenericEnvironment(env);
  encodeDecl->computeType(FunctionType::ExtInfo().withThrows());

  encodeDecl->setValidationToChecked();
  encodeDecl->copyFormalAccessFrom(derived.Nominal,
                                   /*sourceIsParentContext*/ true);

  C.addSynthesizedDecl(encodeDecl);

  derived.addMembersToConformanceContext({encodeDecl});
  return encodeDecl;
}

/// Synthesizes the body for `init(from decoder: Decoder) throws`.
///
/// \param initDecl The function decl whose body to synthesize.
static std::pair<BraceStmt *, bool>
deriveBodyDecodable_init(AbstractFunctionDecl *initDecl, void *) {
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
  auto conformanceDC = initDecl->getDeclContext();
  auto *targetDecl = conformanceDC->getSelfNominalTypeDecl();

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
                                             VarDecl::Introducer::Let);

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
    auto decoderParam = initDecl->getParameters()->get(0);
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
    auto *bindingDecl = PatternBindingDecl::createImplicit(
        C, StaticSpellingKind::None, containerPattern, tryExpr, funcDC);
    statements.push_back(bindingDecl);
    statements.push_back(containerDecl);

    // Now need to generate `x = try container.decode(Type.self, forKey: .x)`
    // for all existing properties. Optional properties get `decodeIfPresent`.
    for (auto *elt : enumElements) {
      VarDecl *varDecl;
      Type varType;
      bool useIfPresentVariant;

      std::tie(varDecl, varType, useIfPresentVariant) =
          lookupVarDeclForCodingKeysCase(conformanceDC, elt, targetDecl);

      // Don't output a decode statement for a var let with a default value.
      if (varDecl->isLet() && varDecl->isParentInitialized())
        continue;

      auto methodName =
          useIfPresentVariant ? C.Id_decodeIfPresent : C.Id_decode;

      // Type.self (where Type === type(of: x))
      // Calculating the metatype needs to happen after potential Optional
      // unwrapping in lookupVarDeclForCodingKeysCase().
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

      auto *selfRef = DerivedConformance::createSelfDeclRef(initDecl);
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
        assert(!superInitDecl->isFailable());

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
  return { body, /*isTypeChecked=*/false };
}

/// Synthesizes a function declaration for `init(from: Decoder) throws` with a
/// lazily synthesized body for the given type.
///
/// Adds the function declaration to the given type before returning it.
static ValueDecl *deriveDecodable_init(DerivedConformance &derived) {
  auto &C = derived.TC.Context;

  auto classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  auto conformanceDC = derived.getConformanceContext();

  // Expected type: (Self) -> (Decoder) throws -> (Self)
  // Constructed as: func type
  //                 input: Self
  //                 throws
  //                 output: function type
  //                         input: Encoder
  //                         output: Self
  // Compute from the inside out:

  // Params: (Decoder)
  auto decoderType = C.getDecoderDecl()->getDeclaredInterfaceType();
  auto *decoderParamDecl = new (C) ParamDecl(
      ParamDecl::Specifier::Default, SourceLoc(), SourceLoc(), C.Id_from,
      SourceLoc(), C.Id_decoder, conformanceDC);
  decoderParamDecl->setImplicit();
  decoderParamDecl->setInterfaceType(decoderType);

  auto *paramList = ParameterList::createWithoutLoc(decoderParamDecl);

  // Func name: init(from: Decoder)
  DeclName name(C, DeclBaseName::createConstructor(), paramList);

  auto *initDecl =
      new (C) ConstructorDecl(name, SourceLoc(),
                              /*Failable=*/false,SourceLoc(),
                              /*Throws=*/true, SourceLoc(), paramList,
                              /*GenericParams=*/nullptr, conformanceDC);
  initDecl->setImplicit();
  initDecl->setSynthesized();
  initDecl->setBodySynthesizer(&deriveBodyDecodable_init);

  // This constructor should be marked as `required` for non-final classes.
  if (classDecl && !classDecl->isFinal()) {
    auto *reqAttr = new (C) RequiredAttr(/*IsImplicit=*/true);
    initDecl->getAttrs().add(reqAttr);
  }

  if (auto env = conformanceDC->getGenericEnvironmentOfContext())
    initDecl->setGenericEnvironment(env);
  initDecl->computeType(AnyFunctionType::ExtInfo().withThrows());

  initDecl->setValidationToChecked();
  initDecl->copyFormalAccessFrom(derived.Nominal,
                                 /*sourceIsParentContext*/ true);

  C.addSynthesizedDecl(initDecl);

  derived.addMembersToConformanceContext({initDecl});
  return initDecl;
}

/// Returns whether the given type is valid for synthesizing {En,De}codable.
///
/// Checks to see whether the given type has a valid \c CodingKeys enum, and if
/// not, attempts to synthesize one for it.
///
/// \param requirement The requirement we want to synthesize.
static bool canSynthesize(DerivedConformance &derived, ValueDecl *requirement) {
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
  auto &tc = derived.TC;
  ASTContext &C = tc.Context;
  auto proto = derived.Protocol;
  auto *classDecl = dyn_cast<ClassDecl>(derived.Nominal);
  if (proto->isSpecificProtocol(KnownProtocolKind::Decodable) && classDecl) {
    if (auto *superclassDecl = classDecl->getSuperclassDecl()) {
      DeclName memberName;
      auto superType = superclassDecl->getDeclaredInterfaceType();
      if (TypeChecker::conformsToProtocol(superType, proto, superclassDecl,
                                          None)) {
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
        superclassDecl->diagnose(diag::decodable_no_super_init_here,
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
        auto conformanceDC = derived.getConformanceContext();
        if (!initializer->isDesignatedInit()) {
          // We must call a superclass's designated initializer.
          initializer->diagnose(diag::decodable_super_init_not_designated_here,
                                requirement->getFullName(), memberName);
          return false;
        } else if (!initializer->isAccessibleFrom(conformanceDC)) {
          // Cannot call an inaccessible method.
          auto accessScope = initializer->getFormalAccessScope(conformanceDC);
          initializer->diagnose(diag::decodable_inaccessible_super_init_here,
                                requirement->getFullName(), memberName,
                                accessScope.accessLevelForDiagnostics());
          return false;
        } else if (initializer->isFailable()) {
          // We can't call super.init() if it's failable, since init(from:)
          // isn't failable.
          initializer->diagnose(diag::decodable_super_init_is_failable_here,
                                requirement->getFullName(), memberName);
          return false;
        }
      }
    }
  }

  // If the target already has a valid CodingKeys enum, we won't need to
  // synthesize one.
  auto validity = hasValidCodingKeysEnum(derived);

  // We found a type, but it wasn't valid.
  if (!validity.isValid)
    return false;

  // We can try to synthesize a type here.
  if (!validity.hasType) {
    auto *synthesizedEnum = synthesizeCodingKeysEnum(derived);
    if (!synthesizedEnum)
      return false;
  }

  return true;
}

ValueDecl *DerivedConformance::deriveEncodable(ValueDecl *requirement) {
  // We can only synthesize Encodable for structs and classes.
  if (!isa<StructDecl>(Nominal) && !isa<ClassDecl>(Nominal))
    return nullptr;

  if (requirement->getBaseName() != TC.Context.Id_encode) {
    // Unknown requirement.
    TC.diagnose(requirement->getLoc(), diag::broken_encodable_requirement);
    return nullptr;
  }

  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;

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
  DiagnosticTransaction diagnosticTransaction(TC.Context.Diags);
  TC.diagnose(ConformanceDecl, diag::type_does_not_conform,
              Nominal->getDeclaredType(), getProtocolType());
  TC.diagnose(requirement, diag::no_witnesses, diag::RequirementKind::Func,
              requirement->getFullName(), getProtocolType(),
              /*AddFixIt=*/false);

  // Check other preconditions for synthesized conformance.
  // This synthesizes a CodingKeys enum if possible.
  if (canSynthesize(*this, requirement)) {
    diagnosticTransaction.abort();
    return deriveEncodable_encode(*this);
  }

  return nullptr;
}

ValueDecl *DerivedConformance::deriveDecodable(ValueDecl *requirement) {
  // We can only synthesize Encodable for structs and classes.
  if (!isa<StructDecl>(Nominal) && !isa<ClassDecl>(Nominal))
    return nullptr;

  if (requirement->getBaseName() != DeclBaseName::createConstructor()) {
    // Unknown requirement.
    TC.diagnose(requirement->getLoc(), diag::broken_decodable_requirement);
    return nullptr;
  }

  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;

  // We're about to try to synthesize Decodable. If something goes wrong,
  // we'll have to output at least one error diagnostic. We need to collate
  // diagnostics produced by canSynthesize and deriveDecodable_init to produce
  // them in the right order -- see the comment in deriveEncodable for
  // background on this transaction.
  DiagnosticTransaction diagnosticTransaction(TC.Context.Diags);
  TC.diagnose(ConformanceDecl->getLoc(), diag::type_does_not_conform,
              Nominal->getDeclaredType(), getProtocolType());
  TC.diagnose(requirement, diag::no_witnesses,
              diag::RequirementKind::Constructor, requirement->getFullName(),
              getProtocolType(), /*AddFixIt=*/false);

  // Check other preconditions for synthesized conformance.
  // This synthesizes a CodingKeys enum if possible.
  if (canSynthesize(*this, requirement)) {
    diagnosticTransaction.abort();
    return deriveDecodable_init(*this);
  }

  return nullptr;
}
