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

// Returns whether the type represented by the given ClassDecl inherits from a
// type which is Codable.
static bool superclassIsCodable(TypeChecker &tc, ClassDecl *type) {
  assert(type && "Cannot get superclass of null type.");
  if (!type->hasSuperclass())
    return false;

  ASTContext &C = tc.Context;
  auto parentDC = type->getDeclContext();
  auto superType = type->getSuperclass();
  return (bool)tc.conformsToProtocol(superType,
                                     C.getProtocol(KnownProtocolKind::Codable),
                                     parentDC,
                                     ConformanceCheckFlags::Used);
}

// Looks up a type declaration with the given name in the Foundation module.
//
// Asserts that the Foundation module is loaded, that the given name does not
// refer to more than one type, and that the entity with the given name refers
// to a type instead of a different type of declaration.
static TypeDecl* lookupFoundationTypeDecl(ASTContext &C, Identifier name) {
  auto foundationModule = C.getLoadedModule(C.Id_Foundation);
  assert(foundationModule && "Foundation module must be loaded.");

  SmallVector<ValueDecl*, 1> results;
  foundationModule->lookupMember(results, cast<DeclContext>(foundationModule),
                                 DeclName(name), Identifier());
  assert(results.size() == 1 && "Ambiguous/missing type.");

  auto typeDecl = dyn_cast<TypeDecl>(results[0]);
  assert(typeDecl && "Found non-type decl.");
  return typeDecl;
}

// Looks up a type with the given name in the Foundation module.
//
// Asserts that the Foundation module is loaded, that the given name does not
// refer to more than one type, and that the entity with the given name refers
// to a type instead of a different type of declaration.
static Type lookupFoundationType(ASTContext &C, Identifier name) {
  return lookupFoundationTypeDecl(C, name)->getDeclaredInterfaceType();
}

// Validates that all the variables declared in the given list of declarations
// conform to the Codable protocol.
//
// Produces a diagnostic on the given typechecker for every var which does not
// conform. Calls a success callback for every var which does conform.
template <typename ValidVarCallback>
static bool
validateVarsConformToCodable(TypeChecker &tc, DeclContext *context,
                             NominalTypeDecl::StoredPropertyRange &vars,
                             ValidVarCallback &callback) {
  auto codableProto = tc.Context.getProtocol(KnownProtocolKind::Codable);
  bool allConform = true;
  for (auto varDecl : vars) {
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
    if (varDecl->hasType() &&
        tc.conformsToProtocol(varDecl->getType(), codableProto, context,
                              ConformanceCheckFlags::Used)) {
      callback(varDecl);
    } else {
      // TODO: We should produce a diagnostic note here explaining that we found
      //       a var not conforming to Codable.
      allConform = false;
    }
  }

  return allConform;
}

// Validates the given CodingKeys enum decl by ensuring its cases are a 1-to-1
// match with the stored vars of the given type.
static bool
validateCodingKeysEnum(TypeChecker &tc, EnumDecl *codingKeysDecl,
                       NominalTypeDecl *type) {
  // Look through all var decls in the given type.
  // * Filter out lazy/computed vars (currently already done by
  //   getStoredProperties).
  // * Filter out ones which are present in the given decl (by name).
  //
  // If any of the entries in the CodingKeys decl are not present in the type
  // by name, then this decl doesn't match.
  // If there are any vars left in the type, then this decl doesn't match.
  //
  // NOTE: If we change the behavior to ignore vars with default values, then we
  //       can further filter out the type names to remove those which
  //       correspond to vars with default values.
  llvm::DenseSet<Identifier> names;

  auto storedProperties = type->getStoredProperties(/*skipInaccessible=*/true);
  auto validVarCallback = [&names](VarDecl *varDecl) {
    names.insert(varDecl->getName());
  };

  if (!validateVarsConformToCodable(tc, type->getDeclContext(),
                                    storedProperties, validVarCallback))
    return false;

  for (auto elt : codingKeysDecl->getAllElements()) {
    auto it = names.find(elt->getName());
    if (it == names.end()) {
      // TODO: Produce diagnostic here complaining that the CodingKeys enum
      //       contains a case which does not correspond to a var.
      // TODO: Investigate typo-correction here; perhaps the case name was
      //       misspelled and we can provide a fix-it.
      return false;
    }

    names.erase(it);
  }

  // TODO: Produce diagnostic here complaining that there are vars which are not
  //       listed in the CodingKeys enum.
  return names.empty();
}

// Fetches or synthesizes a CodingKeys type nested in the given type.
//
// If the given type declaration has a nested CodingKeys entity, returns it if
// it is a valid NominalTypeDecl (nullptr otherwise).
// If the given type declaration does not have a nested CodingKeys entity,
// synthesizes a new enum based on the Codable members of the given type
// (nullptr if unable to synthesize).
static std::pair<NominalTypeDecl*, /* attempted to synthesize? */bool>
getOrSynthesizeCodingKeysDecl(TypeChecker &tc, Decl *parentDecl,
                              NominalTypeDecl *type) {
  ASTContext &C = tc.Context;
  auto parentDC = cast<DeclContext>(parentDecl);
  auto codingKeyProto = C.getProtocol(KnownProtocolKind::CodingKey);
  auto typeDC = cast<DeclContext>(type);

  auto codingKeysDecls = type->lookupDirect(DeclName(C.Id_CodingKeys));
  if (!codingKeysDecls.empty()) {
    // Only ill-formed code would produce multiple results for this lookup.
    // This would get diagnosed later anyway, so we're free to only look at the
    // first result here.
    auto result = codingKeysDecls.front();

    auto typeDecl = dyn_cast<TypeDecl>(result);
    if (!typeDecl) {
      // TODO: Produce a diagnostic complaining that the "CodingKeys" entity we
      //       found is not a type.
      return {nullptr, /* attempted to synthesize? */false};
    }

    // Ensure that the type we found conforms to the CodingKey protocol.
    auto type = typeDecl->getDeclaredInterfaceType();
    if (!tc.conformsToProtocol(type, codingKeyProto, parentDC,
                               ConformanceCheckFlags::Used)) {
      // TODO: Produce a diagnostic complaining that the "CodingKeys" entity we
      //       found does not conform to CodingKey.
      return {nullptr, /* attempted to synthesize? */false};
    }

    // CodingKeys should eventually be a nominal type. If it's a typealias,
    // we'll need to follow it.
    auto codingKeysDecl = dyn_cast<NominalTypeDecl>(result);
    if (auto typealias = dyn_cast<TypeAliasDecl>(result)) {
      // TODO: Do we have to follow through multiple layers of typealiases
      //       here? Or will getCanonicalType() do that for us?
      auto canType = type->getCanonicalType();
      assert(canType);

      codingKeysDecl = type->getNominalOrBoundGenericNominal();
    }

    if (!codingKeysDecl) {
      // We ended up with something that's not a nominal type? Need to look
      // into how this would happen.
      // TODO: Produce a diagnostic complaining that we cannot derive Codable
      //       with a CodingKeys enum that is not a nominal type.
      return {nullptr, /* attempted to synthesize? */false};
    }

    // Didn't synthesize this decl.
    return {codingKeysDecl, /* attempted to synthesize? */false};
  }

  // The type does not have a CodingKeys enum. We can synthesize one here.
  // We want to look through all the var declarations of this type to create
  // enum cases based on those var names.
  auto proto = C.getProtocol(KnownProtocolKind::CodingKey);
  TypeLoc protoTypeLoc[1] = { TypeLoc::withoutLoc(proto->getDeclaredType()) };
  MutableArrayRef<TypeLoc> inherited = C.AllocateCopy(protoTypeLoc);

  auto enumDecl = new (C) EnumDecl(SourceLoc(), C.Id_CodingKeys, SourceLoc(),
                                   inherited, nullptr, typeDC);
  enumDecl->setImplicit();
  enumDecl->setAccessibility(Accessibility::Private);

  auto enumDC = cast<DeclContext>(enumDecl);
  auto mutableEnumDC = cast<IterableDeclContext>(enumDecl);

  // For classes which inherit from something Codable, we provide case `super`
  // as the first key (to be used in encoding super).
  auto classDecl = dyn_cast<ClassDecl>(type);
  if (superclassIsCodable(tc, classDecl)) {
    // TODO: Ensure the class doesn't already have or inherit a variable named
    // "`super`"; otherwise we will generate an invalid enum. In that case,
    // diagnose and bail.
    auto super = new (C) EnumElementDecl(SourceLoc(), C.Id_super, TypeLoc(),
                                         /*HasArgumentType=*/false,
                                         SourceLoc(), nullptr, enumDC);
    super->setImplicit();
    mutableEnumDC->addMember(super);
  }

  // Each of these vars needs a case in the enum. For each var decl, if the type
  // conforms to Codable, add it to the enum.
  auto storedProperties = type->getStoredProperties(/*skipInaccessible=*/true);
  auto validVarCallback = [&C, &enumDC, &mutableEnumDC](VarDecl *varDecl) {
    auto elt = new (C) EnumElementDecl(SourceLoc(), varDecl->getName(),
                                       TypeLoc(), /*HasArgumentType=*/false,
                                       SourceLoc(), nullptr,
                                       enumDC);
    elt->setImplicit();
    mutableEnumDC->addMember(elt);
  };

  if (!validateVarsConformToCodable(tc, parentDC, storedProperties,
                                    validVarCallback))
    return {nullptr, /* attempted to synthesize? */true};

  // Forcibly derive conformance to CodingKey.
  tc.checkConformancesInContext(enumDC, mutableEnumDC);

  // "Synthesized" a null enum decl.
  return {enumDecl, /* attempted to synthesize? */true};
}

// Creates a new var decl representing
//
//   let container : containerBase<keyType>
//
// containerBase is the name of the type to use as the base (either
// KeyedEncodingContainer or KeyedDecodingContainer).
static VarDecl*
createKeyedContainer(ASTContext &C, DeclContext *DC, Identifier containerBase,
                     Type keyType) {
  // Look up Keyed*Container
  auto keyedContainerDecl =
    cast<NominalTypeDecl>(lookupFoundationTypeDecl(C, containerBase));

  // Bind Keyed*Container to Keyed*Container<KeyType>
  Type boundType[1] = { keyType };
  auto containerType = BoundGenericType::get(keyedContainerDecl, Type(),
                                             C.AllocateCopy(boundType));

  // let container : Keyed*Container<KeyType>
  auto containerDecl = new (C) VarDecl(/*IsStatic=*/false, /*IsLet=*/true,
                                       /*IsCaptureList=*/false, SourceLoc(),
                                       C.Id_container, containerType, DC);
  containerDecl->setImplicit();
  containerDecl->setInterfaceType(containerType);
  return containerDecl;
}

static CallExpr*
createContainerKeyedByCall(ASTContext &C, DeclContext *DC, Expr *base,
                           Type returnType, NominalTypeDecl *param) {
  // (keyedBy:)
  auto keyedByDecl = new (C) ParamDecl(/*IsLet=*/true, SourceLoc(),
                                       SourceLoc(), C.Id_keyedBy,
                                       SourceLoc(), C.Id_keyedBy,
                                       returnType, DC);
  keyedByDecl->setImplicit();
  keyedByDecl->setInterfaceType(returnType);

  // container(keyedBy:) method name
  auto paramList = ParameterList::createWithoutLoc(keyedByDecl);
  DeclName callName(C, C.Id_container, paramList);

  // base.container(keyedBy:) expr
  auto unboundCall = new (C) UnresolvedDotExpr(base, SourceLoc(), callName,
                                               DeclNameLoc(),
                                               /*Implicit=*/true);

  // CodingKeys.self expr
  auto codingKeysExpr = new (C) DeclRefExpr(ConcreteDeclRef(param),
                                            DeclNameLoc(), /*Implicit=*/true);
  auto codingKeysMetaTypeExpr = new (C) DotSelfExpr(codingKeysExpr, SourceLoc(),
                                                    SourceLoc());

  // Full bound base.container(keyedBy: CodingKeys.self) call
  Expr* args[1] = { codingKeysMetaTypeExpr };
  Identifier argLabels[1] = { C.Id_keyedBy };
  return CallExpr::createImplicit(C, unboundCall, C.AllocateCopy(args),
                                  C.AllocateCopy(argLabels));
}

static void deriveBodyCodable_encode(AbstractFunctionDecl *encodeDecl) {
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
  //     let container = encoder.container(keyedBy: CodingKeys.self)
  //     try container.encode(x, forKey: .x)
  //     try container.encode(y, forKey: .y)
  //   }
  // }

  // The enclosing type decl.
  auto typeDecl = cast<NominalTypeDecl>(encodeDecl->getDeclContext());

  auto funcDC = cast<DeclContext>(encodeDecl);
  ASTContext &C = funcDC->getASTContext();
  auto &tc = *(TypeChecker *)C.getLazyResolver();

  // We'll want the CodingKeys enum for this type.
  NominalTypeDecl *codingKeysDecl;
  bool derived;
  std::tie(codingKeysDecl, derived) = getOrSynthesizeCodingKeysDecl(tc,
                                                                    typeDecl,
                                                                    typeDecl);

  // We should have bailed already if:
  // a) The type does not have CodingKeys
  assert(codingKeysDecl && "Missing CodingKeys decl.");
  // b) We had to derive it right here
  assert(!derived && "Had to derive CodingKeys type too late.");
  // c) The type is not an enum
  auto codingKeysEnum = cast<EnumDecl>(codingKeysDecl);

  SmallVector<ASTNode, 5> statements;

  // Generate a reference to containerExpr ahead of time in case there are no
  // properties to encode or decode, but the type is a class which inherits from
  // something Codable and needs to encode super.

  // let container : KeyedEncodingContainer<CodingKeys>
  auto codingKeysType = codingKeysEnum->getDeclaredType();
  auto containerDecl = createKeyedContainer(C, funcDC,
                                            C.Id_KeyedEncodingContainer,
                                            codingKeysType);

  auto containerExpr = new (C) DeclRefExpr(ConcreteDeclRef(containerDecl),
                                           DeclNameLoc(), /*Implicit=*/true,
                                           AccessSemantics::DirectToStorage);

  auto enumElements = codingKeysEnum->getAllElements();
  if (!enumElements.empty()) {
    // Need to generate
    //   `let container = encoder.container(keyedBy: CodingKeys.self)`
    // `let container` (containerExpr) is generated above.

    // encoder
    auto encoderParam = encodeDecl->getParameterList(1)->get(0);
    auto encoderExpr = new (C) DeclRefExpr(ConcreteDeclRef(encoderParam),
                                           DeclNameLoc(), /*Implicit=*/true);

    // Bound encoder.container(keyedBy: CodingKeys.self) call
    auto containerType = containerDecl->getInterfaceType();
    auto callExpr = createContainerKeyedByCall(C, funcDC, encoderExpr,
                                               containerType, codingKeysEnum);

    // Full `let container = encoder.container(keyedBy: CodingKeys.self)`
    // binding.
    auto containerPattern = new (C) NamedPattern(containerDecl,
                                                 /*implicit=*/true);
    auto bindingDecl = PatternBindingDecl::create(C, SourceLoc(),
                                                  StaticSpellingKind::None,
                                                  SourceLoc(), containerPattern,
                                                  callExpr, funcDC);
    statements.push_back(bindingDecl);
    statements.push_back(containerDecl);

    // Now need to generate `try container.encode(x, forKey: .x)` for all
    // existing properties.
    for (auto elt : enumElements) {
      // Only ill-formed code would produce multiple results for this lookup.
      // This would get diagnosed later anyway, so we're free to only look at
      // the first result here.
      auto matchingVars = typeDecl->lookupDirect(DeclName(elt->getName()));

      // self.x
      auto selfRef = createSelfDeclRef(encodeDecl);
      auto varExpr = new (C) MemberRefExpr(selfRef, SourceLoc(),
                                           ConcreteDeclRef(matchingVars[0]),
                                           DeclNameLoc(), /*Implicit=*/true);

      // CodingKeys.x
      auto eltRef = new (C) DeclRefExpr(elt, DeclNameLoc(), /*implicit=*/true);
      auto metaTyRef = TypeExpr::createImplicit(codingKeysType, C);
      auto keyExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);

      // encode(_:forKey:)
      SmallVector<Identifier, 2> argNames{Identifier(), C.Id_forKey};
      DeclName name(C, C.Id_encode, argNames);
      auto encodeCall = new (C) UnresolvedDotExpr(containerExpr, SourceLoc(),
                                                  name, DeclNameLoc(),
                                                  /*Implicit=*/true);

      // container.encode(self.x, forKey: CodingKeys.x)
      Expr* args[2] = { varExpr, keyExpr };
      auto callExpr = CallExpr::createImplicit(C, encodeCall,
                                               C.AllocateCopy(args),
                                               C.AllocateCopy(argNames));

      // try container.encode(self.x, forKey: CodingKeys.x)
      auto tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                     /*Implicit=*/true);
      statements.push_back(tryExpr);
    }
  }

  // Classes which inherit from something Codable should encode super as well.
  auto classDecl = dyn_cast<ClassDecl>(typeDecl);
  if (superclassIsCodable(tc, classDecl)) {
    // Need to generate `try super.encode(to: container.superEncoder())`

    // superEncoder()
    auto method = new (C) UnresolvedDeclRefExpr(DeclName(C.Id_superEncoder),
                                                         DeclRefKind::Ordinary,
                                                         DeclNameLoc());

    // container.superEncoder()
    auto superEncoderRef = new (C) DotSyntaxCallExpr(containerExpr, SourceLoc(),
                                                     method);

    // encode(to:) expr
    auto encodeDeclRef = new (C) DeclRefExpr(ConcreteDeclRef(encodeDecl),
                                             DeclNameLoc(), /*Implicit=*/true);

    // super
    auto superRef = new (C) SuperRefExpr(encodeDecl->getImplicitSelfDecl(),
                                         SourceLoc(), /*Implicit=*/true);

    // super.encode(to:)
    auto encodeCall = new (C) DotSyntaxCallExpr(superRef, SourceLoc(),
                                                encodeDeclRef);

    // super.encode(to: container.superEncoder())
    Expr* args[1] = { superEncoderRef };
    Identifier argLabels[1] = { C.Id_to };
    auto callExpr = CallExpr::createImplicit(C, encodeCall,
                                             C.AllocateCopy(args),
                                             C.AllocateCopy(argLabels));

    // try super.encode(to: container.superEncoder())
    auto tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                   /*Implicit=*/true);
    statements.push_back(tryExpr);
  }

  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
                                /*implicit=*/true);
  encodeDecl->setBody(body);
}

static ValueDecl *deriveCodable_encode(TypeChecker &tc, Decl *parentDecl,
                                       NominalTypeDecl *type) {
  ASTContext &C = tc.Context;
  auto typeDC = cast<DeclContext>(type);

  // Expected type: (Self) -> (Encoder) throws -> ()
  // Constructed as: func type
  //                 input: Self
  //                 throws
  //                 output: function type
  //                         input: Encoder
  //                         output: ()
  // Create from the inside out:

  // (to: Encoder)
  auto encoderType = lookupFoundationType(C, C.Id_Encoder);
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
  auto selfDecl = ParamDecl::createSelf(SourceLoc(), typeDC);
  auto encoderParam = new (C) ParamDecl(/*isLet=*/true, SourceLoc(),
                                        SourceLoc(), C.Id_to,
                                        SourceLoc(), C.Id_encoder,
                                        encoderType, typeDC);
  encoderParam->setInterfaceType(encoderType);

  ParameterList *params[] = {
    ParameterList::createWithoutLoc(selfDecl),
    ParameterList::createWithoutLoc(encoderParam)
  };

  // Func name: encode(to: Encoder)
  DeclName name(C, C.Id_encode, params[1]);
  auto encodeDecl = FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None,
                                     SourceLoc(), name, SourceLoc(),
                                     /*Throws=*/true, SourceLoc(),
                                     SourceLoc(), nullptr, params,
                                     TypeLoc::withoutLoc(returnType), typeDC);
  encodeDecl->setImplicit();
  encodeDecl->setBodySynthesizer(deriveBodyCodable_encode);

  // This method should be marked as 'override' for classes inheriting Codable
  // conformance from a parent class.
  if (auto classDecl = dyn_cast<ClassDecl>(type)) {
    if (superclassIsCodable(tc, classDecl)) {
      auto attr = new (C) SimpleDeclAttr<DAK_Override>(/*IsImplicit=*/true);
      encodeDecl->getAttrs().add(attr);
    }
  }

  // Evaluate the type of Self in (Self) -> (Encoder) throws -> ().
  Type selfType = typeDC->getDeclaredInterfaceType();
  Type interfaceType;
  if (auto sig = typeDC->getGenericSignatureOfContext()) {
    // Evaluate the below, but in a generic environment (if Self is generic).
    encodeDecl->setGenericEnvironment(
        typeDC->getGenericEnvironmentOfContext());
    interfaceType = GenericFunctionType::get(sig, selfType, innerType,
                                             FunctionType::ExtInfo());
  } else {
    // (Self) -> innerType == (Encoder) throws -> ()
    interfaceType = FunctionType::get(selfType, innerType);
  }

  encodeDecl->setInterfaceType(interfaceType);
  encodeDecl->setAccessibility(std::max(type->getFormalAccess(),
                                        Accessibility::Internal));

  // If the type was not imported, the derived conformance is either from the
  // type itself or an extension, in which case we will emit the declaration
  // normally.
  if (type->hasClangNode())
    tc.Context.addExternalDecl(encodeDecl);

  cast<IterableDeclContext>(type)->addMember(encodeDecl);
  return encodeDecl;

}

static void deriveBodyCodable_init(AbstractFunctionDecl *initDecl) {
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
  auto typeDecl = cast<NominalTypeDecl>(initDecl->getDeclContext());

  auto funcDC = cast<DeclContext>(initDecl);
  ASTContext &C = funcDC->getASTContext();
  auto &tc = *(TypeChecker *)C.getLazyResolver();

  // We'll want the CodingKeys enum for this type.
  NominalTypeDecl *codingKeysDecl;
  bool derived;
  std::tie(codingKeysDecl, derived) = getOrSynthesizeCodingKeysDecl(tc,
                                                                    typeDecl,
                                                                    typeDecl);

  // We should have bailed already if:
  // a) The type does not have CodingKeys
  assert(codingKeysDecl && "Missing CodingKeys decl.");
  // b) We had to derive it right here
  assert(!derived && "Had to derive CodingKeys type too late.");
  // c) The type is not an enum
  auto codingKeysEnum = cast<EnumDecl>(codingKeysDecl);

  // Generate a reference to containerExpr ahead of time in case there are no
  // properties to encode or decode, but the type is a class which inherits from
  // something Codable and needs to decode super.

  // let container : KeyedDecodingContainer<CodingKeys>
  auto codingKeysType = codingKeysEnum->getDeclaredType();
  auto containerDecl = createKeyedContainer(C, funcDC,
                                            C.Id_KeyedEncodingContainer,
                                            codingKeysType);

  auto containerExpr = new (C) DeclRefExpr(ConcreteDeclRef(containerDecl),
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
    auto decoderExpr = new (C) DeclRefExpr(ConcreteDeclRef(decoderParam),
                                           DeclNameLoc(), /*Implicit=*/true);

    // Bound decoder.container(keyedBy: CodingKeys.self) call
    auto containerType = containerDecl->getInterfaceType();
    auto callExpr = createContainerKeyedByCall(C, funcDC, decoderExpr,
                                               containerType, codingKeysEnum);

    // try decoder.container(keyedBy: CodingKeys.self)
    auto tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                   /*implicit=*/true);

    // Full `let container = decoder.container(keyedBy: CodingKeys.self)`
    // binding.
    auto containerPattern = new (C) NamedPattern(containerDecl,
                                                 /*implicit=*/true);
    auto bindingDecl = PatternBindingDecl::create(C, SourceLoc(),
                                                  StaticSpellingKind::None,
                                                  SourceLoc(), containerPattern,
                                                  tryExpr, funcDC);
    statements.push_back(bindingDecl);
    statements.push_back(containerDecl);

    // Now need to generate `x = try container.encode(Type.self, forKey: .x)`
    // for all existing properties.
    for (auto elt : enumElements) {
      // TODO: Don't decode a let var that has a default value.
      // Only ill-formed code would produce multiple results for this lookup.
      // This would get diagnosed later anyway, so we're free to only look at
      // the first result here.
      auto matchingVars = typeDecl->lookupDirect(DeclName(elt->getName()));
      auto varDecl = cast<VarDecl>(matchingVars[0]);

      // Type.self (where Type === type(of: x)
      auto varType = varDecl->getType();
      auto metaTyRef = TypeExpr::createImplicit(varType, C);
      auto typeExpr = new (C) DotSelfExpr(metaTyRef, SourceLoc(), SourceLoc(),
                                          varType);

      // CodingKeys.x
      auto eltRef = new (C) DeclRefExpr(elt, DeclNameLoc(), /*implicit=*/true);
      metaTyRef = TypeExpr::createImplicit(codingKeysType, C);
      auto keyExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);

      // container.decode(_:forKey:)
      SmallVector<Identifier, 2> argNames{Identifier(), C.Id_forKey};
      DeclName name(C, C.Id_decode, argNames);
      auto decodeCall = new (C) UnresolvedDotExpr(containerExpr, SourceLoc(),
                                                  name, DeclNameLoc(),
                                                  /*Implicit=*/true);

      // container.decode(Type.self, forKey: CodingKeys.x)
      Expr* args[2] = { typeExpr, keyExpr };
      auto callExpr = CallExpr::createImplicit(C, decodeCall,
                                               C.AllocateCopy(args),
                                               C.AllocateCopy(argNames));

      // try container.decode(Type.self, forKey: CodingKeys.x)
      auto tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                     /*Implicit=*/true);

      auto selfRef = createSelfDeclRef(initDecl);
      auto varExpr = new (C) UnresolvedDotExpr(selfRef, SourceLoc(),
                                               DeclName(varDecl->getName()),
                                               DeclNameLoc(),
                                               /*implicit=*/true);
      auto assignExpr = new (C) AssignExpr(varExpr, SourceLoc(), tryExpr,
                                           /*Implicit=*/true);
      statements.push_back(assignExpr);
    }
  }

  // Classes which inherit from something Codable should decode super as well.
  auto classDecl = dyn_cast<ClassDecl>(typeDecl);
  if (superclassIsCodable(tc, classDecl)) {
    // Need to generate `try super.init(from: container.superDecoder())`

    // superDecoder()
    auto method = new (C) UnresolvedDeclRefExpr(DeclName(C.Id_superDecoder),
                                                         DeclRefKind::Ordinary,
                                                         DeclNameLoc());

    // container.superDecoder()
    auto superDecoderRef = new (C) DotSyntaxCallExpr(containerExpr, SourceLoc(),
                                                     method);

    // init(from:) expr
    auto initDeclRef = new (C) DeclRefExpr(ConcreteDeclRef(initDecl),
                                           DeclNameLoc(), /*Implicit=*/true);

    // super
    auto superRef = new (C) SuperRefExpr(initDecl->getImplicitSelfDecl(),
                                         SourceLoc(), /*Implicit=*/true);

    // super.init(from:)
    auto decodeCall = new (C) DotSyntaxCallExpr(superRef, SourceLoc(),
                                                initDeclRef);

    // super.decode(from: container.superDecoder())
    Expr* args[1] = { superDecoderRef };
    Identifier argLabels[1] = { C.Id_from };
    auto callExpr = CallExpr::createImplicit(C, decodeCall,
                                             C.AllocateCopy(args),
                                             C.AllocateCopy(argLabels));

    // try super.init(from: container.superDecoder())
    auto tryExpr = new (C) TryExpr(SourceLoc(), callExpr, Type(),
                                   /*Implicit=*/true);
    statements.push_back(tryExpr);
  }

  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc(),
                                /*implicit=*/true);
  initDecl->setBody(body);
}

static ValueDecl *deriveCodable_init(TypeChecker &tc, Decl *parentDecl,
                                     NominalTypeDecl *type) {
  ASTContext &C = tc.Context;
  auto typeDC = cast<DeclContext>(type);

  // Expected type: (Self) -> (Decoder) throws -> (Self)
  // Constructed as: func type
  //                 input: Self
  //                 throws
  //                 output: function type
  //                         input: Encoder
  //                         output: Self
  // Compute from the inside out:

  // (from: Decoder)
  auto decoderType = lookupFoundationType(C, C.Id_Decoder);
  auto inputTypeElt = TupleTypeElt(decoderType, C.Id_from);
  auto inputType = TupleType::get(ArrayRef<TupleTypeElt>(inputTypeElt), C);

  // throws
  auto extInfo = FunctionType::ExtInfo(FunctionTypeRepresentation::Swift,
                                       /*Throws=*/true);

  // (Self)
  auto returnType = typeDC->getDeclaredInterfaceType();

  // (from: Decoder) throws -> (Self)
  Type innerType = FunctionType::get(inputType, returnType, extInfo);

  // Params: (self [implicit], Decoder)
  // self should be inout if the type is a value type; not inout otherwise.
  auto inOut = !isa<ClassDecl>(type);
  auto selfDecl = ParamDecl::createSelf(SourceLoc(), typeDC,
                                        /*isStatic=*/false,
                                        /*isInOut=*/inOut);
  auto decoderParamDecl = new (C) ParamDecl(/*isLet=*/true, SourceLoc(),
                                            SourceLoc(), C.Id_from,
                                            SourceLoc(), C.Id_decoder,
                                            decoderType, typeDC);
  decoderParamDecl->setImplicit();
  decoderParamDecl->setInterfaceType(decoderType);

  auto paramList = ParameterList::createWithoutLoc(decoderParamDecl);

  // Func name: init(from: Decoder)
  DeclName name(C, C.Id_init, paramList);

  auto initDecl =
    new (C) ConstructorDecl(name, SourceLoc(),
                            /*Failability=*/OTK_None,
                            /*FailabilityLoc=*/SourceLoc(),
                            /*Throws=*/true, /*ThrowsLoc=*/SourceLoc(),
                            selfDecl, paramList,
                            /*GenericParams=*/nullptr, typeDC);
  initDecl->setImplicit();
  initDecl->setBodySynthesizer(deriveBodyCodable_init);

  // This constructor should be marked as `required` for non-final classes.
  if (isa<ClassDecl>(type) && type->getAttrs().hasAttribute<FinalAttr>()) {
    auto reqAttr = new (C) SimpleDeclAttr<DAK_Required>(/*IsImplicit=*/true);
    initDecl->getAttrs().add(reqAttr);
  }

  Type selfType = initDecl->computeInterfaceSelfType();
  Type selfInitType = initDecl->computeInterfaceSelfType(/*init=*/true);
  Type interfaceType;
  Type initializerType;
  if (auto sig = typeDC->getGenericSignatureOfContext()) {
    // Evaluate the below, but in a generic environment (if Self is generic).
    initDecl->setGenericEnvironment(typeDC->getGenericEnvironmentOfContext());
    interfaceType = GenericFunctionType::get(sig, selfType, innerType,
                                             FunctionType::ExtInfo());
    initializerType = GenericFunctionType::get(sig, selfInitType, innerType,
                                               FunctionType::ExtInfo());
  } else {
    // (Self) -> (Decoder) throws -> (Self)
    interfaceType = FunctionType::get(selfType, innerType);
    initializerType = FunctionType::get(selfInitType, innerType);
  }

  initDecl->setInterfaceType(interfaceType);
  initDecl->setInitializerInterfaceType(initializerType);
  initDecl->setAccessibility(std::max(type->getFormalAccess(),
                                      Accessibility::Internal));

  // If the type was not imported, the derived conformance is either from the
  // type itself or an extension, in which case we will emit the declaration
  // normally.
  if (type->hasClangNode())
    tc.Context.addExternalDecl(initDecl);

  cast<IterableDeclContext>(type)->addMember(initDecl);
  return initDecl;
}

static bool canSynthesizeCodable(TypeChecker &tc, Decl *parentDecl,
                                 NominalTypeDecl *type) {
  // Fetch or synthesize coding keys for this type. We're going to validate them
  // against the vars we encounter on this type.
  NominalTypeDecl *codingKeysDecl;
  bool derived;
  std::tie(codingKeysDecl, derived) = getOrSynthesizeCodingKeysDecl(tc,
                                                                    parentDecl,
                                                                    type);

  // We attempted to synthesize coding keys for this type but were unable to,
  // because of a type mismatch or otherwise.
  if (!codingKeysDecl) {
    // We better have "derived" this failure; if we couldn't find a CodingKeys
    // type, we should have attempted to synthesize something.
    assert(derived && "Unable to fetch CodingKeys declaration for type.");
    // TODO: Produce a diagnostic here complaining that we couldn't synthesize a
    //       a CodingKeys type (potentially carry along the diagnostic produced
    //       in getOrSynthesize...).
    return false;
  }

  auto codingKeysEnum = dyn_cast<EnumDecl>(codingKeysDecl);
  if (!codingKeysEnum) {
    // We better not have synthesized a decl that wasn't an EnumDecl. This check
    // isn't strictly necessary, but good to sanity check.
    assert(!derived && "Somehow synthesized a non-enum CodingKeys type.");

    // We can't synthesize Codable if the type has a CodingKeys struct, class,
    // or other unknown type.
    // TODO: Produce a diagnostic here complaining that we couldn't synthesize
    //       the Codable requirement because we cannot read through non-enum
    //       CodingKeys.
    return false;
  }

  // We have a CodingKeys enum decl. If the decl wasn't derived, it needs to be
  // validated.
  if (!derived && !validateCodingKeysEnum(tc, codingKeysEnum, type)) {
    // TODO: Produce a diagnostic here complaining that we couldn't synthesize
    //       the Codable requirement because of a mismatch in the enum
    //       (possibly carry forward the diagnostic produced in
    //       validateCodingKeys...).
    return false;
  }

  // If we didn't get the chance to synthesize this enum before (currently the
  // case as we don't yet have a hook for always deriving the type if need be,
  // i.e. with a fake associatedtype requirement) and synthesized it now, we can
  // add it to the type.
  // We're only going to add it if we're in the context of actually performing
  // the synthesis.
  if (derived)
    cast<IterableDeclContext>(type)->addMember(codingKeysEnum);

  return true;
}

ValueDecl *DerivedConformance::deriveCodable(TypeChecker &tc,
                                             Decl *parentDecl,
                                             NominalTypeDecl *type,
                                             ValueDecl *requirement) {
  // We can only synthesize Codable for structs and classes.
  if (!isa<StructDecl>(type) && !isa<ClassDecl>(type))
    return nullptr;

  // Check other preconditions for synthesized conformance.
  bool canSynthesize = canSynthesizeCodable(tc, parentDecl, type);

  diag::RequirementKind reqKind;
  auto name = requirement->getName();
  if (name == tc.Context.Id_init) {
    reqKind = diag::RequirementKind::Constructor;
    if (canSynthesize)
      return deriveCodable_init(tc, parentDecl, type);
  } else if (name == tc.Context.Id_encode) {
    reqKind = diag::RequirementKind::Func;
    if (canSynthesize)
      return deriveCodable_encode(tc, parentDecl, type);
  } else {
    // Unknown requirement.
    tc.diagnose(requirement->getLoc(), diag::broken_codable_requirement);
    return nullptr;
  }

  // Known protocol requirement but could not synthesize.
  // FIXME: We have to output at least one error diagnostic here because we
  // returned true from NominalTypeDecl::derivesProtocolConformance; if we
  // don't, we expect to return a witness here later and crash on an assertion.
  // Producing an error stops compilation before then.
  auto codableProto = tc.Context.getProtocol(KnownProtocolKind::Codable);
  auto codableType = codableProto->getDeclaredType();
  tc.diagnose(type, diag::type_does_not_conform, type->getDeclaredType(),
              codableType);
  tc.diagnose(requirement, diag::no_witnesses, reqKind,
              requirement->getFullName(), codableType, /*AddFixIt=*/false);
  return nullptr;
}
