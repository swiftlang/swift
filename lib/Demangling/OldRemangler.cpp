//===--- OldRemangler.cpp - Old Swift Re-mangler --------------------------===//
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
//  This file implements the remangler, which turns a demangling parse
//  tree back into a mangled string.  This is useful for tools which
//  want to extract subtrees from mangled strings.
//
//===----------------------------------------------------------------------===//

#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/Punycode.h"
#include "swift/Demangling/ManglingUtils.h"
#include "swift/AST/Ownership.h"
#include "swift/Strings.h"
#include "RemanglerBase.h"
#include <cstdio>
#include <cstdlib>

using namespace swift;
using namespace Demangle;

[[noreturn]]
static void unreachable(const char *Message) {
  fprintf(stderr, "fatal error: %s\n", Message);
  std::abort();
}

namespace {
  class Remangler : public RemanglerBase {
  public:
    Remangler(NodeFactory &Factory) : RemanglerBase(Factory) { }

    class EntityContext {
      bool AsContext = false;
      std::string AnonymousContextDiscriminator;
    public:
      bool isAsContext() const {
        return AsContext;
      }
      
      void setAnonymousContextDiscriminator(StringRef discriminator) {
        AnonymousContextDiscriminator = discriminator;
      }
      
      std::string takeAnonymousContextDiscriminator() {
        auto r = std::move(AnonymousContextDiscriminator);
        AnonymousContextDiscriminator.clear();
        return r;
      }

      class ManglingContextRAII {
        EntityContext &Ctx;
        bool SavedValue;
      public:
        ManglingContextRAII(EntityContext &ctx)
          : Ctx(ctx), SavedValue(ctx.AsContext) {
          ctx.AsContext = true;
        }

        ~ManglingContextRAII() {
          Ctx.AsContext = SavedValue;
        }
      };
    };

    void mangle(Node *node) {
      switch (node->getKind()) {
#define NODE(ID) case Node::Kind::ID: return mangle##ID(node);
#include "swift/Demangling/DemangleNodes.def"
      }
      unreachable("bad demangling tree node");
    }

    void mangleGenericArgs(Node *node, EntityContext &ctx);
    void mangleAnyNominalType(Node *node, EntityContext &ctx);

#define NODE(ID)                                                        \
    void mangle##ID(Node *node);
#define CONTEXT_NODE(ID)                                                \
    void mangle##ID(Node *node);                                        \
    void mangle##ID(Node *node, EntityContext &ctx);
#include "swift/Demangling/DemangleNodes.def"

    void mangleIndex(Node::IndexType index);
    void mangleIdentifier(StringRef name, OperatorKind operatorKind);
    void mangleAccessor(Node *storageNode, StringRef accessorCode,
                        EntityContext &ctx);

    void mangleChildNodes(Node *node) { mangleNodes(node->begin(), node->end()); }
    void mangleNodes(Node::iterator i, Node::iterator e) {
      for (; i != e; ++i) {
        mangle(*i);
      }
    }
    void mangleSingleChildNode(Node *node) {
      assert(node->getNumChildren() == 1);
      mangle(*node->begin());
    }
    void mangleChildNode(Node *node, unsigned index) {
      assert(index < node->getNumChildren());
      mangle(node->begin()[index]);
    }

    void mangleSimpleEntity(Node *node, char basicKind, StringRef entityKind,
                            EntityContext &ctx);
    void mangleNamedEntity(Node *node, char basicKind, StringRef entityKind,
                          EntityContext &ctx,
                          StringRef ArtificialPrivateDiscriminator = {});
    void mangleTypedEntity(Node *node, char basicKind, StringRef entityKind,
                           EntityContext &ctx);
    void mangleNamedAndTypedEntity(Node *node, char basicKind,
                                   StringRef entityKind,
                                   EntityContext &ctx);
    void mangleNominalType(Node *node, char basicKind, EntityContext &ctx,
                          StringRef ArtificialPrivateDiscriminator = {});

    void mangleProtocolWithoutPrefix(Node *node);
    void mangleProtocolListWithoutPrefix(Node *node,
                                         Node *additionalProto = nullptr);

    void mangleEntityContext(Node *node, EntityContext &ctx);
    void mangleEntityType(Node *node, EntityContext &ctx);
    void mangleEntityGenericType(Node *node, EntityContext &ctx);

    bool trySubstitution(Node *node, SubstitutionEntry &entry);
    bool mangleStandardSubstitution(Node *node);

    void mangleDependentGenericParamIndex(Node *node);
    void mangleConstrainedType(Node *node);
  };
} // end anonymous namespace

#define NODE(ID)
#define CONTEXT_NODE(ID)                        \
void Remangler::mangle##ID(Node *node) {        \
  EntityContext ctx;                            \
  mangle##ID(node, ctx);                        \
}
#include "swift/Demangling/DemangleNodes.def"

/// Re-apply labels from the function to its parameter type
/// to preserve old mangling style.
///
/// \param LabelList The list of labels to apply.
/// \param OrigType  The function parameter type to apply labels to.
/// \param Factory   The node factory to use to allocate new nodes.
static NodePointer applyParamLabels(NodePointer LabelList, NodePointer OrigType,
                                    NodeFactory &Factory) {
  if (LabelList->getNumChildren() == 0)
    return OrigType;

  auto applyParamLabels = [&](NodePointer ArgTuple) -> NodePointer {
    assert(ArgTuple->getKind() == Node::Kind::ArgumentTuple);

    auto ParamsType = Factory.createNode(Node::Kind::ArgumentTuple);
    auto Tuple = Factory.createNode(Node::Kind::Tuple);

    auto OrigTuple = ArgTuple->getFirstChild()->getFirstChild();
    assert(OrigTuple->getKind() == Node::Kind::Tuple);

    for (unsigned i = 0, n = OrigTuple->getNumChildren(); i != n; ++i) {
      const auto Label = LabelList->getChild(i);
      if (Label->getKind() == Node::Kind::FirstElementMarker) {
        Tuple->addChild(OrigTuple->getChild(i), Factory);
        continue;
      }

      auto OrigElt = OrigTuple->getChild(i);
      auto NewElt = Factory.createNode(Node::Kind::TupleElement);

      NewElt->addChild(Factory.createNodeWithAllocatedText(
                           Node::Kind::TupleElementName, Label->getText()),
                       Factory);

      for (auto &Child : *OrigElt)
        NewElt->addChild(Child, Factory);

      Tuple->addChild(NewElt, Factory);
    }

    auto Type = Factory.createNode(Node::Kind::Type);
    Type->addChild(Tuple, Factory);
    ParamsType->addChild(Type, Factory);
    return ParamsType;
  };

  auto visitTypeChild = [&](NodePointer Child) -> NodePointer {
    if (Child->getKind() != Node::Kind::FunctionType &&
        Child->getKind() != Node::Kind::NoEscapeFunctionType)
      return Child;

    auto FuncType = Factory.createNode(Node::Kind::FunctionType);
    for (unsigned i = 0, n = Child->getNumChildren(); i != n; ++i) {
      NodePointer FuncChild = Child->getChild(i);
      if (FuncChild->getKind() == Node::Kind::ArgumentTuple)
        FuncChild = applyParamLabels(FuncChild);
      FuncType->addChild(FuncChild, Factory);
    }
    return FuncType;
  };

  auto Type = Factory.createNode(OrigType->getKind());
  for (auto &Child : *OrigType)
    Type->addChild(visitTypeChild(Child), Factory);

  return Type;
}

bool Remangler::trySubstitution(Node *node, SubstitutionEntry &entry) {
  if (mangleStandardSubstitution(node))
    return true;

  // Go ahead and initialize the substitution entry.
  entry.setNode(node, /*treatAsIdentifier=*/ false);

  int Idx = findSubstitution(entry);
  if (Idx < 0)
    return false;

  Buffer << 'S';
  mangleIndex(Idx);
  return true;
}

static bool isInSwiftModule(Node *node) {
  Node *context = node->getFirstChild();
  return (context->getKind() == Node::Kind::Module &&
          context->getText() == STDLIB_NAME &&
          // Check for private declarations in Swift
          node->getChild(1)->getKind() == Node::Kind::Identifier);
};

bool Remangler::mangleStandardSubstitution(Node *node) {
  // Look for known substitutions.
  switch (node->getKind()) {
#define SUCCESS_IF_IS(VALUE, EXPECTED, SUBSTITUTION)            \
    do {                                                        \
      if ((VALUE) == (EXPECTED)) {                              \
        Buffer << SUBSTITUTION;                                    \
        return true;                                            \
      }                                                         \
    } while (0)
#define SUCCESS_IF_TEXT_IS(EXPECTED, SUBSTITUTION)              \
    SUCCESS_IF_IS(node->getText(), EXPECTED, SUBSTITUTION)
#define SUCCESS_IF_DECLNAME_IS(EXPECTED, SUBSTITUTION)          \
    SUCCESS_IF_IS(node->getChild(1)->getText(), EXPECTED, SUBSTITUTION)

    case Node::Kind::Module:
      SUCCESS_IF_TEXT_IS(STDLIB_NAME, "s");
      SUCCESS_IF_TEXT_IS(MANGLING_MODULE_OBJC, "So");
      SUCCESS_IF_TEXT_IS(MANGLING_MODULE_CLANG_IMPORTER, "SC");
      break;
    case Node::Kind::Structure:
      if (isInSwiftModule(node)) {
        SUCCESS_IF_DECLNAME_IS("Array", "Sa");
        SUCCESS_IF_DECLNAME_IS("Bool", "Sb");
        SUCCESS_IF_DECLNAME_IS("UnicodeScalar", "Sc");
        SUCCESS_IF_DECLNAME_IS("Double", "Sd");
        SUCCESS_IF_DECLNAME_IS("Float", "Sf");
        SUCCESS_IF_DECLNAME_IS("Int", "Si");
        SUCCESS_IF_DECLNAME_IS("UnsafeRawPointer", "SV");
        SUCCESS_IF_DECLNAME_IS("UnsafeMutableRawPointer", "Sv");
        SUCCESS_IF_DECLNAME_IS("UnsafePointer", "SP");
        SUCCESS_IF_DECLNAME_IS("UnsafeMutablePointer", "Sp");
        SUCCESS_IF_DECLNAME_IS("UnsafeBufferPointer", "SR");
        SUCCESS_IF_DECLNAME_IS("UnsafeMutableBufferPointer", "Sr");
        SUCCESS_IF_DECLNAME_IS("String", "SS");
        SUCCESS_IF_DECLNAME_IS("UInt", "Su");
      }
      break;
    case Node::Kind::Enum:
      if (isInSwiftModule(node)) {
        SUCCESS_IF_DECLNAME_IS("Optional", "Sq");
        SUCCESS_IF_DECLNAME_IS("ImplicitlyUnwrappedOptional", "SQ");
      }
      break;

    default:
      break;

#undef SUCCESS_IF_DECLNAME_IS
#undef SUCCESS_IF_TEXT_IS
#undef SUCCESS_IF_IS
  }
  return false;
}

void Remangler::mangleIdentifier(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::NotOperator);
}
void Remangler::manglePrefixOperator(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::Prefix);
}
void Remangler::manglePostfixOperator(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::Postfix);
}
void Remangler::mangleInfixOperator(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::Infix);
}
void Remangler::mangleIdentifier(StringRef ident, OperatorKind operatorKind) {
  // Mangle normal identifiers as
  //   count identifier-char+
  // where the count is the number of characters in the identifier,
  // and where individual identifier characters represent themselves.
  if (operatorKind == OperatorKind::NotOperator) {
    Buffer << ident.size() << ident;
    return;
  }

  // Mangle operator identifiers as
  //   operator ::= 'o' operator-fixity count operator-char+
  //   operator-fixity ::= 'p' // prefix
  //   operator-fixity ::= 'P' // postfix
  //   operator-fixity ::= 'i' // infix
  // where the count is the number of characters in the operator,
  // and where the individual operator characters are translated.
  Buffer << 'o';
  switch (operatorKind) {
    case OperatorKind::NotOperator: unreachable("invalid");
    case OperatorKind::Infix: Buffer << 'i'; break;
    case OperatorKind::Prefix: Buffer << 'p'; break;
    case OperatorKind::Postfix: Buffer << 'P'; break;
  }

  // Mangle ASCII operators directly.
  Buffer << ident.size();
  for (char ch : ident) {
    Buffer << Mangle::translateOperatorChar(ch);
  }
}

void Remangler::mangleNumber(Node *node) {
  mangleIndex(node->getIndex());
}
void Remangler::mangleIndex(Node::IndexType value) {
  if (value == 0) {
    Buffer << '_';
  } else {
    Buffer << (value - 1) << '_';
  }
}

void Remangler::mangleGlobal(Node *node) {
  Buffer << "_T";
  mangleChildNodes(node);
}

void Remangler::mangleSuffix(Node *node) {
  // Just add the suffix back on.
  Buffer << node->getText();
}

void Remangler::mangleGenericSpecialization(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleGenericSpecializationNotReAbstracted(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleInlinedGenericFunction(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleGenericPartialSpecialization(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleGenericPartialSpecializationNotReAbstracted(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleGenericSpecializationParam(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleFunctionSignatureSpecialization(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleSpecializationPassID(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleIsSerialized(Node *node) {
  Buffer << "q";
}

void Remangler::mangleFunctionSignatureSpecializationReturn(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleFunctionSignatureSpecializationParam(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleFunctionSignatureSpecializationParamPayload(Node *node) {
  // This should never be called since mangling parameter payloads require
  // knowing what the parameter kind is.
  unreachable("This should never be called");
}

void Remangler::mangleFunctionSignatureSpecializationParamKind(Node *node) {
  // This should never be called since mangling parameter kinds have influence
  // on the payloads.
  unreachable("This should never be called");
}

void Remangler::mangleRetroactiveConformance(Node *node) {
  unreachable("Retroactive conformances aren't in the old mangling");
}

void Remangler::mangleProtocolConformanceRefInTypeModule(Node *node) {
  unreachable("Protocol conformance references aren't in the old mangling");
}

void Remangler::mangleProtocolConformanceRefInProtocolModule(Node *node) {
  unreachable("Protocol conformance references aren't in the old mangling");
}

void Remangler::mangleProtocolConformanceRefInOtherModule(Node *node) {
  unreachable("Protocol conformance references aren't in the old mangling");
}

void Remangler::mangleConcreteProtocolConformance(Node *node) {
  unreachable("Concrete conformances aren't in the old mangling");
}

void Remangler::mangleAnyProtocolConformanceList(Node *node) {
  unreachable("Conformance lists aren't in the old mangling");
}

void Remangler::mangleDependentAssociatedConformance(Node *node) {
  unreachable("Dependent associated conformances aren't in the old mangling");
}

void Remangler::mangleDependentProtocolConformanceRoot(Node *node) {
  unreachable("Dependent conformances aren't in the old mangling");
}

void Remangler::mangleDependentProtocolConformanceInherited(Node *node) {
  unreachable("Dependent conformances aren't in the old mangling");
}

void Remangler::mangleDependentProtocolConformanceAssociated(Node *node) {
  unreachable("Dependent conformances aren't in the old mangling");
}

void Remangler::mangleProtocolConformance(Node *node) {
  // type, protocol name, context
  assert(node->getNumChildren() == 3);
  mangleChildNode(node, 0);
  mangleProtocolWithoutPrefix(node->begin()[1]);
  mangleChildNode(node, 2);
}

void Remangler::mangleObjCAttribute(Node *node) {
  Buffer << "To";
}

void Remangler::mangleNonObjCAttribute(Node *node) {
  Buffer << "TO";
}

void Remangler::mangleDirectMethodReferenceAttribute(Node *node) {
  Buffer << "Td";
}

void Remangler::mangleDynamicAttribute(Node *node) {
  Buffer << "TD";
}

void Remangler::mangleVTableAttribute(Node *node) {
  Buffer << "TV";
}

void Remangler::mangleGenericTypeMetadataPattern(Node *node) {
  Buffer << "MP";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleTypeMetadataAccessFunction(Node *node) {
  Buffer << "Ma";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleTypeMetadataInstantiationCache(Node *node) {
  Buffer << "MI";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleTypeMetadataInstantiationFunction(Node *node) {
  Buffer << "Mi";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleTypeMetadataSingletonInitializationCache(Node *node) {
  Buffer << "Ml";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleTypeMetadataCompletionFunction(Node *node) {
  Buffer << "Mr";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleTypeMetadataDemanglingCache(Node *node) {
  unreachable("not supported");
}

void Remangler::mangleTypeMetadataLazyCache(Node *node) {
  Buffer << "ML";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleMetaclass(Node *node) {
  Buffer << "Mm";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleClassMetadataBaseOffset(Node *node) {
  Buffer << "Mo";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleNominalTypeDescriptor(Node *node) {
  Buffer << "Mn";
  mangleSingleChildNode(node); // type
}

void Remangler::manglePropertyDescriptor(Node *node) {
  unreachable("not supported");
}

void Remangler::mangleTypeMetadata(Node *node) {
  Buffer << "M";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleFullTypeMetadata(Node *node) {
  Buffer << "Mf";
  mangleChildNodes(node); // type
}

void Remangler::mangleProtocolDescriptor(Node *node) {
  Buffer << "Mp";
  mangleProtocolWithoutPrefix(node->begin()[0]);
}

void Remangler::mangleProtocolRequirementsBaseDescriptor(Node *node) {
  Buffer << "<protocol-requirements-base-descriptor>";
}

void Remangler::mangleProtocolWitnessTablePattern(Node *node) {
  unreachable("todo");
}

void Remangler::mangleProtocolConformanceDescriptor(Node *node) {
  Buffer << "Mc";
  mangleProtocolConformance(node->begin()[0]);
}

void Remangler::mangleProtocolSelfConformanceDescriptor(Node *node) {
  Buffer << "MS";
  mangleProtocol(node->begin()[0]);
}

void Remangler::manglePartialApplyForwarder(Node *node) {
  Buffer << "PA__T";
  mangleSingleChildNode(node); // global
}

void Remangler::manglePartialApplyObjCForwarder(Node *node) {
  Buffer << "PAo__T";
  mangleSingleChildNode(node); // global
}

void Remangler::mangleMergedFunction(Node *node) {
  Buffer << "Tm";
}

void Remangler::mangleDynamicallyReplaceableFunctionImpl(Node *node) {
  Buffer << "TI";
}

void Remangler::mangleDynamicallyReplaceableFunctionKey(Node *node) {
  Buffer << "Tx";
}

void Remangler::mangleDynamicallyReplaceableFunctionVar(Node *node) {
  Buffer << "TX";
}

void Remangler::mangleDirectness(Node *node) {
  auto getChar = [](Directness d) -> char {
    switch (d) {
    case Directness::Direct: return 'd';
    case Directness::Indirect: return 'i';
    }
    unreachable("bad directness kind");
  };
  Buffer << getChar(Directness(node->getIndex()));
}

void Remangler::mangleValueWitness(Node *node) {
  const char *Code = nullptr;
  switch (ValueWitnessKind(node->getFirstChild()->getIndex())) {
#define VALUE_WITNESS(MANGLING, NAME) \
    case ValueWitnessKind::NAME: Code = #MANGLING; break;
#include "swift/Demangling/ValueWitnessMangling.def"
  }
  Buffer << 'w' << Code;
  mangleChildNode(node, 1); // type
}

void Remangler::mangleValueWitnessTable(Node *node) {
  Buffer << "WV";
  mangleSingleChildNode(node); // type
}

void Remangler::mangleThrowsAnnotation(Node *node) {
  Buffer << "z";
}

void Remangler::mangleFieldOffset(Node *node) {
  Buffer << "Wv";
  mangleChildNodes(node); // directness, entity
}

void Remangler::mangleEnumCase(Node *node) {
  Buffer << "WC";
  mangleSingleChildNode(node); // enum case
}

void Remangler::mangleProtocolSelfConformanceWitnessTable(Node *node) {
  Buffer << "WS";
  mangleSingleChildNode(node); // protocol
}

void Remangler::mangleProtocolWitnessTable(Node *node) {
  Buffer << "WP";
  mangleSingleChildNode(node); // protocol conformance
}

void Remangler::mangleGenericProtocolWitnessTable(Node *node) {
  Buffer << "WG";
  mangleSingleChildNode(node); // protocol conformance
}

void Remangler::mangleResilientProtocolWitnessTable(Node *node) {
  unreachable("todo");
}

void Remangler::mangleGenericProtocolWitnessTableInstantiationFunction(
                                                                  Node *node) {
  Buffer << "WI";
  mangleSingleChildNode(node); // protocol conformance
}

void Remangler::mangleProtocolWitnessTableAccessor(Node *node) {
  Buffer << "Wa";
  mangleSingleChildNode(node); // protocol conformance
}

void Remangler::mangleLazyProtocolWitnessTableAccessor(Node *node) {
  Buffer << "Wl";
  mangleChildNodes(node); // type, protocol conformance
}

void Remangler::mangleLazyProtocolWitnessTableCacheVariable(Node *node) {
  Buffer << "WL";
  mangleChildNodes(node); // type, protocol conformance
}

void Remangler::mangleAssociatedTypeDescriptor(Node *node) {
  Buffer << "<associated-type-descriptor>";
}

void Remangler::mangleAssociatedConformanceDescriptor(Node *node) {
  Buffer << "<associated-conformance-descriptor>";
}

void Remangler::mangleDefaultAssociatedConformanceAccessor(Node *node) {
  Buffer << "<default-associated-conformance-descriptor>";
}

void Remangler::mangleBaseConformanceDescriptor(Node *node) {
  Buffer << "<base-conformance-descriptor>";
}

void Remangler::mangleAssociatedTypeMetadataAccessor(Node *node) {
  Buffer << "Wt";
  mangleChildNodes(node); // protocol conformance, identifier
}

void Remangler::mangleDefaultAssociatedTypeMetadataAccessor(Node *node) {
  Buffer << "<default-associated-type-metadata-accessor>";
}

void Remangler::mangleAssociatedTypeWitnessTableAccessor(Node *node) {
  Buffer << "WT";
  assert(node->getNumChildren() == 3);
  mangleChildNode(node, 0); // protocol conformance
  mangleChildNode(node, 1); // type
  mangleProtocolWithoutPrefix(node->begin()[2]); // type
}

void Remangler::mangleBaseWitnessTableAccessor(Node *node) {
  Buffer << "<base-witness-table-accessor>";
}

void Remangler::mangleReabstractionThunkHelper(Node *node) {
  Buffer << "<reabstraction-thunk-helper>";
}

void Remangler::mangleReabstractionThunkHelperWithSelf(Node *node) {
  Buffer << "<reabstraction-thunk-helper-with-self>";
}

void Remangler::mangleReabstractionThunk(Node *node) {
  Buffer << "<reabstraction-thunk>";
}

void Remangler::mangleProtocolSelfConformanceWitness(Node *node) {
  Buffer << "TS";
  mangleSingleChildNode(node); // entity
}

void Remangler::mangleProtocolWitness(Node *node) {
  Buffer << "TW";
  mangleChildNodes(node); // protocol conformance, entity
}

void Remangler::mangleFunction(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "", ctx);
}

void Remangler::mangleVariable(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'v', "", ctx);
}

void Remangler::mangleSubscript(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'i', "", ctx);
}

void Remangler::mangleAccessor(Node *storageNode, StringRef accessorCode,
                               EntityContext &ctx) {
  Buffer << 'F';
  mangleEntityContext(storageNode->getChild(0), ctx);
  Buffer << accessorCode;

  auto mangleAccessorType = [&](unsigned TypeIndex) {
    auto LabelList = storageNode->getChild(TypeIndex);
    if (LabelList->getKind() == Node::Kind::LabelList) {
      auto Type = storageNode->getChild(TypeIndex + 1);
      mangleEntityType(applyParamLabels(LabelList, Type, Factory), ctx);
    } else {
      mangleEntityType(storageNode->getChild(TypeIndex), ctx);
    }
  };

  switch (storageNode->getKind()) {
  case Demangle::Node::Kind::Variable: {
    mangleChildNode(storageNode, 1);
    mangleAccessorType(2);
    break;
  }

  case Demangle::Node::Kind::Subscript: {
    auto NumChildren = storageNode->getNumChildren();
    assert(NumChildren <= 4);

    auto PrivateName = storageNode->getChild(NumChildren - 1);
    if (PrivateName->getKind() == Node::Kind::PrivateDeclName)
      mangle(PrivateName);

    mangleIdentifier("subscript", OperatorKind::NotOperator);
    mangleAccessorType(1);
    break;
  }
  default:
      unreachable("Not a storage node");
  }
}

void Remangler::mangleInitializer(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'I', "i", ctx);
}

void Remangler::manglePropertyWrapperBackingInitializer(Node *node,
                                                        EntityContext &ctx) {
  mangleSimpleEntity(node, 'I', "P", ctx);
}

void Remangler::mangleDefaultArgumentInitializer(Node *node,
                                                 EntityContext &ctx) {
  mangleNamedEntity(node, 'I', "A", ctx);
}

void Remangler::mangleDeallocator(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'F', "D", ctx);
}

void Remangler::mangleDestructor(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'F', "d", ctx);
}

void Remangler::mangleAllocator(Node *node, EntityContext &ctx) {
  mangleTypedEntity(node, 'F', "C", ctx);
}

void Remangler::mangleConstructor(Node *node, EntityContext &ctx) {
  mangleTypedEntity(node, 'F', "c", ctx);
}

void Remangler::mangleIVarInitializer(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'F', "e", ctx);
}

void Remangler::mangleIVarDestroyer(Node *node, EntityContext &ctx) {
  mangleSimpleEntity(node, 'F', "E", ctx);
}

void Remangler::mangleGetter(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "g", ctx);
}

void Remangler::mangleGlobalGetter(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "G", ctx);
}

void Remangler::mangleSetter(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "s", ctx);
}

void Remangler::mangleMaterializeForSet(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "m", ctx);
}

void Remangler::mangleWillSet(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "w", ctx);
}

void Remangler::mangleDidSet(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "W", ctx);
}

void Remangler::mangleOwningMutableAddressor(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "aO", ctx);
}

void Remangler::mangleNativeOwningMutableAddressor(Node *node,
                                                   EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "ao", ctx);
}

void Remangler::mangleNativePinningMutableAddressor(Node *node,
                                                    EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "ap", ctx);
}

void Remangler::mangleUnsafeMutableAddressor(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "au", ctx);
}

void Remangler::mangleOwningAddressor(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "lO", ctx);
}

void Remangler::mangleNativeOwningAddressor(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "lo", ctx);
}

void Remangler::mangleNativePinningAddressor(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "lp", ctx);
}

void Remangler::mangleUnsafeAddressor(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "lu", ctx);
}

void Remangler::mangleReadAccessor(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "r", ctx);
}

void Remangler::mangleModifyAccessor(Node *node, EntityContext &ctx) {
  mangleAccessor(node->getFirstChild(), "M", ctx);
}

void Remangler::mangleExplicitClosure(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "U", ctx); // name is index
}

void Remangler::mangleImplicitClosure(Node *node, EntityContext &ctx) {
  mangleNamedAndTypedEntity(node, 'F', "u", ctx); // name is index
}

void Remangler::mangleStatic(Node *node, EntityContext &ctx) {
  Buffer << 'Z';
  mangleEntityContext(node->getChild(0), ctx);
}

void Remangler::mangleSimpleEntity(Node *node, char basicKind,
                                   StringRef entityKind,
                                   EntityContext &ctx) {
  assert(node->getNumChildren() == 1);
  Buffer << basicKind;
  mangleEntityContext(node->begin()[0], ctx);
  Buffer << entityKind;
}

void Remangler::mangleNamedEntity(Node *node, char basicKind,
                                  StringRef entityKind,
                                  EntityContext &ctx,
                                  StringRef artificialPrivateDiscriminator) {
  assert(node->getNumChildren() == 2);
  if (basicKind != '\0') Buffer << basicKind;
  mangleEntityContext(node->begin()[0], ctx);
  Buffer << entityKind;
  
  auto privateDiscriminator = ctx.takeAnonymousContextDiscriminator();
  if (!privateDiscriminator.empty() && isdigit(privateDiscriminator[0]))
    privateDiscriminator = "_" + privateDiscriminator;
  if (!artificialPrivateDiscriminator.empty())
    privateDiscriminator.append(artificialPrivateDiscriminator.data(),
                                artificialPrivateDiscriminator.size());
  
  // Include the artificial private discriminator if one was given.
  auto name = node->getChild(1);
  if (!privateDiscriminator.empty()
      && name->getKind() == Node::Kind::Identifier) {
    Buffer << 'P';
    mangleIdentifier(privateDiscriminator, OperatorKind::NotOperator);
  }
  mangle(name);
}

void Remangler::mangleTypedEntity(Node *node, char basicKind,
                                  StringRef entityKind,
                                  EntityContext &ctx) {
  assert(node->getNumChildren() == 2 || node->getNumChildren() == 3);
  Buffer << basicKind;
  mangleEntityContext(node->begin()[0], ctx);
  Buffer << entityKind;

  if (node->begin()[1]->getKind() == Node::Kind::LabelList) {
    auto LabelList = node->begin()[1];
    auto Type = node->begin()[2];
    mangleEntityType(applyParamLabels(LabelList, Type, Factory), ctx);
  } else {
    mangleEntityType(node->begin()[1], ctx);
  }
}

void Remangler::mangleNamedAndTypedEntity(Node *node, char basicKind,
                                          StringRef entityKind,
                                          EntityContext &ctx) {
  assert(node->getNumChildren() == 3 || node->getNumChildren() == 4);
  Buffer << basicKind;
  mangleEntityContext(node->begin()[0], ctx);
  Buffer << entityKind;
  mangleChildNode(node, 1); // decl name / index

  if (node->begin()[2]->getKind() == Node::Kind::LabelList) {
    auto LabelList = node->begin()[2];
    auto Type = node->begin()[3];
    mangleEntityType(applyParamLabels(LabelList, Type, Factory), ctx);
  } else {
    mangleEntityType(node->begin()[2], ctx);
  }
}

void Remangler::mangleEntityContext(Node *node, EntityContext &ctx) {
  // Remember that we're mangling a context.
  EntityContext::ManglingContextRAII raii(ctx);

  // Deal with bound generic types.
  switch (node->getKind()) {
    case Node::Kind::BoundGenericStructure:
    case Node::Kind::BoundGenericEnum:
    case Node::Kind::BoundGenericClass:
    case Node::Kind::BoundGenericOtherNominalType:
    case Node::Kind::BoundGenericTypeAlias:
      mangleAnyNominalType(node, ctx);
      return;

    default:
      break;
  }

  switch (node->getKind()) {
#define NODE(ID)                                \
  case Node::Kind::ID:
#define CONTEXT_NODE(ID)
#include "swift/Demangling/DemangleNodes.def"
    unreachable("not a context node");

#define NODE(ID)
#define CONTEXT_NODE(ID)                        \
  case Node::Kind::ID:                          \
    return mangle##ID(node, ctx);
#include "swift/Demangling/DemangleNodes.def"
  }
  unreachable("bad node kind");
}

void Remangler::mangleEntityType(Node *node, EntityContext &ctx) {
  assert(node->getKind() == Node::Kind::Type);
  assert(node->getNumChildren() == 1);
  node = node->begin()[0];

  // Expand certain kinds of type within the entity context.
  switch (node->getKind()) {
  case Node::Kind::NoEscapeFunctionType:
  case Node::Kind::FunctionType:
  case Node::Kind::UncurriedFunctionType: {
    Buffer << ((node->getKind() == Node::Kind::FunctionType ||
             node->getKind() == Node::Kind::NoEscapeFunctionType)
                ? 'F'
                : 'f');
    unsigned inputIndex = node->getNumChildren() - 2;
    assert(inputIndex <= 1);
    for (unsigned i = 0; i <= inputIndex; ++i)
      mangle(node->begin()[i]);
    auto returnType = node->begin()[inputIndex+1];
    assert(returnType->getKind() == Node::Kind::ReturnType);
    assert(returnType->getNumChildren() == 1);
    mangleEntityType(returnType->begin()[0], ctx);
    return;
  }
  default:
    mangle(node);
    return;
  }
}

void Remangler::mangleLocalDeclName(Node *node) {
  Buffer << 'L';
  mangleChildNodes(node); // index, identifier
}

void Remangler::manglePrivateDeclName(Node *node) {
  Buffer << 'P';
  mangleChildNodes(node); // identifier, identifier
}

void Remangler::mangleRelatedEntityDeclName(Node *node) {
  // Non-round-trip mangling: pretend we have a private discriminator "$A" for a
  // related entity "A".
  NodePointer kindNode = node->getFirstChild();
  Buffer << 'P' << (kindNode->getText().size() + 1) << '$' << kindNode->getText();
  mangleChildNode(node, 1);
}

void Remangler::mangleTypeMangling(Node *node) {
  Buffer << 't';
  mangleSingleChildNode(node); // type
}

void Remangler::mangleType(Node *node) {
  mangleSingleChildNode(node);
}

template <size_t N> 
static bool stripPrefix(StringRef &string, const char (&data)[N]) {
  constexpr size_t prefixLength = N - 1;
  if (!string.startswith(StringRef(data, prefixLength)))
    return false;
  string = string.drop_front(prefixLength);
  return true;
}

void Remangler::mangleBuiltinTypeName(Node *node) {
  Buffer << 'B';
  StringRef text = node->getText();

  if (text == "Builtin.BridgeObject") {
    Buffer << 'b';
  } else if (text == "Builtin.UnsafeValueBuffer") {
    Buffer << 'B';
  } else if (text == "Builtin.UnknownObject") {
    Buffer << 'O';
  } else if (text == "Builtin.NativeObject") {
    Buffer << 'o';
  } else if (text == "Builtin.RawPointer") {
    Buffer << 'p';
  } else if (text == "Builtin.Word") {
    Buffer << 'w';
  } else if (stripPrefix(text, "Builtin.Int")) {
    Buffer << 'i' << text << '_';
  } else if (stripPrefix(text, "Builtin.FPIEEE")) {
    Buffer << 'f' << text << '_';
  } else if (stripPrefix(text, "Builtin.Vec")) {
    // Avoid using StringRef::split because its definition is not
    // provided in the header so that it requires linking with libSupport.a.
    size_t splitIdx = text.find('x');
    Buffer << 'v' << text.substr(0, splitIdx) << 'B';
    auto element = text.substr(splitIdx).substr(1);
    if (element == "RawPointer") {
      Buffer << 'p';
    } else if (stripPrefix(element, "Float")) {
      Buffer << 'f' << element << '_';
    } else if (stripPrefix(element, "Int")) {
      Buffer << 'i' << element << '_';
    } else {
      unreachable("unexpected builtin vector type");
    }
  } else {
    unreachable("unexpected builtin type");
  }
}

void Remangler::mangleTypeAlias(Node *node, EntityContext &ctx) {
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleFunctionType(Node *node) {
  Buffer << 'F';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleUncurriedFunctionType(Node *node) {
  Buffer << 'f';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleObjCBlock(Node *node) {
  Buffer << 'b';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleEscapingObjCBlock(Node *node) {
  // We shouldn't ever be remangling anything with a DWARF-only mangling.
  Buffer << "<escaping block type>";
}

void Remangler::mangleCFunctionPointer(Node *node) {
  Buffer << 'c';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleAutoClosureType(Node *node) {
  Buffer << 'K';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleNoEscapeFunctionType(Node *node) {
  Buffer << 'F';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleEscapingAutoClosureType(Node *node) {
  Buffer << 'K';
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleThinFunctionType(Node *node) {
  Buffer << "Xf";
  mangleChildNodes(node); // argument tuple, result type
}

void Remangler::mangleArgumentTuple(Node *node) {
  mangleSingleChildNode(node);
}

void Remangler::mangleReturnType(Node *node) {
  mangleSingleChildNode(node);
}

void Remangler::mangleImplFunctionType(Node *node) {
  Buffer << "XF";
  auto i = node->begin(), e = node->end();
  if (i != e && (*i)->getKind() == Node::Kind::ImplConvention) {
    StringRef text = (*i)->getText();
    i++;
    if (text == "@callee_unowned") {
      Buffer << 'd';
    } else if (text == "@callee_guaranteed") {
      Buffer << 'g';
    } else if (text == "@callee_owned") {
      Buffer << 'o';
    } else {
      unreachable("bad callee convention");
    }
  } else {
    Buffer << 't';
  }
  for (; i != e &&
         (*i)->getKind() == Node::Kind::ImplFunctionAttribute; ++i) {
    mangle(*i); // impl function attribute
  }
  if (i != e &&
      ((*i)->getKind() == Node::Kind::DependentGenericSignature ||
       (*i)->getKind() == Node::Kind::DependentPseudogenericSignature)) {
    Buffer << ((*i)->getKind() == Node::Kind::DependentGenericSignature
              ? 'G' : 'g');
    mangleDependentGenericSignature((*i));
    i++;
  }
  Buffer << '_';
  for (; i != e && (*i)->getKind() == Node::Kind::ImplParameter; ++i) {
    mangleImplParameter(*i);
  }
  Buffer << '_';
  mangleNodes(i, e); // impl results
  Buffer << '_';
}

void Remangler::mangleImplFunctionAttribute(Node *node) {
  StringRef text = node->getText();
  if (text == "@convention(block)") {
    Buffer << "Cb";
  } else if (text == "@convention(c)") {
    Buffer << "Cc";
  } else if (text == "@convention(method)") {
    Buffer << "Cm";
  } else if (text == "@convention(objc_method)") {
    Buffer << "CO";
  } else if (text == "@convention(witness_method)") {
    Buffer << "Cw";
  } else {
    unreachable("bad impl-function-attribute");
  }
}

void Remangler::mangleImplParameter(Node *node) {
  assert(node->getNumChildren() == 2);
  mangleChildNodes(node); // impl convention, type
}

void Remangler::mangleImplErrorResult(Node *node) {
  assert(node->getNumChildren() == 2);
  Buffer << 'z';
  mangleChildNodes(node); // impl convention, type
}

void Remangler::mangleImplResult(Node *node) {
  assert(node->getNumChildren() == 2);
  mangleChildNodes(node); // impl convention, type
}

void Remangler::mangleImplEscaping(Node *node) {
  // The old mangler does not encode escaping.
}

void Remangler::mangleImplSubstitutions(Node *node) {
  // The old mangler does not encode substituted function types.
}

void Remangler::mangleImplImpliedSubstitutions(Node *node) {
  // The old mangler does not encode substituted function types.
}

void Remangler::mangleImplConvention(Node *node) {
  assert(node->getKind() == Node::Kind::ImplConvention);
  StringRef text = node->getText();
  if (text == "@autoreleased") {
    Buffer << 'a';
  } else if (text == "@unowned") {
    Buffer << 'd';
  } else if (text == "@unowned_inner_pointer") {
    Buffer << 'D'; // only in results
  } else if (text == "@guaranteed") {
    Buffer << 'g';
  } else if (text == "@deallocating") {
    Buffer << 'e';
  } else if (text == "@in") {
    Buffer << 'i'; // only in parameters
  } else if (text == "@out") {
    Buffer << 'i'; // only in results
  } else if (text == "@inout") {
    Buffer << 'l';
  } else if (text == "@owned") {
    Buffer << 'o';
  } else {
    unreachable("invalid impl convention");
  }
}

void Remangler::mangleDynamicSelf(Node *node) {
  Buffer << 'D';
  mangleSingleChildNode(node); // type
}

void Remangler::mangleErrorType(Node *node) {
  Buffer << "ERR";
}

void Remangler::mangleSILBoxType(Node *node) {
  Buffer << 'X' << 'b';
  mangleSingleChildNode(node);
}

void Remangler::mangleMetatype(Node *node) {
  if (node->getNumChildren() == 1) {
    Buffer << 'M';
    mangleSingleChildNode(node); // type
  } else {
    assert(node->getNumChildren() == 2);
    Buffer << "XM";
    mangleChildNodes(node); // metatype representation, type
  }
}

void Remangler::mangleExistentialMetatype(Node *node) {
  if (node->getNumChildren() == 1) {
    Buffer << "PM";
    mangleSingleChildNode(node); // type
  } else {
    assert(node->getNumChildren() == 2);
    Buffer << "XPM";
    mangleChildNodes(node); // metatype representation, type
  }
}

void Remangler::mangleMetatypeRepresentation(Node *node) {
  StringRef text = node->getText();
  if (text == "@thin") {
    Buffer << 't';
  } else if (text == "@thick") {
    Buffer << 'T';
  } else if (text == "@objc_metatype") {
    Buffer << 'o';
  } else {
    unreachable("bad metatype representation");
  }
}

void Remangler::mangleProtocolList(Node *node) {
  // In its usual use as a type, this gets a prefix 'P'.
  Buffer << 'P';
  mangleProtocolListWithoutPrefix(node);
}

void Remangler::mangleProtocolListWithoutPrefix(Node *node,
                                                Node *additionalProto) {
  assert(node->getKind() == Node::Kind::ProtocolList);
  assert(node->getNumChildren() == 1);
  auto typeList = node->begin()[0];
  assert(typeList->getKind() == Node::Kind::TypeList);
  for (auto &child : *typeList) {
    mangleProtocolWithoutPrefix(child);
  }
  if (additionalProto) {
    mangleProtocolWithoutPrefix(additionalProto);
  }
  Buffer << '_';
}

#define REF_STORAGE(Name, ...) \
  void Remangler::mangle##Name(Node *node) { \
    Buffer << manglingOf(ReferenceOwnership::Name); \
    mangleSingleChildNode(node); /* type */ \
  }
#include "swift/AST/ReferenceStorage.def"

void Remangler::mangleShared(Node *node) {
  Buffer << 'h';
  mangleSingleChildNode(node); // type
}

void Remangler::mangleOwned(Node *node) {
  Buffer << 'n';
  mangleSingleChildNode(node); // type
}

void Remangler::mangleInOut(Node *node) {
  Buffer << 'R';
  mangleSingleChildNode(node); // type
}

void Remangler::mangleTuple(Node *node) {
  size_t NumElems = node->getNumChildren();
  if (NumElems > 0 &&
      node->getChild(NumElems - 1)->getFirstChild()->getKind() ==
      Node::Kind::VariadicMarker) {
    Buffer << 't';
  } else {
    Buffer << 'T';
  }
  mangleChildNodes(node); // tuple elements
  Buffer << '_';
}

void Remangler::mangleTupleElement(Node *node) {
  mangleChildNodes(node); // tuple element name?, type
}

void Remangler::mangleTupleElementName(Node *node) {
  mangleIdentifier(node->getText(), OperatorKind::NotOperator);
}

void Remangler::mangleDependentGenericType(Node *node) {
  Buffer << 'u';
  mangleChildNodes(node); // generic signature, type
}

void Remangler::mangleDependentPseudogenericSignature(Node *node) {
  mangleDependentGenericSignature(node);
}

void Remangler::mangleDependentGenericSignature(Node *node) {
  auto i = node->begin(), e = node->end();
  
  // If there's only one generic param, mangle nothing.
  if (node->getNumChildren() >= 1
      && node->getChild(0)->getKind() == Node::Kind::DependentGenericParamCount
      && node->getChild(0)->getIndex() == 1
      && (node->getNumChildren() == 1
          || node->getChild(1)->getKind() != Node::Kind::DependentGenericParamCount))
  {
    ++i;
    goto mangle_requirements;
  }
  
  // Remangle generic params.
  for (; i != e &&
         (*i)->getKind() == Node::Kind::DependentGenericParamCount; ++i) {
    auto count = *i;
    if (count->getIndex() > 0)
      mangleIndex(count->getIndex() - 1);
    else
      Buffer << 'z';
  }
  
mangle_requirements:
  if (i == e) { // no generic requirements
    Buffer << 'r';
    return;
  }
  
  Buffer << 'R';
  mangleNodes(i, e); // generic requirements
  Buffer << 'r';
}

void Remangler::mangleDependentGenericParamCount(Node *node) {
  unreachable("handled inline in DependentGenericSignature");
}

void Remangler::mangleDependentGenericConformanceRequirement(Node *node) {
  mangleConstrainedType(node->getChild(0));
  // If the constraint represents a protocol, use the shorter mangling.
  if (node->getNumChildren() == 2
      && node->getChild(1)->getKind() == Node::Kind::Type
      && node->getChild(1)->getNumChildren() == 1
      && node->getChild(1)->getChild(0)->getKind() == Node::Kind::Protocol) {
    mangleProtocolWithoutPrefix(node->getChild(1)->getChild(0));
    return;
  }

  mangle(node->getChild(1));
}

void Remangler::mangleDependentGenericSameTypeRequirement(Node *node) {
  mangleConstrainedType(node->getChild(0));
  Buffer << 'z';
  mangle(node->getChild(1));
}

void Remangler::mangleDependentGenericLayoutRequirement(Node *node) {
  mangleConstrainedType(node->getChild(0));
  Buffer << 'l';
  auto id =  node->getChild(1)->getText();
  auto size = -1;
  if (node->getNumChildren() > 2) {
    size = node->getChild(2)->getIndex();
  }
  int alignment = -1;
  if (node->getNumChildren() > 3) {
    alignment = node->getChild(3)->getIndex();
  }
  Buffer << id;
  if (size >= 0)
    Buffer << size;
  if (alignment >= 0) {
    Buffer << "_" << alignment;
  }
}

void Remangler::mangleConstrainedType(Node *node) {
  if (node->getFirstChild()->getKind()
        == Node::Kind::DependentGenericParamType) {
    // Can be mangled without an introducer.
    mangleDependentGenericParamIndex(node->getFirstChild());
  } else {
    mangle(node);
  }
}

void Remangler::mangleAssociatedType(Node *node) {
  if (node->hasChildren()) {
    assert(node->getNumChildren() == 1);
    mangleProtocolListWithoutPrefix(*node->begin());
  } else {
    Buffer << '_';
  }
}

void Remangler::mangleDeclContext(Node *node) {
  mangleSingleChildNode(node);
}

void Remangler::mangleExtension(Node *node, EntityContext &ctx) {
  assert(node->getNumChildren() == 2 || node->getNumChildren() == 3);
  if (node->getNumChildren() == 3) {
    Buffer << 'e';
  } else {
    Buffer << 'E';
  }
  mangleEntityContext(node->begin()[0], ctx); // module
  if (node->getNumChildren() == 3) {
    mangleDependentGenericSignature(node->begin()[2]); // generic sig
  }

  mangleEntityContext(node->begin()[1], ctx); // context
}

void Remangler::mangleAnonymousContext(Node *node, EntityContext &ctx) {
  mangleEntityContext(node->getChild(1), ctx);

  // Since we can't change the old mangling, mangle an anonymous context by
  // introducing a private discriminator onto its child contexts.
  ctx.setAnonymousContextDiscriminator(node->getChild(0)->getText());
}

void Remangler::mangleModule(Node *node, EntityContext &ctx) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;

  // Module types get an M prefix, but module contexts don't.
  if (!ctx.isAsContext()) Buffer << 'M';
  mangleIdentifier(node->getText(), OperatorKind::NotOperator);
  addSubstitution(entry);
}

void Remangler::mangleAssociatedTypeRef(Node *node) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;
  Buffer << "Q";
  mangleChildNodes(node); // type, identifier
  addSubstitution(entry);
}

void Remangler::mangleDependentMemberType(Node *node) {
  Vector<Node *> members;
  Node *base = node;
  do {
    members.push_back(base, Factory);
    base = base->getFirstChild()->getFirstChild();
  } while (base->getKind() == Node::Kind::DependentMemberType);

  assert(base->getKind() == Node::Kind::DependentGenericParamType
         && "dependent members not based on a generic param are non-canonical"
            " and shouldn't need remangling");
  assert(members.size() >= 1);
  if (members.size() == 1) {
    Buffer << 'w';
    mangleDependentGenericParamIndex(base);
    mangle(members[0]->getChild(1));
  } else {
    Buffer << 'W';
    mangleDependentGenericParamIndex(base);

    for (unsigned i = 1, n = members.size(); i <= n; ++i) {
      Node *member = members[n - i];
      mangle(member->getChild(1));
    }
    Buffer << '_';
  }
}

void Remangler::mangleDependentAssociatedTypeRef(Node *node) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;

  if (node->getNumChildren() > 1) {
    Buffer << 'P';
    mangleProtocolWithoutPrefix(node->getChild(1));
  }
  mangleIdentifier(node->getFirstChild());

  addSubstitution(entry);
}

void Remangler::mangleDependentGenericParamIndex(Node *node) {
  auto depth = node->getChild(0)->getIndex();
  auto index = node->getChild(1)->getIndex();

  if (depth != 0) {
    Buffer << 'd';
    mangleIndex(depth - 1);
    mangleIndex(index);
    return;
  }
  if (index != 0) {
    mangleIndex(index - 1);
    return;
  }

  // depth == index == 0
  Buffer << 'x';
}

void Remangler::mangleDependentGenericParamType(Node *node) {
  if (node->getChild(0)->getIndex() == 0
      && node->getChild(1)->getIndex() == 0) {
    Buffer << 'x';
    return;
  }

  Buffer << 'q';
  mangleDependentGenericParamIndex(node);
}

void Remangler::mangleIndex(Node *node) {
  mangleIndex(node->getIndex());
}

void Remangler::mangleUnknownIndex(Node *node) {
  unreachable("should not be reached in an arbitrary context");
}

void Remangler::mangleProtocol(Node *node, EntityContext &ctx) {
  mangleNominalType(node, 'P', ctx);
}

void Remangler::mangleProtocolWithoutPrefix(Node *node) {
  if (mangleStandardSubstitution(node))
    return;

  if (node->getKind() == Node::Kind::Type) {
    assert(node->getNumChildren() == 1);
    node = node->begin()[0];
  }

  assert(node->getKind() == Node::Kind::Protocol);
  EntityContext ctx;
  mangleNominalType(node, '\0', ctx);
}

void Remangler::mangleGenericArgs(Node *node, EntityContext &ctx) {
  switch (node->getKind()) {
  case Node::Kind::Structure:
  case Node::Kind::Enum:
  case Node::Kind::Class: {
    NodePointer parentOrModule = node->getChild(0);
    mangleGenericArgs(parentOrModule, ctx);

    // No generic arguments at this level
    Buffer << '_';
    break;
  }

  case Node::Kind::BoundGenericStructure:
  case Node::Kind::BoundGenericEnum:
  case Node::Kind::BoundGenericClass: {
    NodePointer unboundType = node->getChild(0);
    assert(unboundType->getKind() == Node::Kind::Type);
    NodePointer nominalType = unboundType->getChild(0);
    NodePointer parentOrModule = nominalType->getChild(0);
    mangleGenericArgs(parentOrModule, ctx);

    mangleTypeList(node->getChild(1));
    break;
  }

  case Node::Kind::AnonymousContext:
  case Node::Kind::Extension: {
    mangleGenericArgs(node->getChild(1), ctx);
    break;
  }

  default:
    break;
  }
}

void Remangler::mangleAnyNominalType(Node *node, EntityContext &ctx) {
  if (isSpecialized(node)) {
    Buffer << 'G';

    NodePointer unboundType = getUnspecialized(node, Factory);

    mangleAnyNominalType(unboundType, ctx);
    mangleGenericArgs(node, ctx);
    return;
  }

  switch (node->getKind()) {
  case Node::Kind::OtherNominalType:
    // Mangle unknown type kinds as structures since we can't change the old
    // mangling. Give the mangling an artificial "private discriminator" so that
    // clients who understand the old mangling know this is an unstable
    // mangled name.
    mangleNominalType(node, 'V', ctx, "_UnknownTypeKind");
    break;
  case Node::Kind::Structure:
    mangleNominalType(node, 'V', ctx);
    break;
  case Node::Kind::Enum:
    mangleNominalType(node, 'O', ctx);
    break;
  case Node::Kind::Class:
    mangleNominalType(node, 'C', ctx);
    break;
  case Node::Kind::TypeAlias:
    mangleNominalType(node, 'a', ctx);
    break;
  default:
    unreachable("bad nominal type kind");
  }
}

void Remangler::mangleStructure(Node *node, EntityContext &ctx) {
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleEnum(Node *node, EntityContext &ctx) {
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleClass(Node *node, EntityContext &ctx) {
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleOtherNominalType(Node *node, EntityContext &ctx) {
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleNominalType(Node *node, char kind, EntityContext &ctx,
                                  StringRef artificialPrivateDiscriminator) {
  SubstitutionEntry entry;
  if (trySubstitution(node, entry)) return;
  mangleNamedEntity(node, kind, "", ctx, artificialPrivateDiscriminator);
  addSubstitution(entry);
}

void Remangler::mangleBoundGenericClass(Node *node) {
  EntityContext ctx;
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleBoundGenericStructure(Node *node) {
  EntityContext ctx;
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleBoundGenericEnum(Node *node) {
  EntityContext ctx;
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleBoundGenericOtherNominalType(Node *node) {
  EntityContext ctx;
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleBoundGenericProtocol(Node *node) {
  EntityContext ctx;
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleBoundGenericTypeAlias(Node *node) {
  EntityContext ctx;
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleBoundGenericFunction(Node *node) {
  EntityContext ctx;
  // Not really a nominal type, but it works for functions, too.
  mangleAnyNominalType(node, ctx);
}

void Remangler::mangleTypeList(Node *node) {
  mangleChildNodes(node); // all types
  Buffer << '_';
}

void Remangler::mangleLabelList(Node *node) {
  if (node->getNumChildren() == 0)
    Buffer << 'y';
  else
    mangleChildNodes(node);
}

void Remangler::mangleReflectionMetadataBuiltinDescriptor(Node *node) {
  Buffer << "MRb";
}

void Remangler::mangleReflectionMetadataFieldDescriptor(Node *node) {
  Buffer << "MRf";
}

void Remangler::mangleReflectionMetadataAssocTypeDescriptor(Node *node) {
  Buffer << "MRa";
}

void Remangler::mangleReflectionMetadataSuperclassDescriptor(Node *node) {
  Buffer << "MRc";
}

void Remangler::mangleGenericTypeParamDecl(Node *node) {
  unreachable("todo");
}

void Remangler::mangleCurryThunk(Node *node) {
  Buffer << "<curry-thunk>";
}

void Remangler::mangleDispatchThunk(Node *node) {
  Buffer << "<dispatch-thunk>";
}

void Remangler::mangleMethodDescriptor(Node *node) {
  Buffer << "<method-descriptor>";
}

void Remangler::mangleMethodLookupFunction(Node *node) {
  Buffer << "<method-lookup-function>";
}

void Remangler::mangleObjCMetadataUpdateFunction(Node *node) {
  Buffer << "<objc-metadata-update-function>";
}

void Remangler::mangleObjCResilientClassStub(Node *node) {
  Buffer << "<objc-resilient-class-stub>";
}

void Remangler::mangleFullObjCResilientClassStub(Node *node) {
  Buffer << "<full-objc-resilient-class-stub>";
}

void Remangler::mangleEmptyList(Node *node) {
  Buffer << "<empty>";
}

void Remangler::mangleFirstElementMarker(Node *node) {
  Buffer << "<first>";
}

void Remangler::mangleVariadicMarker(Node *node) {
  // Handled in mangleTuple
}

void Remangler::mangleOutlinedCopy(Node *node) {
  Buffer << "Wy";
  mangleChildNodes(node);
}

void Remangler::mangleOutlinedConsume(Node *node) {
  Buffer << "We";
  mangleChildNodes(node);
}

void Remangler::mangleOutlinedRetain(Node *node) {
  Buffer << "Wr";
  mangleSingleChildNode(node);
}

void Remangler::mangleOutlinedRelease(Node *node) {
  Buffer << "Ws";
  mangleSingleChildNode(node);
}

void Remangler::mangleOutlinedInitializeWithTake(Node *node) {
  Buffer << "Wb";
  mangleSingleChildNode(node);
}

void Remangler::mangleOutlinedInitializeWithCopy(Node *node) {
  Buffer << "Wc";
  mangleSingleChildNode(node);
}

void Remangler::mangleOutlinedAssignWithTake(Node *node) {
  Buffer << "Wd";
  mangleSingleChildNode(node);
}

void Remangler::mangleOutlinedAssignWithCopy(Node *node) {
  Buffer << "Wf";
  mangleSingleChildNode(node);
}

void Remangler::mangleOutlinedDestroy(Node *node) {
  Buffer << "Wh";
  mangleSingleChildNode(node);
}

void Remangler::mangleOutlinedVariable(Node *node) {
  Buffer << "Tv" << node->getIndex();
  mangleSingleChildNode(node);
}

void Remangler::mangleOutlinedBridgedMethod(Node *node) {
  Buffer << "Te" << node->getText();
  mangleSingleChildNode(node);
}

void Remangler::mangleCoroutineContinuationPrototype(Node *node) {
  Buffer << "TC";
  mangleChildNodes(node);
}

void Remangler::mangleKeyPathGetterThunkHelper(Node *node) {
  Buffer << "TK";
  mangleChildNodes(node);
}

void Remangler::mangleKeyPathSetterThunkHelper(Node *node) {
  Buffer << "Tk";
  mangleChildNodes(node);
}

void Remangler::mangleKeyPathEqualsThunkHelper(Node *node) {
  Buffer << "TH";
  mangleChildNodes(node);
}

void Remangler::mangleKeyPathHashThunkHelper(Node *node) {
  Buffer << "Th";
  mangleChildNodes(node);
}

void Remangler::mangleProtocolListWithClass(Node *node) {
  Buffer << "Xc";
  mangleChildNode(node, 1);
  mangleProtocolListWithoutPrefix(node->getChild(0));
}

void Remangler::mangleProtocolListWithAnyObject(Node *node) {
  Node *P = Factory.createNode(Node::Kind::Protocol);
  P->addChild(Factory.createNode(Node::Kind::Module, "Swift"), Factory);
  P->addChild(Factory.createNode(Node::Kind::Identifier, "AnyObject"), Factory);
  Buffer << "P";
  mangleProtocolListWithoutPrefix(node->getChild(0), /*additionalProto*/ P);
}

void Remangler::mangleVTableThunk(Node *node) {
  Buffer << "TV";
  mangleChildNodes(node);
}

void Remangler::mangleSILBoxTypeWithLayout(Node *node) {
  assert(node->getKind() == Node::Kind::SILBoxTypeWithLayout);
  assert(node->getNumChildren() == 1 || node->getNumChildren() == 3);
  Buffer << "XB";
  auto layout = node->getChild(0);
  assert(layout->getKind() == Node::Kind::SILBoxLayout);
  NodePointer genericArgs = nullptr;
  if (node->getNumChildren() == 3) {
    NodePointer signature = node->getChild(1);
    assert(signature->getKind() == Node::Kind::DependentGenericSignature);
    genericArgs = node->getChild(2);
    assert(genericArgs->getKind() == Node::Kind::TypeList);
    
    Buffer << 'G';
    mangleDependentGenericSignature(signature);
  }
  mangleSILBoxLayout(layout);
  if (genericArgs) {
    for (unsigned i = 0; i < genericArgs->getNumChildren(); ++i) {
      auto type = genericArgs->getChild(i);
      assert(genericArgs->getKind() == Node::Kind::Type);
      mangleType(type);
    }
    Buffer << '_';
  }
}

void Remangler::mangleSILBoxLayout(Node *node) {
  assert(node->getKind() == Node::Kind::SILBoxLayout);
  for (unsigned i = 0; i < node->getNumChildren(); ++i) {
    assert(node->getKind() == Node::Kind::SILBoxImmutableField
           || node->getKind() == Node::Kind::SILBoxMutableField);
    mangle(node->getChild(i));
    
  }
  Buffer << '_';
}

void Remangler::mangleSILBoxMutableField(Node *node) {
  Buffer << 'm';
  assert(node->getNumChildren() == 1
         && node->getChild(0)->getKind() == Node::Kind::Type);
  mangleType(node->getChild(0));
}

void Remangler::mangleSILBoxImmutableField(Node *node) {
  Buffer << 'i';
  assert(node->getNumChildren() == 1
         && node->getChild(0)->getKind() == Node::Kind::Type);
  mangleType(node->getChild(0));
}

void Remangler::mangleAssocTypePath(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleModuleDescriptor(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleExtensionDescriptor(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleAnonymousDescriptor(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleAssociatedTypeGenericParamRef(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleTypeSymbolicReference(Node *node, EntityContext&) {
  unreachable("unsupported");
}

void Remangler::mangleProtocolSymbolicReference(Node *node, EntityContext&) {
  unreachable("unsupported");
}

void Remangler::mangleOpaqueTypeDescriptorSymbolicReference(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleSugaredOptional(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleSugaredArray(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleSugaredDictionary(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleSugaredParen(Node *node) {
  unreachable("unsupported");
}

void Remangler::mangleOpaqueReturnType(Node *node) {
  unreachable("unsupported");
}
void Remangler::mangleOpaqueReturnTypeOf(Node *node, EntityContext &ctx) {
  unreachable("unsupported");
}
void Remangler::mangleOpaqueType(Node *node) {
  unreachable("unsupported");
}
void Remangler::mangleOpaqueTypeDescriptor(Node *node) {
  unreachable("unsupported");
}
void Remangler::mangleOpaqueTypeDescriptorAccessor(Node *node) {
  unreachable("unsupported");
}
void Remangler::mangleOpaqueTypeDescriptorAccessorImpl(Node *node) {
  unreachable("unsupported");
}
void Remangler::mangleOpaqueTypeDescriptorAccessorKey(Node *node) {
  unreachable("unsupported");
}
void Remangler::mangleOpaqueTypeDescriptorAccessorVar(Node *node) {
  unreachable("unsupported");
}
void Remangler::mangleAccessorFunctionReference(Node *node) {
  unreachable("can't remangle");
}

/// The top-level interface to the remangler.
std::string Demangle::mangleNodeOld(NodePointer node) {
  if (!node) return "";

  NodeFactory Factory;
  Remangler remangler(Factory);
  remangler.mangle(node);

  return remangler.str();
}

llvm::StringRef Demangle::mangleNodeOld(NodePointer node, NodeFactory &Factory) {
  if (!node) return "";

  Remangler remangler(Factory);
  remangler.mangle(node);

  return remangler.getBufferStr();
}

const char *Demangle::mangleNodeAsObjcCString(NodePointer node,
                                              NodeFactory &Factory) {
  assert(node);

  Remangler remangler(Factory);
  remangler.append("_Tt");
  remangler.mangle(node);
  remangler.append(StringRef("_", 2)); // Include the trailing 0 char.

  return remangler.getBufferStr().data();
}
