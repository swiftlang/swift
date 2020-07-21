//===--- TypeRef.cpp - Swift Type References for Reflection ---------------===//
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
// Implements the structures of type references for property and enum
// case reflection.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Range.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"

using namespace swift;
using namespace reflection;

class PrintTypeRef : public TypeRefVisitor<PrintTypeRef, void> {
  FILE *file;
  unsigned Indent;

  FILE * &indent(unsigned Amount) {
    for (unsigned i = 0; i < Amount; ++i)
      fprintf(file, " ");
    return file;
  }

  FILE * &printHeader(std::string Name) {
    fprintf(indent(Indent), "(%s", Name.c_str());
    return file;
  }

  FILE * &printField(std::string name, std::string value) {
    if (!name.empty())
      fprintf(file, " %s=%s", name.c_str(), value.c_str());
    else
      fprintf(file, " %s", value.c_str());
    return file;
  }

  void printRec(const TypeRef *typeRef) {
    fprintf(file, "\n");

    Indent += 2;
    visit(typeRef);
    Indent -= 2;
  }

public:
  PrintTypeRef(FILE *file, unsigned Indent)
    : file(file), Indent(Indent) {}

  void visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    printHeader("builtin");
    auto demangled = Demangle::demangleTypeAsString(B->getMangledName());
    printField("", demangled);
    fprintf(file, ")");
  }

  void visitNominalTypeRef(const NominalTypeRef *N) {
    StringRef mangledName = N->getMangledName();
    if (N->isStruct())
      printHeader("struct");
    else if (N->isEnum())
      printHeader("enum");
    else if (N->isClass())
      printHeader("class");
    else if (N->isProtocol()) {
      printHeader("protocol");
      mangledName = Demangle::dropSwiftManglingPrefix(mangledName);
    }
    else if (N->isAlias())
      printHeader("alias");
    else
      printHeader("nominal");
    auto demangled = Demangle::demangleTypeAsString(mangledName);
    printField("", demangled);
    if (auto parent = N->getParent())
      printRec(parent);
    fprintf(file, ")");
  }

  void visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    if (BG->isStruct())
      printHeader("bound_generic_struct");
    else if (BG->isEnum())
      printHeader("bound_generic_enum");
    else if (BG->isClass())
      printHeader("bound_generic_class");
    else
      printHeader("bound_generic");

    auto demangled = Demangle::demangleTypeAsString(BG->getMangledName());
    printField("", demangled);
    for (auto param : BG->getGenericParams())
      printRec(param);
    if (auto parent = BG->getParent())
      printRec(parent);
    fprintf(file, ")");
  }

  void visitTupleTypeRef(const TupleTypeRef *T) {
    printHeader("tuple");
    for (auto element : T->getElements())
      printRec(element);
    fprintf(file, ")");
  }

  void visitFunctionTypeRef(const FunctionTypeRef *F) {
    printHeader("function");

    switch (F->getFlags().getConvention()) {
    case FunctionMetadataConvention::Swift:
      break;
    case FunctionMetadataConvention::Block:
      printField("convention", "block");
      break;
    case FunctionMetadataConvention::Thin:
      printField("convention", "thin");
      break;
    case FunctionMetadataConvention::CFunctionPointer:
      printField("convention", "c");
      break;
    }

    fprintf(file, "\n");
    Indent += 2;
    printHeader("parameters");

    auto &parameters = F->getParameters();
    for (const auto &param : parameters) {
      auto flags = param.getFlags();

      if (!flags.isNone()) {
        Indent += 2;
        fprintf(file, "\n");
      }

      switch (flags.getValueOwnership()) {
      case ValueOwnership::Default:
        /* nothing */
        break;
      case ValueOwnership::InOut:
        printHeader("inout");
        break;
      case ValueOwnership::Shared:
        printHeader("shared");
        break;
      case ValueOwnership::Owned:
        printHeader("owned");
        break;
      }

      if (flags.isVariadic())
        printHeader("variadic");

      printRec(param.getType());

      if (!flags.isNone()) {
        Indent -= 2;
        fprintf(file, ")");
      }
    }

    if (parameters.empty())
      fprintf(file, ")");

    fprintf(file, "\n");
    printHeader("result");
    printRec(F->getResult());
    fprintf(file, ")");

    Indent -= 2;
  }

  void visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    printHeader("protocol_composition");
    if (PC->hasExplicitAnyObject())
      fprintf(file, " any_object");
    if (auto superclass = PC->getSuperclass())
      printRec(superclass);
    for (auto protocol : PC->getProtocols())
      printRec(protocol);
    fprintf(file, ")");
  }

  void visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    printHeader("metatype");
    if (M->wasAbstract())
      printField("", "was_abstract");
    printRec(M->getInstanceType());
    fprintf(file, ")");
  }

  void visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    printHeader("existential_metatype");
    printRec(EM->getInstanceType());
    fprintf(file, ")");
  }

  void visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP){
    printHeader("generic_type_parameter");
    printField("depth", std::to_string(GTP->getDepth()));
    printField("index", std::to_string(GTP->getIndex()));
    fprintf(file, ")");
  }

  void visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    printHeader("dependent_member");
    printField("protocol", DM->getProtocol());
    printRec(DM->getBase());
    printField("member", DM->getMember());
    fprintf(file, ")");
  }

  void visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    printHeader("foreign");
    if (!F->getName().empty())
      printField("name", F->getName());
    fprintf(file, ")");
  }

  void visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    printHeader("objective_c_class");
    if (!OC->getName().empty())
      printField("name", OC->getName());
    fprintf(file, ")");
  }

  void visitObjCProtocolTypeRef(const ObjCProtocolTypeRef *OC) {
    printHeader("objective_c_protocol");
    if (!OC->getName().empty())
      printField("name", OC->getName());
    fprintf(file, ")");
  }

#define REF_STORAGE(Name, name, ...) \
  void visit##Name##StorageTypeRef(const Name##StorageTypeRef *US) { \
    printHeader(#name "_storage"); \
    printRec(US->getType()); \
    fprintf(file, ")"); \
  }
#include "swift/AST/ReferenceStorage.def"

  void visitSILBoxTypeRef(const SILBoxTypeRef *SB) {
    printHeader("sil_box");
    printRec(SB->getBoxedType());
    fprintf(file, ")");
  }

  void visitOpaqueArchetypeTypeRef(const OpaqueArchetypeTypeRef *O) {
    printHeader("opaque_archetype");
    printField("id", O->getID().str());
    printField("description", O->getDescription().str());
    fprintf(file, " ordinal %u ", O->getOrdinal());
    for (auto argList : O->getArgumentLists()) {
      fprintf(file, "\n");
      fprintf(indent(Indent + 2), "args: <");
      for (auto arg : argList) {
        printRec(arg);
      }
      fprintf(file, ">");
    }
    fprintf(file, ")");
  }

  void visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    printHeader("opaque");
    fprintf(file, ")");
  }
};

struct TypeRefIsConcrete
  : public TypeRefVisitor<TypeRefIsConcrete, bool> {
  const GenericArgumentMap &Subs;

  TypeRefIsConcrete(const GenericArgumentMap &Subs) : Subs(Subs) {}

  bool visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return true;
  }

  bool visitNominalTypeRef(const NominalTypeRef *N) {
    if (N->getParent())
      return visit(N->getParent());
    return true;
  }

  bool visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    if (BG->getParent())
      if (!visit(BG->getParent()))
        return false;
    for (auto Param : BG->getGenericParams())
      if (!visit(Param))
        return false;
    return true;
  }

  bool visitTupleTypeRef(const TupleTypeRef *T) {
    for (auto Element : T->getElements()) {
      if (!visit(Element))
        return false;
    }
    return true;
  }

  bool visitFunctionTypeRef(const FunctionTypeRef *F) {
    for (const auto &Param : F->getParameters())
      if (!visit(Param.getType()))
        return false;
    return visit(F->getResult());
  }

  bool
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    for (auto Protocol : PC->getProtocols())
      if (!visit(Protocol))
        return false;
    if (auto Superclass = PC->getSuperclass())
      if (!visit(Superclass))
        return false;
    return true;
  }

  bool visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    return visit(M->getInstanceType());
  }

  bool
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    return visit(EM->getInstanceType());
  }

  bool
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP) {
    return Subs.find({GTP->getDepth(), GTP->getIndex()}) != Subs.end();
  }

  bool
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    return visit(DM->getBase());
  }

  bool visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return true;
  }

  bool visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    return true;
  }

  bool visitObjCProtocolTypeRef(const ObjCProtocolTypeRef *OC) {
    return true;
  }
  
  bool visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    return true;
  }
    
  bool visitOpaqueArchetypeTypeRef(const OpaqueArchetypeTypeRef *O) {
    return false;
  }

#define REF_STORAGE(Name, name, ...) \
  bool visit##Name##StorageTypeRef(const Name##StorageTypeRef *US) { \
    return visit(US->getType()); \
  }
#include "swift/AST/ReferenceStorage.def"

  bool visitSILBoxTypeRef(const SILBoxTypeRef *SB) {
    return visit(SB->getBoxedType());
  }
};

const OpaqueTypeRef *
OpaqueTypeRef::Singleton = new OpaqueTypeRef();

const OpaqueTypeRef *OpaqueTypeRef::get() {
  return Singleton;
}

void TypeRef::dump() const {
  dump(stderr);
}

void TypeRef::dump(FILE *file, unsigned Indent) const {
  PrintTypeRef(file, Indent).visit(this);
  fprintf(file, "\n");
}

class DemanglingForTypeRef
    : public TypeRefVisitor<DemanglingForTypeRef, Demangle::NodePointer> {
  Demangle::Demangler &Dem;

public:
  DemanglingForTypeRef(Demangle::Demangler &Dem) : Dem(Dem) {}

  Demangle::NodePointer visit(const TypeRef *typeRef) {
    auto node = TypeRefVisitor<DemanglingForTypeRef,
                                Demangle::NodePointer>::visit(typeRef);

    // Wrap all nodes in a Type node, as consumers generally expect.
    auto typeNode = Dem.createNode(Node::Kind::Type);
    typeNode->addChild(node, Dem);
    return typeNode;
  }

  Demangle::NodePointer visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return Dem.demangleType(B->getMangledName());
  }

  Demangle::NodePointer visitNominalTypeRef(const NominalTypeRef *N) {
    if (auto parent = N->getParent())
      assert(false && "not implemented");
    return Dem.demangleType(N->getMangledName());
  }

  Demangle::NodePointer
  visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    Node::Kind nodeKind;
    Node::Kind genericNodeKind;
    if (BG->isStruct()) {
      nodeKind = Node::Kind::Structure;
      genericNodeKind = Node::Kind::BoundGenericStructure;
    } else if (BG->isEnum()) {
      nodeKind = Node::Kind::Enum;
      genericNodeKind = Node::Kind::BoundGenericEnum;
    } else if (BG->isClass()) {
      nodeKind = Node::Kind::Class;
      genericNodeKind = Node::Kind::BoundGenericClass;
    } else {
      nodeKind = Node::Kind::OtherNominalType;
      genericNodeKind = Node::Kind::BoundGenericOtherNominalType;
    }
    auto unspecializedType = Dem.demangleType(BG->getMangledName());

    auto genericArgsList = Dem.createNode(Node::Kind::TypeList);
    for (auto param : BG->getGenericParams())
      genericArgsList->addChild(visit(param), Dem);

    auto genericNode = Dem.createNode(genericNodeKind);
    genericNode->addChild(unspecializedType, Dem);
    genericNode->addChild(genericArgsList, Dem);

    if (auto parent = BG->getParent())
      assert(false && "not implemented");

    return genericNode;
  }

  Demangle::NodePointer visitTupleTypeRef(const TupleTypeRef *T) {
    auto tuple = Dem.createNode(Node::Kind::Tuple);
    if (T->isVariadic()) {
      auto tupleElt = Dem.createNode(Node::Kind::TupleElement);
      tupleElt->addChild(Dem.createNode(Node::Kind::VariadicMarker), Dem);
      tuple->addChild(tupleElt, Dem);
      return tuple;
    }

    for (auto element : T->getElements()) {
      auto tupleElt = Dem.createNode(Node::Kind::TupleElement);
      tupleElt->addChild(visit(element), Dem);
      tuple->addChild(tupleElt, Dem);
    }
    return tuple;
  }

  Demangle::NodePointer visitFunctionTypeRef(const FunctionTypeRef *F) {
    Node::Kind kind;
    switch (F->getFlags().getConvention()) {
    case FunctionMetadataConvention::Swift:
      kind = !F->getFlags().isEscaping() ? Node::Kind::NoEscapeFunctionType
                                         : Node::Kind::FunctionType;
      break;
    case FunctionMetadataConvention::Block:
      kind = Node::Kind::ObjCBlock;
      break;
    case FunctionMetadataConvention::Thin:
      kind = Node::Kind::ThinFunctionType;
      break;
    case FunctionMetadataConvention::CFunctionPointer:
      kind = Node::Kind::CFunctionPointer;
      break;
    }

    SmallVector<std::pair<NodePointer, bool>, 8> inputs;
    for (const auto &param : F->getParameters()) {
      auto flags = param.getFlags();
      auto input = visit(param.getType());

      auto wrapInput = [&](Node::Kind kind) {
        auto parent = Dem.createNode(kind);
        parent->addChild(input, Dem);
        input = parent;
      };
      switch (flags.getValueOwnership()) {
      case ValueOwnership::Default:
        /* nothing */
        break;
      case ValueOwnership::InOut:
        wrapInput(Node::Kind::InOut);
        break;
      case ValueOwnership::Shared:
        wrapInput(Node::Kind::Shared);
        break;
      case ValueOwnership::Owned:
        wrapInput(Node::Kind::Owned);
        break;
      }

      inputs.push_back({input, flags.isVariadic()});
    }
    NodePointer totalInput = nullptr;
    // FIXME: this is copy&paste from Demangle.cpp
    switch (inputs.size()) {
    case 1: {
      auto singleParam = inputs.front();

      // If the sole unlabeled parameter has a non-tuple type, encode
      // the parameter list as a single type.
      if (!singleParam.second) {
        auto singleType = singleParam.first;
        if (singleType->getKind() == Node::Kind::Type)
          singleType = singleType->getFirstChild();
        if (singleType->getKind() != Node::Kind::Tuple) {
          totalInput = singleParam.first;
          break;
        }
      }

      // Otherwise it requires a tuple wrapper.
      LLVM_FALLTHROUGH;
    }

    // This covers both none and multiple parameters.
    default:
      auto tuple = Dem.createNode(Node::Kind::Tuple);
      for (auto &input : inputs) {
        NodePointer eltType;
        bool isVariadic;
        std::tie(eltType, isVariadic) = input;

        // Tuple element := variadic-marker label? type
        auto tupleElt = Dem.createNode(Node::Kind::TupleElement);

        if (isVariadic)
          tupleElt->addChild(Dem.createNode(Node::Kind::VariadicMarker), Dem);

        if (eltType->getKind() == Node::Kind::Type) {
          tupleElt->addChild(eltType, Dem);
        } else {
          auto type = Dem.createNode(Node::Kind::Type);
          type->addChild(eltType, Dem);
          tupleElt->addChild(type, Dem);
        }

        tuple->addChild(tupleElt, Dem);
      }
      totalInput = tuple;
      break;
    }

    NodePointer parameters = Dem.createNode(Node::Kind::ArgumentTuple);
    NodePointer paramType = Dem.createNode(Node::Kind::Type);

    paramType->addChild(totalInput, Dem);
    parameters->addChild(paramType, Dem);

    NodePointer resultTy = visit(F->getResult());
    NodePointer result = Dem.createNode(Node::Kind::ReturnType);
    result->addChild(resultTy, Dem);

    auto funcNode = Dem.createNode(kind);
    if (F->getFlags().throws())
      funcNode->addChild(Dem.createNode(Node::Kind::ThrowsAnnotation), Dem);
    funcNode->addChild(parameters, Dem);
    funcNode->addChild(result, Dem);
    return funcNode;
  }

  Demangle::NodePointer
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    auto type_list = Dem.createNode(Node::Kind::TypeList);
    for (auto protocol : PC->getProtocols())
      type_list->addChild(visit(protocol), Dem);

    auto proto_list = Dem.createNode(Node::Kind::ProtocolList);
    proto_list->addChild(type_list, Dem);

    auto node = proto_list;
    if (auto superclass = PC->getSuperclass()) {
      node = Dem.createNode(Node::Kind::ProtocolListWithClass);
      node->addChild(proto_list, Dem);
      node->addChild(visit(superclass), Dem);
    } else if (PC->hasExplicitAnyObject()) {
      node = Dem.createNode(Node::Kind::ProtocolListWithAnyObject);
      node->addChild(proto_list, Dem);
    }
    return node;
  }

  Demangle::NodePointer visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    auto node = Dem.createNode(Node::Kind::Metatype);
    assert(!M->wasAbstract() && "not implemented");
    node->addChild(visit(M->getInstanceType()), Dem);
    return node;
  }

  Demangle::NodePointer
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    auto node = Dem.createNode(Node::Kind::Metatype);
    node->addChild(visit(EM->getInstanceType()), Dem);
    return node;
  }

  Demangle::NodePointer
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP) {
    assert(false && "not tested");
    auto node = Dem.createNode(Node::Kind::DependentGenericParamType);
    node->addChild(Dem.createNode(Node::Kind::Index, GTP->getDepth()), Dem);
    node->addChild(Dem.createNode(Node::Kind::Index, GTP->getIndex()), Dem);
    return node;
  }

  Demangle::NodePointer
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    assert(false && "not tested");
    assert(DM->getProtocol().empty() && "not implemented");
    auto node = Dem.createNode(Node::Kind::DependentMemberType);
    node->addChild(visit(DM->getBase()), Dem);
    node->addChild(Dem.createNode(Node::Kind::Identifier, DM->getMember()),
                   Dem);
    return node;
  }

  Demangle::NodePointer visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return Dem.demangleType(F->getName());
  }

  Demangle::NodePointer visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    auto module = Dem.createNode(Node::Kind::Module, MANGLING_MODULE_OBJC);
    auto node = Dem.createNode(Node::Kind::Class);
    node->addChild(module, Dem);
    node->addChild(Dem.createNode(Node::Kind::Identifier, OC->getName()), Dem);
    return node;
  }

  Demangle::NodePointer
  visitObjCProtocolTypeRef(const ObjCProtocolTypeRef *OC) {
    auto module = Dem.createNode(Node::Kind::Module, MANGLING_MODULE_OBJC);
    auto node = Dem.createNode(Node::Kind::Protocol);
    node->addChild(module, Dem);
    node->addChild(Dem.createNode(Node::Kind::Identifier, OC->getName()), Dem);
    return node;
  }

#define REF_STORAGE(Name, name, ...)                                           \
  Demangle::NodePointer visit##Name##StorageTypeRef(                           \
      const Name##StorageTypeRef *US) {                                        \
    auto node = Dem.createNode(Node::Kind::Name);                              \
    node->addChild(visit(US->getType()), Dem);                                 \
    return node;                                                               \
  }
#include "swift/AST/ReferenceStorage.def"

  Demangle::NodePointer visitSILBoxTypeRef(const SILBoxTypeRef *SB) {
    auto node = Dem.createNode(Node::Kind::SILBoxType);
    node->addChild(visit(SB->getBoxedType()), Dem);
    return node;
  }

  Demangle::NodePointer visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    return Dem.createNode(Node::Kind::OpaqueType);
  }
      
  Demangle::NodePointer visitOpaqueArchetypeTypeRef(const OpaqueArchetypeTypeRef *O) {
    auto decl = Dem.demangleSymbol(O->getID());
    if (!decl)
      return nullptr;
    
    auto index = Dem.createNode(Node::Kind::Index, O->getOrdinal());
    
    auto argNodeLists = Dem.createNode(Node::Kind::TypeList);
    for (auto argList : O->getArgumentLists()) {
      auto argNodeList = Dem.createNode(Node::Kind::TypeList);
      
      for (auto arg : argList) {
        auto argNode = visit(arg);
        if (!argNode)
          return nullptr;
        
        argNodeList->addChild(argNode, Dem);
      }
      
      argNodeLists->addChild(argNodeList, Dem);
    }
    
    auto node = Dem.createNode(Node::Kind::OpaqueType);
    node->addChild(decl, Dem);
    node->addChild(index, Dem);
    node->addChild(argNodeLists, Dem);
    
    return node;
  }
};

Demangle::NodePointer TypeRef::getDemangling(Demangle::Demangler &Dem) const {
  return DemanglingForTypeRef(Dem).visit(this);
}

bool TypeRef::isConcrete() const {
  GenericArgumentMap Subs;
  return TypeRefIsConcrete(Subs).visit(this);
}

bool TypeRef::isConcreteAfterSubstitutions(
    const GenericArgumentMap &Subs) const {
  return TypeRefIsConcrete(Subs).visit(this);
}

unsigned NominalTypeTrait::getDepth() const {
  if (auto P = Parent) {
    switch (P->getKind()) {
    case TypeRefKind::Nominal:
      return 1 + cast<NominalTypeRef>(P)->getDepth();
    case TypeRefKind::BoundGeneric:
      return 1 + cast<BoundGenericTypeRef>(P)->getDepth();
    default:
      break;
    }
  }
  return 0;
}

llvm::Optional<GenericArgumentMap> TypeRef::getSubstMap() const {
  GenericArgumentMap Substitutions;
  switch (getKind()) {
    case TypeRefKind::Nominal: {
      auto Nom = cast<NominalTypeRef>(this);
      if (auto Parent = Nom->getParent())
        return Parent->getSubstMap();
      return GenericArgumentMap();
    }
    case TypeRefKind::BoundGeneric: {
      auto BG = cast<BoundGenericTypeRef>(this);
      auto Depth = BG->getDepth();
      unsigned Index = 0;
      for (auto Param : BG->getGenericParams()) {
        if (!Param->isConcrete())
          return None;
        Substitutions.insert({{Depth, Index++}, Param});
      }
      if (auto Parent = BG->getParent()) {
        auto ParentSubs = Parent->getSubstMap();
        if (!ParentSubs)
          return None;
        Substitutions.insert(ParentSubs->begin(), ParentSubs->end());
      }
      break;
    }
    default:
      break;
  }
  return Substitutions;
}

bool NominalTypeTrait::isStruct() const {
  return Demangle::isStruct(MangledName);
}

bool NominalTypeTrait::isEnum() const { return Demangle::isEnum(MangledName); }

bool NominalTypeTrait::isClass() const {
  return Demangle::isClass(MangledName);
}

bool NominalTypeTrait::isProtocol() const {
  return Demangle::isProtocol(MangledName);
}

bool NominalTypeTrait::isAlias() const {
  return Demangle::isAlias(MangledName);
}

/// Visitor class to set the WasAbstract flag of any MetatypeTypeRefs
/// contained in the given type.
class ThickenMetatype
  : public TypeRefVisitor<ThickenMetatype, const TypeRef *> {
  TypeRefBuilder &Builder;
public:
  using TypeRefVisitor<ThickenMetatype, const TypeRef *>::visit;

  ThickenMetatype(TypeRefBuilder &Builder) : Builder(Builder) {}

  const TypeRef *visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return B;
  }

  const TypeRef *visitNominalTypeRef(const NominalTypeRef *N) {
    return N;
  }

  const TypeRef *visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    std::vector<const TypeRef *> GenericParams;
    for (auto Param : BG->getGenericParams())
      GenericParams.push_back(visit(Param));
    return BoundGenericTypeRef::create(Builder, BG->getMangledName(),
                                       GenericParams);
  }

  const TypeRef *visitTupleTypeRef(const TupleTypeRef *T) {
    std::vector<const TypeRef *> Elements;
    for (auto Element : T->getElements())
      Elements.push_back(visit(Element));
    return TupleTypeRef::create(Builder, Elements);
  }

  const TypeRef *visitFunctionTypeRef(const FunctionTypeRef *F) {
    std::vector<remote::FunctionParam<const TypeRef *>> SubstitutedParams;
    for (const auto &Param : F->getParameters()) {
      auto typeRef = Param.getType();
      SubstitutedParams.push_back(Param.withType(visit(typeRef)));
    }

    auto SubstitutedResult = visit(F->getResult());

    return FunctionTypeRef::create(Builder, SubstitutedParams,
                                   SubstitutedResult, F->getFlags());
  }

  const TypeRef *
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    return PC;
  }

  const TypeRef *visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    return MetatypeTypeRef::create(Builder, visit(M->getInstanceType()),
                                   /*WasAbstract=*/true);
  }

  const TypeRef *
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    return EM;
  }

  const TypeRef *
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP) {
    return GTP;
  }

  const TypeRef *visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    return DM;
  }

  const TypeRef *visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return F;
  }

  const TypeRef *visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    return OC;
  }

  const TypeRef *visitObjCProtocolTypeRef(const ObjCProtocolTypeRef *OP) {
    return OP;
  }

#define REF_STORAGE(Name, name, ...) \
  const TypeRef *visit##Name##StorageTypeRef(const Name##StorageTypeRef *US) { \
    return US; \
  }
#include "swift/AST/ReferenceStorage.def"

  const TypeRef *visitSILBoxTypeRef(const SILBoxTypeRef *SB) {
    return SILBoxTypeRef::create(Builder, visit(SB->getBoxedType()));
  }

  const TypeRef *visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    return O;
  }

  const TypeRef *visitOpaqueArchetypeTypeRef(const OpaqueArchetypeTypeRef *O) {
    return O;
  }

};

static const TypeRef *
thickenMetatypes(TypeRefBuilder &Builder, const TypeRef *TR) {
  return ThickenMetatype(Builder).visit(TR);
}

class TypeRefSubstitution
  : public TypeRefVisitor<TypeRefSubstitution, const TypeRef *> {
  TypeRefBuilder &Builder;
  GenericArgumentMap Substitutions;
public:
  using TypeRefVisitor<TypeRefSubstitution, const TypeRef *>::visit;

  TypeRefSubstitution(TypeRefBuilder &Builder, GenericArgumentMap Substitutions)
    : Builder(Builder), Substitutions(Substitutions) {}

  const TypeRef *visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return B;
  }

  const TypeRef *visitNominalTypeRef(const NominalTypeRef *N) {
    if (N->getParent())
      return NominalTypeRef::create(Builder, N->getMangledName(),
                                    visit(N->getParent()));
    return N;
  }

  const TypeRef *visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    auto *Parent = BG->getParent();
    if (Parent != nullptr)
      Parent = visit(Parent);
    std::vector<const TypeRef *> GenericParams;
    for (auto Param : BG->getGenericParams())
      GenericParams.push_back(visit(Param));
    return BoundGenericTypeRef::create(Builder, BG->getMangledName(),
                                       GenericParams, Parent);
  }

  const TypeRef *visitTupleTypeRef(const TupleTypeRef *T) {
    std::vector<const TypeRef *> Elements;
    for (auto Element : T->getElements())
      Elements.push_back(visit(Element));
    return TupleTypeRef::create(Builder, Elements);
  }

  const TypeRef *visitFunctionTypeRef(const FunctionTypeRef *F) {
    std::vector<remote::FunctionParam<const TypeRef *>> SubstitutedParams;
    for (const auto &Param : F->getParameters()) {
      auto typeRef = Param.getType();
      SubstitutedParams.push_back(Param.withType(visit(typeRef)));
    }

    auto SubstitutedResult = visit(F->getResult());

    return FunctionTypeRef::create(Builder, SubstitutedParams,
                                   SubstitutedResult, F->getFlags());
  }

  const TypeRef *
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    return PC;
  }

  const TypeRef *visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    // If the metatype's instance type does not contain any type parameters,
    // substitution does not alter anything, and the empty representation
    // can still be used.
    if (M->isConcrete())
      return M;

    // When substituting a concrete type into a type parameter inside
    // of a metatype's instance type, (eg; T.Type, T := C), we must
    // represent the metatype at runtime as a value, even if the
    // metatype naturally has an empty representation.
    return MetatypeTypeRef::create(Builder, visit(M->getInstanceType()),
                                   /*WasAbstract=*/true);
  }

  const TypeRef *
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    // Existential metatypes do not contain type parameters.
    assert(EM->getInstanceType()->isConcrete());
    return EM;
  }

  const TypeRef *
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP) {
    auto found = Substitutions.find({GTP->getDepth(), GTP->getIndex()});
    if (found == Substitutions.end())
      return GTP;
    assert(found->second->isConcrete());

    // When substituting a concrete type containing a metatype into a
    // type parameter, (eg: T, T := C.Type), we must also represent
    // the metatype as a value.
    return thickenMetatypes(Builder, found->second);
  }

  const TypeRef *visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    // Substitute type parameters in the base type to get a fully concrete
    // type.
    auto SubstBase = visit(DM->getBase());

    const TypeRef *TypeWitness = nullptr;

    while (TypeWitness == nullptr) {
      auto &Member = DM->getMember();
      const auto &Protocol = DM->getProtocol();

      // Get the original type of the witness from the conformance.
      if (auto *Nominal = dyn_cast<NominalTypeRef>(SubstBase)) {
        TypeWitness = Builder.lookupTypeWitness(Nominal->getMangledName(),
                                                Member, Protocol);
      } else if (auto *BG = dyn_cast<BoundGenericTypeRef>(SubstBase)) {
        TypeWitness = Builder.lookupTypeWitness(BG->getMangledName(),
                                                Member, Protocol);
      }

      if (TypeWitness != nullptr)
        break;

      // If we didn't find the member type, check the superclass.
      auto *Superclass = Builder.lookupSuperclass(SubstBase);
      if (Superclass == nullptr)
        break;

      SubstBase = Superclass;
    }

    auto Protocol = std::make_pair(DM->getProtocol(), false);

    // We didn't find the member type, so return something to let the
    // caller know we're dealing with incomplete metadata.
    if (TypeWitness == nullptr)
      return Builder.createDependentMemberType(DM->getMember(),
                                               SubstBase,
                                               Protocol);

    // Likewise if we can't get the substitution map.
    auto SubstMap = SubstBase->getSubstMap();
    if (!SubstMap)
      return Builder.createDependentMemberType(DM->getMember(),
                                               SubstBase,
                                               Protocol);

    // Apply base type substitutions to get the fully-substituted nested type.
    auto *Subst = TypeWitness->subst(Builder, *SubstMap);

    // Same as above.
    return thickenMetatypes(Builder, Subst);
  }

  const TypeRef *visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return F;
  }

  const TypeRef *visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    return OC;
  }

  const TypeRef *visitObjCProtocolTypeRef(const ObjCProtocolTypeRef *OP) {
    return OP;
  }

#define REF_STORAGE(Name, name, ...) \
  const TypeRef *visit##Name##StorageTypeRef(const Name##StorageTypeRef *US) { \
    return Name##StorageTypeRef::create(Builder, visit(US->getType())); \
  }
#include "swift/AST/ReferenceStorage.def"

  const TypeRef *visitSILBoxTypeRef(const SILBoxTypeRef *SB) {
    return SILBoxTypeRef::create(Builder, visit(SB->getBoxedType()));
  }

  const TypeRef *visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    return O;
  }
    
  const TypeRef *visitOpaqueArchetypeTypeRef(const OpaqueArchetypeTypeRef *O) {
    std::vector<const TypeRef *> newArgsBuffer;
    for (auto argList : O->getArgumentLists()) {
      for (auto arg : argList) {
        newArgsBuffer.push_back(visit(arg));
      }
    }
    
    std::vector<ArrayRef<const TypeRef *>> newArgLists;
    
    return OpaqueArchetypeTypeRef::create(Builder, O->getID(), O->getDescription(),
                                          O->getOrdinal(),
                                          newArgLists);
  }
};

const TypeRef *
TypeRef::subst(TypeRefBuilder &Builder, const GenericArgumentMap &Subs) const {
  return TypeRefSubstitution(Builder, Subs).visit(this);
}

bool TypeRef::deriveSubstitutions(GenericArgumentMap &Subs,
                                  const TypeRef *OrigTR,
                                  const TypeRef *SubstTR) {

  // Walk into parent types of concrete nominal types.
  if (auto *O = dyn_cast<NominalTypeRef>(OrigTR)) {
    if (auto *S = dyn_cast<NominalTypeRef>(SubstTR)) {
      if (!!O->getParent() != !!S->getParent() ||
          O->getMangledName() != S->getMangledName())
        return false;

      if (O->getParent() &&
          !deriveSubstitutions(Subs,
                               O->getParent(),
                               S->getParent()))
        return false;

      return true;
    }
  }

  // Decompose arguments of bound generic types in parallel.
  if (auto *O = dyn_cast<BoundGenericTypeRef>(OrigTR)) {
    if (auto *S = dyn_cast<BoundGenericTypeRef>(SubstTR)) {
      if (!!O->getParent() != !!S->getParent() ||
          O->getMangledName() != S->getMangledName() ||
          O->getGenericParams().size() != S->getGenericParams().size())
        return false;

      if (O->getParent() &&
          !deriveSubstitutions(Subs,
                               O->getParent(),
                               S->getParent()))
        return false;

      for (unsigned i = 0, e = O->getGenericParams().size(); i < e; i++) {
        if (!deriveSubstitutions(Subs,
                                 O->getGenericParams()[i],
                                 S->getGenericParams()[i]))
          return false;
      }

      return true;
    }
  }

  // Decompose tuple element types in parallel.
  if (auto *O = dyn_cast<TupleTypeRef>(OrigTR)) {
    if (auto *S = dyn_cast<TupleTypeRef>(SubstTR)) {
      if (O->getElements().size() != S->getElements().size())
        return false;

      for (unsigned i = 0, e = O->getElements().size(); i < e; i++) {
        if (!deriveSubstitutions(Subs,
                                 O->getElements()[i],
                                 S->getElements()[i]))
          return false;
      }

      return true;
    }
  }

  // Decompose parameter and result types in parallel.
  if (auto *O = dyn_cast<FunctionTypeRef>(OrigTR)) {
    if (auto *S = dyn_cast<FunctionTypeRef>(SubstTR)) {
      auto oParams = O->getParameters();
      auto sParams = S->getParameters();

      if (oParams.size() != sParams.size())
        return false;

      for (auto index : indices(oParams)) {
        if (!deriveSubstitutions(Subs, oParams[index].getType(),
                                 sParams[index].getType()))
          return false;
      }

      if (!deriveSubstitutions(Subs,
                               O->getResult(),
                               S->getResult()))
        return false;

      return true;
    }
  }

  // Walk down into the instance type.
  if (auto *O = dyn_cast<MetatypeTypeRef>(OrigTR)) {
    if (auto *S = dyn_cast<MetatypeTypeRef>(SubstTR)) {

      if (!deriveSubstitutions(Subs,
                               O->getInstanceType(),
                               S->getInstanceType()))
        return false;

      return true;
    }
  }

  // Walk down into the referent storage type.
  if (auto *O = dyn_cast<ReferenceStorageTypeRef>(OrigTR)) {
    if (auto *S = dyn_cast<ReferenceStorageTypeRef>(SubstTR)) {

      if (O->getKind() != S->getKind())
        return false;

      if (!deriveSubstitutions(Subs,
                               O->getType(),
                               S->getType()))
        return false;

      return true;
    }
  }

  if (isa<DependentMemberTypeRef>(OrigTR)) {
    // FIXME: Do some validation here?
    return true;
  }

  // If the original type is a generic type parameter, just make
  // sure the substituted type matches anything we've already
  // seen.
  if (auto *O = dyn_cast<GenericTypeParameterTypeRef>(OrigTR)) {
    DepthAndIndex key = {O->getDepth(), O->getIndex()};
    auto found = Subs.find(key);
    if (found == Subs.end()) {
      Subs[key] = SubstTR;
      return true;
    }

    return (found->second == SubstTR);
  }

  // Anything else must be concrete and the two types must match
  // exactly.
  return (OrigTR == SubstTR);
}
