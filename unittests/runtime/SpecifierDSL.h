//===--- SpecifierDSL.h ----------------------------------------*- C++ -*--===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines a little DSL for building metadata objects from specifier
// values.
//
//===----------------------------------------------------------------------===//

#ifndef SPECIFIER_DSL_H
#define SPECIFIER_DSL_H

#include "ObjectBuilder.h"
#include "swift/Basic/TaggedUnion.h"
#include "swift/ABI/Metadata.h"
#include "swift/Demangling/Demangler.h"
#include <sstream>

namespace swift {
namespace specifierDSL {

struct ProtocolSpecifier {
  const ProtocolDescriptor *descriptor;
};

/// Construct a protocol specifier for a protocol descriptor reference.
inline ProtocolSpecifier protocol(const ProtocolDescriptor *descriptor) {
  return ProtocolSpecifier{descriptor};
}

struct TypeSpecifier;

struct IndirectTypeSpecifier {
  std::unique_ptr<TypeSpecifier> ptr;

  template <class T>
  IndirectTypeSpecifier(T &&specifier);
};
struct ParamTypeSpecifier {
  unsigned depth;
  unsigned index;
};
struct MemberTypeSpecifier {
  IndirectTypeSpecifier _base;
  ProtocolSpecifier protocol;
  std::string name;

  const TypeSpecifier &base() const { return *_base.ptr.get(); }
};
struct ProtocolTypeSpecifier {
  ProtocolSpecifier protocol;
};
struct ConstrainedExistentialTypeSpecifier {
  ProtocolSpecifier base;
  std::vector<std::pair<std::string, TypeSpecifier>> args;
};
struct ProtocolCompositionTypeSpecifier {
  std::vector<TypeSpecifier> components;
};
struct TypeSpecifier
    : TaggedUnion<ParamTypeSpecifier, MemberTypeSpecifier,
                  ProtocolTypeSpecifier, ProtocolCompositionTypeSpecifier,
                  ConstrainedExistentialTypeSpecifier> {
  using TaggedUnion::TaggedUnion;
};

template <class T>
inline IndirectTypeSpecifier::IndirectTypeSpecifier(T &&spec)
  : ptr(new TypeSpecifier(std::forward<T>(spec))) {}

/// Construct a type specifier for a generic type parameter at the
/// given depth and index.
inline TypeSpecifier typeParam(unsigned depth, unsigned index) {
  return ParamTypeSpecifier{depth, index};
}

/// Construct a type specifier for a dependent member type.
inline TypeSpecifier member(TypeSpecifier &&base,
                            ProtocolSpecifier &&protocol,
                            std::string &&member) {
  return MemberTypeSpecifier{std::move(base),
                             std::move(protocol),
                             std::move(member)};
}

/// Construct a type specifier for a protocol type.
inline TypeSpecifier protocolType(ProtocolSpecifier &&protocol) {
  return ProtocolTypeSpecifier{std::move(protocol)};
}

inline void addCompositionComponent(std::vector<TypeSpecifier> &components,
                                    ProtocolSpecifier &&protocol) {
  components.push_back(protocolType(std::move(protocol)));
}
inline void addCompositionComponent(std::vector<TypeSpecifier> &components,
                                    TypeSpecifier &&type) {
  components.push_back(std::move(type));
}
inline void addCompositionComponents(std::vector<TypeSpecifier> &components) {}
template <class Spec, class... Specs>
inline void addCompositionComponents(std::vector<TypeSpecifier> &components,
                                     Spec &&spec, Specs &&...specs) {
  addCompositionComponent(components, std::forward<Spec>(spec));
  addCompositionComponents(components, std::forward<Specs>(specs)...);
}

template <class... Specs>
inline TypeSpecifier protocolComposition(Specs &&...specs) {
  std::vector<TypeSpecifier> components;
  addCompositionComponents(components, std::forward<Specs>(specs)...);
  return ProtocolCompositionTypeSpecifier{std::move(components)};
}

inline void addParameterizedProtocolArguments(
              std::vector<std::pair<std::string, TypeSpecifier>> &args) {}
template <class... Specs>
inline void addParameterizedProtocolArguments(
              std::vector<std::pair<std::string, TypeSpecifier>> &args,
              std::string &&associatedTypeName,
              TypeSpecifier &&type,
              Specs &&...specs) {
  args.push_back(std::make_pair(std::move(associatedTypeName), std::move(type)));
  addParameterizedProtocolArguments(args, std::forward<Specs>(specs)...);
}

template <class... Specs>
inline TypeSpecifier parameterizedProtocol(ProtocolSpecifier &&base,
                                           Specs &&...specs) {
  std::vector<std::pair<std::string, TypeSpecifier>> args;
  addParameterizedProtocolArguments(args, std::forward<Specs>(specs)...);
  return ConstrainedExistentialTypeSpecifier{std::move(base), std::move(args)};
}

/// A class which "demangles" various DSL specifiers into demangle
/// tree nodes.
class Demangler {
  NodeFactory factory;

  using NodePointer = Demangle::NodePointer;
  using Kind = Demangle::Node::Kind;

  NodePointer node(Kind kind, llvm::ArrayRef<NodePointer> children) {
    auto node = factory.createNode(kind);
    for (auto child: children)
      node->addChild(child, factory);
    return node;
  }
  NodePointer nodeWithIndex(Kind kind, Node::IndexType index) {
    return factory.createNode(kind, index);
  }
  NodePointer nodeWithText(Kind kind, llvm::StringRef string) {
    auto node = factory.createNode(kind, string);
    return node;
  }

  NodePointer demangleModule(const ModuleContextDescriptor *descriptor) {
    assert(descriptor->Parent.isNull());
    return nodeWithText(Kind::Module, descriptor->Name.get());
  }
  NodePointer demangleProtocol(const ProtocolDescriptor *descriptor) {
    return node(Kind::Protocol,
                {demangleContext(descriptor->Parent.get()),
                 nodeWithText(Kind::Identifier, descriptor->Name.get())});
  }
  NodePointer demangleProtocol(const ProtocolSpecifier &spec) {
    return demangleProtocol(spec.descriptor);
  }

  NodePointer demangleContext(const ContextDescriptor *context) {
    if (auto module = dyn_cast<ModuleContextDescriptor>(context))
      return demangleModule(module);
    if (auto protocol = dyn_cast<ProtocolDescriptor>(context))
      return demangleProtocol(protocol);
    swift_unreachable("unknown context");
  }

  NodePointer demangleParamType(const ParamTypeSpecifier &spec) {
    return node(Kind::DependentGenericParamType,
                {nodeWithIndex(Kind::Index, spec.depth),
                 nodeWithIndex(Kind::Index, spec.index)});
  }

  NodePointer demangleMemberType(const MemberTypeSpecifier &spec) {
    return node(Kind::DependentMemberType,
                {demangleType(spec.base()),
                 node(Kind::DependentAssociatedTypeRef,
                      {nodeWithText(Kind::Identifier, spec.name),
                       demangleProtocol(spec.protocol)})});
  }

  NodePointer demangleProtocolType(const ProtocolTypeSpecifier &spec) {
    return demangleProtocol(spec.protocol);
  }

  NodePointer demangleProtocolCompositionType(
                        const ProtocolCompositionTypeSpecifier &spec) {
    std::vector<NodePointer> components;
    for (auto &component: spec.components) {
      components.push_back(demangleType(component));
    }
    return node(Kind::ProtocolList,
                {node(Kind::TypeList, components)});
  }

  NodePointer demangleConstrainedExistentialType(
      const ConstrainedExistentialTypeSpecifier &spec) {
    // Demangle the base protocol and then wrap it up like the tree expects,
    // which for some reason is this.
    auto base = demangleProtocol(spec.base);
    base = node(Kind::Type,
                {node(Kind::ProtocolList,
                      {node(Kind::Type,
                            {base})})});

    std::vector<NodePointer> reqVector;
    bool firstReq = false;
    for (auto &arg: spec.args) {
      auto depType =
          node(Kind::Type,
               {node(Kind::DependentMemberType,
                       {node(Kind::Type,
                             {factory.createNode(Kind::ConstrainedExistentialSelf),
                         node(Kind::DependentAssociatedTypeRef,
                             {factory.createNode(Kind::Identifier, arg.first),
                               node(Kind::Type,
                                   {demangleProtocol(spec.base)})})})})});
      auto constraintType = demangleType(arg.second);
      reqVector.push_back(node(Kind::DependentGenericSameTypeRequirement,
                               {depType, constraintType}));
      if (firstReq) {
        reqVector.push_back(node(Kind::FirstElementMarker, {}));
        firstReq = false;
      }
    }
    NodePointer constraints =
        node(Node::Kind::ConstrainedExistentialRequirementList, reqVector);
    return node(Kind::ConstrainedExistential, {base, constraints});
  }

  NodePointer demangleTypeImpl(const TypeSpecifier &spec) {
#define CASE(ID)                                              \
    if (auto type = spec.dyn_cast<ID##Specifier>())           \
      return demangle##ID(*type);
    CASE(ParamType)
    CASE(MemberType)
    CASE(ProtocolType)
    CASE(ProtocolCompositionType)
    CASE(ConstrainedExistentialType)
#undef CASE
    swift_unreachable("unknown type specifier");
  }

public:
  NodePointer demangleType(const TypeSpecifier &spec) {
    return node(Kind::Type, {demangleTypeImpl(spec)});
  }
};

inline ObjectRef<const char>
createMangledTypeString(AnyObjectBuilder &builder, const TypeSpecifier &spec) {
  Demangler demangler;
  auto node = demangler.demangleType(spec);
  auto nameBuilder = builder.createSubobject<const char>(/*align*/ 2);
  nameBuilder.addString(Demangle::nodeToString(node));
  return nameBuilder.ref();
}

/// Add a ProtocolDescriptorRef.
inline void addProtocolDescriptorRef(AnyObjectBuilder &builder,
                                     const ProtocolSpecifier &spec) {
  builder.addRelativeIndirectReference(spec.descriptor, /*addend*/ 1);
}

struct SameTypeReqtSpecifier {
  TypeSpecifier lhs, rhs;
};
struct ConformanceReqtSpecifier {
  TypeSpecifier type;
  ProtocolSpecifier protocol;
  bool isKeyArgument;
};
struct ReqtSpecifier : TaggedUnion<SameTypeReqtSpecifier,
                                   ConformanceReqtSpecifier> {
  using TaggedUnion::TaggedUnion;
};

inline ReqtSpecifier sameType(TypeSpecifier &&lhs, TypeSpecifier &&rhs) {
  return SameTypeReqtSpecifier{std::move(lhs), std::move(rhs)};
}

inline ReqtSpecifier conforms(TypeSpecifier &&type,
                              ProtocolSpecifier &&protocol,
                              bool isKeyArgument = true) {
  return ConformanceReqtSpecifier{std::move(type), std::move(protocol),
                                  isKeyArgument};
}

/// Add a GenericRequirementDescriptor that makes the given two
/// types the same.
inline void addSameTypeRequirement(AnyObjectBuilder &builder,
                                   ObjectRef<const char> type1,
                                   ObjectRef<const char> type2) {
  auto flags = GenericRequirementFlags(GenericRequirementKind::SameType,
                                       /*key argument*/ false,
                                       /*is pack requirement*/ false,
                                       /*is value requirement*/ false);
  builder.add32(flags.getIntValue());
  builder.addRelativeReference(type1);
  builder.addRelativeReference(type2);
}
inline void addSameTypeRequirement(AnyObjectBuilder &builder,
                                   const TypeSpecifier &type1,
                                   const TypeSpecifier &type2) {
  auto type1Object = createMangledTypeString(builder, type1);
  auto type2Object = createMangledTypeString(builder, type2);
  addSameTypeRequirement(builder, type1Object, type2Object);
}

/// Add a GenericRequirementDescriptor that marks the first type
/// as conforming to the given protocol.
inline void addConformanceRequirement(AnyObjectBuilder &builder,
                                      ObjectRef<const char> type,
                                      const ProtocolSpecifier &protocol,
                                      bool isKeyArgument = true) {
  auto flags = GenericRequirementFlags(GenericRequirementKind::Protocol,
                                       isKeyArgument,
                                       /*is pack requirement*/ false,
                                       /*is value requirement*/ false);
  builder.add32(flags.getIntValue());
  builder.addRelativeReference(type);
  addProtocolDescriptorRef(builder, protocol);
}
inline void addConformanceRequirement(AnyObjectBuilder &builder,
                                      const TypeSpecifier &type,
                                      const ProtocolSpecifier &protocol,
                                      bool isKeyArgument = true) {
  auto typeObject = createMangledTypeString(builder, type);
  addConformanceRequirement(builder, typeObject, protocol, isKeyArgument);
}

static void addGenericRequirements(AnyObjectBuilder &builder,
                                   llvm::ArrayRef<ReqtSpecifier> reqts) {
  for (auto &reqt : reqts) {
    if (auto sameType = reqt.dyn_cast<SameTypeReqtSpecifier>()) {
      addSameTypeRequirement(builder, sameType->lhs, sameType->rhs);
    } else if (auto conforms = reqt.dyn_cast<ConformanceReqtSpecifier>()) {
      addConformanceRequirement(builder, conforms->type, conforms->protocol,
                                conforms->isKeyArgument);
    } else {
      swift_unreachable("unknown requirement specifier");
    }
  }
}

struct GenericSignatureSpecifier {
  std::vector<GenericParamDescriptor> params;
  std::vector<ReqtSpecifier> reqts;

  void addSpecifiers() {}
  template <class S, class... T>
  void addSpecifiers(S &&spec, T &&...rest) {
    addSpecifier(std::forward<S>(spec));
    addSpecifiers(std::forward<T>(rest)...);
  }

  void addSpecifier(GenericParamDescriptor param) {
    params.push_back(param);
  }
  void addSpecifier(ReqtSpecifier &&reqt) {
    reqts.push_back(std::move(reqt));
  }
};

inline GenericParamDescriptor param() {
  return GenericParamDescriptor::implicit();
}

template <class... Specs>
inline GenericSignatureSpecifier signature(Specs &&...specs) {
  GenericSignatureSpecifier sig;
  sig.addSpecifiers(std::forward<Specs>(specs)...);
  return sig;
}

inline void addGenericParams(AnyObjectBuilder &builder,
                             llvm::ArrayRef<GenericParamDescriptor> params) {
  builder.addBytes(params.data(), params.size());
}

/// Add a GenericContextDescriptorHeader to the given object.
inline void addGenericContextDescriptorHeader(AnyObjectBuilder &builder,
                             llvm::ArrayRef<GenericParamDescriptor> params,
                             llvm::ArrayRef<ReqtSpecifier> reqts) {
  unsigned argSizeInWords = 0;
  for (auto param: params) {
    if (param.hasKeyArgument())
      argSizeInWords++;
  }
  for (auto &reqt : reqts) {
    if (auto conf = reqt.dyn_cast<ConformanceReqtSpecifier>())
      if (conf->isKeyArgument)
        argSizeInWords++;
  }

  // NumParams
  builder.add16(params.size());

  // NumReqts
  builder.add16(reqts.size());

  // NumKeyArguments
  builder.add16(argSizeInWords);

  // NumExtraArguments
  builder.add16(0);
}
inline void addGenericContextDescriptorHeader(AnyObjectBuilder &builder,
                                        const GenericSignatureSpecifier &sig) {
  addGenericContextDescriptorHeader(builder, sig.params, sig.reqts);
}

inline void addGenericParams(AnyObjectBuilder &builder,
                             const GenericSignatureSpecifier &sig) {
  addGenericParams(builder, sig.params);
}

inline void addGenericRequirements(AnyObjectBuilder &builder,
                                   const GenericSignatureSpecifier &sig) {
  addGenericRequirements(builder, sig.reqts);
}

} // end namespace specifierDSL
} // end namespace swift

#endif
