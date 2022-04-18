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
#include <sstream>

namespace swift {
namespace specifierDSL {

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
  std::string name;

  const TypeSpecifier &base() const { return *_base.ptr.get(); }
};
struct TypeSpecifier : TaggedUnion<ParamTypeSpecifier,
                                   MemberTypeSpecifier> {
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
inline TypeSpecifier member(TypeSpecifier &&base, std::string &&member) {
  return MemberTypeSpecifier{std::move(base), std::move(member)};
}

inline void mangleNatural(AnyObjectBuilder &builder, size_t value) {
  std::ostringstream str;
  str << value;
  builder.addBytes(str.str());
}

inline void mangleIndex(AnyObjectBuilder &builder, unsigned value) {
  if (value > 0)
    mangleNatural(builder, value - 1);
  builder.add8('_');
}

inline void mangleIdentifier(AnyObjectBuilder &builder, llvm::StringRef value) {
  mangleNatural(builder, value.size());
  // assumes no non-ASCII characters, which is fine for testing
  builder.addBytes(value);
}

inline void mangleGenericParamIndex(AnyObjectBuilder &builder,
                                    unsigned depth, unsigned index) {

  if (depth == 0) {
    assert(index != 0 && "didn't special-case depth=0, index=0 in caller");
    builder.add8('q');
    mangleIndex(builder, index - 1);
  } else {
    builder.add8('q');
    builder.add8('d');
    mangleIndex(builder, depth - 1);
    mangleIndex(builder, index);
  }
}

inline void mangleGenericParamType(AnyObjectBuilder &builder,
                                   unsigned depth, unsigned index) {
  if (depth == 0 && index == 0) {
    builder.add8('x');
  } else {
    builder.add8('q');
    mangleGenericParamIndex(builder, depth, index);
  }
}

inline void
mangleDependentMemberType(AnyObjectBuilder &builder,
                          const MemberTypeSpecifier &spec) {
  std::vector<const std::string *> path;
  path.push_back(&spec.name);

  auto cur = &spec.base();
  while (auto member = cur->dyn_cast<MemberTypeSpecifier>()) {
    path.push_back(&member->name);
    cur = &member->base();
  }
  bool first = true;
  for (auto *memberName : llvm::reverse(path)) {
    if (!first) builder.add8('_');
    first = false;
    mangleIdentifier(builder, *memberName);
  }
  auto &param = cur->get<ParamTypeSpecifier>();
  if (param.depth == 0 && param.index == 0)
    builder.addBytes(path.size() == 1 ? "Qz" : "QZ");
  else {
    builder.addBytes(path.size() == 1 ? "Qy" : "QY");
    mangleGenericParamIndex(builder, param.depth, param.index);
  }
}

inline ObjectRef<const char>
createMangledTypeString(AnyObjectBuilder &builder, const TypeSpecifier &spec) {
  auto nameBuilder = builder.createSubobject<const char>(/*align*/ 2);
  if (auto paramType = spec.dyn_cast<ParamTypeSpecifier>()) {
    mangleGenericParamType(nameBuilder, paramType->depth, paramType->index);
  } else if (auto memberType = spec.dyn_cast<MemberTypeSpecifier>()) {
    mangleDependentMemberType(builder, *memberType);
  } else {
    swift_unreachable("unknown type specifier");
  }
  return nameBuilder.ref();
}

struct ProtocolSpecifier {
  const ProtocolDescriptor *descriptor;
};

/// Construct a protocol specifier for a protocol descriptor reference.
inline ProtocolSpecifier protocol(const ProtocolDescriptor *descriptor) {
  return ProtocolSpecifier{descriptor};
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
                                       /*extra argument*/ false);
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
                                       /*extra argument*/ false);
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
