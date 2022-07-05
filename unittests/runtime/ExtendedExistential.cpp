//===--- ExtendedExistential.cpp - Extended existential metadata unit tests -===//
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

#include "MetadataObjectBuilder.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Runtime/Metadata.h"
#include "gtest/gtest.h"

using namespace swift;

static const ModuleContextDescriptor *module() {
  return buildGlobalModuleContextDescriptor([] {
    return "ExtendedExistential";
  });
}

static ProtocolSpecifier P() {
  return protocol(buildGlobalProtocolDescriptor(module(), [] {
    return "P";
  }));
}

static ProtocolSpecifier Q() {
  return protocol(buildGlobalProtocolDescriptor(module(), [] {
    return "Q";
  }));
}

/// Construct the type () -> ... -> (), just so we have different
/// metadata.
static const Metadata *metadata(unsigned n) {
  auto result = swift_getTupleTypeMetadata(MetadataState::Abstract,
                                           TupleTypeFlags().withNumElements(0),
                                           nullptr, nullptr, nullptr).Value;
  while (n--) {
    result = swift_getFunctionTypeMetadata(FunctionTypeFlags().withNumParameters(0),
                                           nullptr, nullptr, result);
  }
  return result;
}

using SpecialKind = ExtendedExistentialTypeShapeFlags::SpecialKind;

namespace {

struct GenSigShapeSpecifier {
  GenericSignatureSpecifier sig;
};
struct ReqSigShapeSpecifier {
  GenericSignatureSpecifier sig;
};
struct SpecialKindShapeSpecifier {
  SpecialKind kind;
};
struct VWTableShapeSpecifier {
  const ValueWitnessTable *vwtable;
};
struct ShapeTypeSpecifier {
  TypeSpecifier type;
};

struct ShapeSpecifier : TaggedUnion<GenSigShapeSpecifier,
                                    ReqSigShapeSpecifier,
                                    ShapeTypeSpecifier,
                                    SpecialKindShapeSpecifier,
                                    VWTableShapeSpecifier> {
  using TaggedUnion::TaggedUnion;
};

} // end anonymous namespace

static TypeSpecifier genParam(unsigned index) {
  return typeParam(1, index);
}
static TypeSpecifier reqParam(unsigned index) {
 return typeParam(0, index);
}

template <class... Specs>
static GenSigShapeSpecifier genSig(Specs &&...specs) {
  return GenSigShapeSpecifier{signature(std::forward<Specs>(specs)...)};
}
template <class... Specs>
static ReqSigShapeSpecifier reqSig(Specs &&...specs) {
  return ReqSigShapeSpecifier{signature(std::forward<Specs>(specs)...)};
}
static SpecialKindShapeSpecifier special(SpecialKind kind) {
  return SpecialKindShapeSpecifier{kind};
}
static VWTableShapeSpecifier valueWitnesses(const ValueWitnessTable *table) {
  return VWTableShapeSpecifier{table};
}
static ShapeTypeSpecifier shapeType(TypeSpecifier &&spec) {
  return ShapeTypeSpecifier{std::move(spec)};
}

using ShapeSpecifierList = std::vector<ShapeSpecifier>;

template <class... Specs>
static ShapeSpecifierList shape(Specs &&...specs) {
  ShapeSpecifierList list;
  // When we move to C++17, this can become:
  //   (list.emplace_back(std::forward<Specs>(specs)), ...);
  emplace_back_all(list, std::forward<Specs>(specs)...);
  return list;
}

static void addExistentialShape(AnyObjectBuilder &builder,
                                const ShapeSpecifierList &specifiers) {
  ExtendedExistentialTypeShapeFlags flags;
  const ValueWitnessTable *vwtable = nullptr;
  const GenericSignatureSpecifier *genSig = nullptr;
  const GenericSignatureSpecifier *reqSig = nullptr;
  const TypeSpecifier *shapeType = nullptr;

  for (auto &spec : specifiers) {
    if (auto sig = spec.dyn_cast<GenSigShapeSpecifier>()) {
      assert(genSig == nullptr && "generalization signature specified twice");
      genSig = &sig->sig;
    } else if (auto sig = spec.dyn_cast<ReqSigShapeSpecifier>()) {
      assert(reqSig == nullptr && "requirement signature specified twice");
      reqSig = &sig->sig;
    } else if (auto table = spec.dyn_cast<VWTableShapeSpecifier>()) {
      assert(vwtable == nullptr && "vwtable override specified twice");
      vwtable = table->vwtable;
      flags = flags.withSuggestedValueWitnesses(true);
    } else if (auto special = spec.dyn_cast<SpecialKindShapeSpecifier>()) {
      assert(flags.getSpecialKind() == SpecialKind::None &&
             "special kind specified twice");
      flags = flags.withSpecialKind(special->kind);
    } else if (auto type = spec.dyn_cast<ShapeTypeSpecifier>()) {
      assert(shapeType == nullptr && "shape specified twice");
      shapeType = &type->type;
    } else {
      swift_unreachable("bad shape specifier");
    }
  }

  assert(shapeType && "shape type wasn't specified");
  assert(reqSig && "requirement signature wasn't specified");
  flags = flags.withImplicitReqSigParams(
                                canGenericParamsBeImplicit(reqSig->params));
  if (genSig) {
    flags = flags.withGeneralizationSignature(true)
                 .withImplicitGenSigParams(
                                canGenericParamsBeImplicit(genSig->params));
  }

  // Flags
  builder.add32(flags.getIntValue());

  // ExistentialType
  builder.addRelativeReference(createMangledTypeString(builder, *shapeType));

  // ReqSigHeader
  addGenericContextDescriptorHeader(builder, *reqSig);

  // Optional GenSigHeader
  if (flags.hasGeneralizationSignature())
    addGenericContextDescriptorHeader(builder, *genSig);

  // TODO: optional type expression

  // Optional suggested value witnesses
  if (flags.hasSuggestedValueWitnesses())
    builder.addRelativeIndirectReference(vwtable, /*addend*/ 1);

  // Generic parameter descriptors
  if (!flags.hasImplicitReqSigParams())
    addGenericParams(builder, *reqSig);
  if (flags.hasGeneralizationSignature() &&
      !flags.hasImplicitGenSigParams())
    addGenericParams(builder, *genSig);

  // Generic requirement descriptors
  builder.padToAlignment(alignof(GenericRequirementDescriptor));
  addGenericRequirements(builder, *reqSig);
  if (flags.hasGeneralizationSignature())
    addGenericRequirements(builder, *genSig);
}

static void addNonUniqueExistentialShape(AnyObjectBuilder &builder,
                                   const ShapeSpecifierList &specifiers) {
  // cache
  builder.addRelativeIndirectReference(nullptr);

  addExistentialShape(builder, specifiers);
}

template <class Fn>
static const NonUniqueExtendedExistentialTypeShape *
buildGlobalNonUniqueShape(Fn &&fn) {
  return buildGlobalObject<NonUniqueExtendedExistentialTypeShape>(
      [&](AnyObjectBuilder &builder) {
    addNonUniqueExistentialShape(builder, std::forward<Fn>(fn)());
  });
}

template <class Fn>
static const ExtendedExistentialTypeShape *
buildGlobalShape(Fn &&fn) {
  return buildGlobalObject<ExtendedExistentialTypeShape>(
      [&](AnyObjectBuilder &builder) {
    addExistentialShape(builder, std::forward<Fn>(fn)());
  });
}

TEST(TestExtendedExistential, shapeUniquing) {
  auto shape0 = buildGlobalNonUniqueShape([]{
    return shape(
      genSig(param()),
      shapeType(parameterizedProtocol(P(), "Element", genParam(0))),
      reqSig(param(),
             conforms(reqParam(0), P()),
             sameType(member(reqParam(0), P(), "Element"), genParam(0)))
    );
  });
  auto shape1 = buildGlobalNonUniqueShape([]{
    return shape(
      genSig(param()),
      shapeType(parameterizedProtocol(P(), "Element", genParam(0))),
      reqSig(param(),
             conforms(reqParam(0), P()),
             sameType(member(reqParam(0), P(), "Element"), genParam(0)))
    );
  });
  auto shape2 = buildGlobalNonUniqueShape([]{
    return shape(
      genSig(param()),
      shapeType(parameterizedProtocol(Q(), "Element", genParam(0))),
      reqSig(param(),
             conforms(reqParam(0), Q()),
             sameType(member(reqParam(0), Q(), "Element"), genParam(0)))
    );
  });
  auto shape3 = buildGlobalNonUniqueShape([]{
    return shape(
      genSig(param()),
      shapeType(parameterizedProtocol(Q(), "Element", genParam(0))),
      reqSig(param(),
             conforms(reqParam(0), Q()),
             sameType(member(reqParam(0), Q(), "Element"), genParam(0)))
    );
  });

  auto result0 = swift_getExtendedExistentialTypeShape(shape0);
  EXPECT_EQ(result0, &shape0->LocalCopy);

  auto result1 = shape0->UniqueCache.get()->load(std::memory_order_acquire);
  EXPECT_EQ(result1, result0);

  auto result2 = swift_getExtendedExistentialTypeShape(shape0);
  EXPECT_EQ(result2, result0);

  auto result3 = swift_getExtendedExistentialTypeShape(shape1);
  EXPECT_EQ(result3, result0);

  auto result4 = shape1->UniqueCache.get()->load(std::memory_order_acquire);
  EXPECT_EQ(result4, result0);

  auto result5 = swift_getExtendedExistentialTypeShape(shape2);
  EXPECT_EQ(result5, &shape2->LocalCopy);
  EXPECT_NE(result5, result0);

  auto result6 = swift_getExtendedExistentialTypeShape(shape3);
  EXPECT_EQ(result6, result5);
}

TEST(TestExtendedExistential, nullaryMetadata) {
  auto shape1 = buildGlobalShape([]{
    return shape(
      shapeType(protocolType(P())),
      reqSig(param(),
             conforms(reqParam(0), P()))
    );
  });
  auto metadata1 = swift_getExtendedExistentialTypeMetadata_unique(shape1, nullptr);
  EXPECT_EQ(metadata1->Shape, shape1);

  auto metadata2 = swift_getExtendedExistentialTypeMetadata_unique(shape1, nullptr);
  EXPECT_EQ(metadata2->Shape, shape1);
  EXPECT_EQ(metadata2, metadata1);
}

TEST(TestExtendedExistential, unaryMetadata) {
  auto shape1 = buildGlobalShape([]{
    return shape(
      genSig(param()),
      shapeType(protocolType(P())),
      reqSig(param(),
             conforms(reqParam(0), P()))
    );
  });
  auto shape2 = buildGlobalShape([]{
    return shape(
      genSig(param()),
      shapeType(protocolType(P())),
      reqSig(param(),
             conforms(reqParam(0), Q()))
    );
  });

  const void *args1[] = { metadata(0) };
  auto metadata1 = swift_getExtendedExistentialTypeMetadata_unique(shape1, args1);
  EXPECT_EQ(metadata1->Shape, shape1);
  EXPECT_EQ(metadata1->getGeneralizationArguments()[0], args1[0]);

  const void *args2[] = { metadata(0) };
  auto metadata2 = swift_getExtendedExistentialTypeMetadata_unique(shape1, args2);
  EXPECT_EQ(metadata2->Shape, shape1);
  EXPECT_EQ(metadata2->getGeneralizationArguments()[0], args2[0]);
  EXPECT_EQ(metadata2, metadata1);

  const void *args3[] = { metadata(1) };
  auto metadata3 = swift_getExtendedExistentialTypeMetadata_unique(shape1, args3);
  EXPECT_EQ(metadata3->Shape, shape1);
  EXPECT_EQ(metadata3->getGeneralizationArguments()[0], args3[0]);
  EXPECT_NE(metadata3, metadata1);

  const void *args4[] = { metadata(1) };
  auto metadata4 = swift_getExtendedExistentialTypeMetadata_unique(shape1, args4);
  EXPECT_EQ(metadata4->Shape, shape1);
  EXPECT_EQ(metadata4->getGeneralizationArguments()[0], args4[0]);
  EXPECT_NE(metadata4, metadata1);
  EXPECT_EQ(metadata4, metadata3);

  const void *args5[] = { metadata(0) };
  auto metadata5 = swift_getExtendedExistentialTypeMetadata_unique(shape2, args5);
  EXPECT_EQ(metadata5->Shape, shape2);
  EXPECT_EQ(metadata5->getGeneralizationArguments()[0], args5[0]);
  EXPECT_NE(metadata5, metadata1);
}

TEST(TestExtendedExistential, binaryMetadata) {
  auto shape1 = buildGlobalShape([]{
    return shape(
      genSig(param(), param()),
      shapeType(protocolType(P())),
      reqSig(param(),
             conforms(reqParam(0), P()))
    );
  });

  const void *args1[] = { metadata(0), metadata(0) };
  auto metadata1 = swift_getExtendedExistentialTypeMetadata_unique(shape1, args1);
  EXPECT_EQ(metadata1->Shape, shape1);
  EXPECT_EQ(metadata1->getGeneralizationArguments()[0], args1[0]);
  EXPECT_EQ(metadata1->getGeneralizationArguments()[1], args1[1]);

  const void *args2[] = { metadata(0), metadata(0) };
  auto metadata2 = swift_getExtendedExistentialTypeMetadata_unique(shape1, args2);
  EXPECT_EQ(metadata2->Shape, shape1);
  EXPECT_EQ(metadata2->getGeneralizationArguments()[0], args2[0]);
  EXPECT_EQ(metadata2->getGeneralizationArguments()[1], args2[1]);
  EXPECT_EQ(metadata2, metadata1);

  const void *args3[] = { metadata(1), metadata(0) };
  auto metadata3 = swift_getExtendedExistentialTypeMetadata_unique(shape1, args3);
  EXPECT_EQ(metadata3->Shape, shape1);
  EXPECT_EQ(metadata3->getGeneralizationArguments()[0], args3[0]);
  EXPECT_EQ(metadata3->getGeneralizationArguments()[1], args3[1]);
  EXPECT_NE(metadata3, metadata1);

  const void *args4[] = { metadata(0), metadata(1) };
  auto metadata4 = swift_getExtendedExistentialTypeMetadata_unique(shape1, args4);
  EXPECT_EQ(metadata4->Shape, shape1);
  EXPECT_EQ(metadata4->getGeneralizationArguments()[0], args4[0]);
  EXPECT_EQ(metadata4->getGeneralizationArguments()[1], args4[1]);
  EXPECT_NE(metadata4, metadata1);
  EXPECT_NE(metadata4, metadata3);
}

TEST(TestExtendedExistential, overrideValueWitnesses) {
  static ValueWitnessTable table = {};
  auto shape0 = buildGlobalShape([]{
    return shape(
      special(SpecialKind::ExplicitLayout),
      valueWitnesses(&table),
      shapeType(protocolType(P())),
      reqSig(param())
    );
  });
  auto metadata0 = swift_getExtendedExistentialTypeMetadata_unique(shape0, nullptr);
  EXPECT_EQ(metadata0->Shape, shape0);
  EXPECT_EQ(metadata0->Shape->Flags.getSpecialKind(),
            SpecialKind::ExplicitLayout);

  auto vwtable0 = metadata0->getValueWitnesses();
  EXPECT_EQ(vwtable0, &table);
}

TEST(TestExtendedExistential, defaultOpaqueValueWitnesses) {
  auto shape0 = buildGlobalShape([]{
    return shape(
      shapeType(protocolType(P())),
      reqSig(param())
    );
  });
  EXPECT_EQ(shape0->Flags.getSpecialKind(), SpecialKind::None);

  auto metadata0 = swift_getExtendedExistentialTypeMetadata_unique(shape0, nullptr);
  auto vwtable0 = metadata0->getValueWitnesses();
  EXPECT_NE(vwtable0, nullptr);
  EXPECT_EQ(vwtable0->size, sizeof(ValueBuffer) + 1 * sizeof(void*));
  EXPECT_FALSE(vwtable0->isPOD());

  auto shape1 = buildGlobalShape([]{
    return shape(
      shapeType(protocolType(P())),
      reqSig(param(),
             conforms(reqParam(0), P()))
    );
  });
  auto metadata1 = swift_getExtendedExistentialTypeMetadata_unique(shape1, nullptr);
  auto vwtable1 = metadata1->getValueWitnesses();
  EXPECT_NE(vwtable1, nullptr);
  EXPECT_EQ(vwtable1->size, sizeof(ValueBuffer) + 2 * sizeof(void*));
  EXPECT_FALSE(vwtable1->isPOD());

  auto shape2 = buildGlobalShape([]{
    return shape(
      shapeType(protocolComposition(P(), Q())),
      reqSig(param(),
             conforms(reqParam(0), P()),
             conforms(reqParam(0), Q()))
    );
  });
  auto metadata2 = swift_getExtendedExistentialTypeMetadata_unique(shape2, nullptr);
  auto vwtable2 = metadata2->getValueWitnesses();
  EXPECT_NE(vwtable2, nullptr);
  EXPECT_EQ(vwtable2->size, sizeof(ValueBuffer) + 3 * sizeof(void*));
  EXPECT_FALSE(vwtable2->isPOD());
}

TEST(TestExtendedExistential, defaultClassValueWitnesses) {
  auto shape0 = buildGlobalShape([]{
    return shape(
      special(SpecialKind::Class),
      shapeType(protocolType(P())),
      reqSig(param())
    );
  });
  EXPECT_EQ(shape0->Flags.getSpecialKind(), SpecialKind::Class);

  auto metadata0 = swift_getExtendedExistentialTypeMetadata_unique(shape0, nullptr);
  auto vwtable0 = metadata0->getValueWitnesses();
  EXPECT_NE(vwtable0, nullptr);
  EXPECT_EQ(vwtable0->size, sizeof(void*) + 0 * sizeof(void*));
  EXPECT_FALSE(vwtable0->isPOD());

  auto shape1 = buildGlobalShape([]{
    return shape(
      special(SpecialKind::Class),
      shapeType(protocolType(P())),
      reqSig(param(),
             conforms(reqParam(0), P()))
    );
  });
  auto metadata1 = swift_getExtendedExistentialTypeMetadata_unique(shape1, nullptr);
  auto vwtable1 = metadata1->getValueWitnesses();
  EXPECT_NE(vwtable1, nullptr);
  EXPECT_EQ(vwtable1->size, sizeof(void*) + 1 * sizeof(void*));
  EXPECT_FALSE(vwtable1->isPOD());

  auto shape2 = buildGlobalShape([]{
    return shape(
      special(SpecialKind::Class),
      shapeType(protocolComposition(P(), Q())),
      reqSig(param(),
             conforms(reqParam(0), P()),
             conforms(reqParam(0), Q()))
    );
  });
  auto metadata2 = swift_getExtendedExistentialTypeMetadata_unique(shape2, nullptr);
  auto vwtable2 = metadata2->getValueWitnesses();
  EXPECT_NE(vwtable2, nullptr);
  EXPECT_EQ(vwtable2->size, sizeof(void*) + 2 * sizeof(void*));
  EXPECT_FALSE(vwtable2->isPOD());
}

TEST(TestExtendedExistential, defaultMetatypeValueWitnesses) {
  auto shape0 = buildGlobalShape([]{
    return shape(
      special(SpecialKind::Metatype),
      shapeType(protocolType(P())),
      reqSig(param())
    );
  });
  EXPECT_EQ(shape0->Flags.getSpecialKind(), SpecialKind::Metatype);

  auto metadata0 = swift_getExtendedExistentialTypeMetadata_unique(shape0, nullptr);
  auto vwtable0 = metadata0->getValueWitnesses();
  EXPECT_NE(vwtable0, nullptr);
  EXPECT_EQ(vwtable0->size, sizeof(void*) + 0 * sizeof(void*));
  EXPECT_TRUE(vwtable0->isPOD());

  auto shape1 = buildGlobalShape([]{
    return shape(
      special(SpecialKind::Metatype),
      shapeType(protocolType(P())),
      reqSig(param(),
             conforms(reqParam(0), P()))
    );
  });
  auto metadata1 = swift_getExtendedExistentialTypeMetadata_unique(shape1, nullptr);
  auto vwtable1 = metadata1->getValueWitnesses();
  EXPECT_NE(vwtable1, nullptr);
  EXPECT_EQ(vwtable1->size, sizeof(void*) + 1 * sizeof(void*));
  EXPECT_TRUE(vwtable1->isPOD());

  auto shape2 = buildGlobalShape([]{
    return shape(
      special(SpecialKind::Metatype),
      shapeType(protocolComposition(P(), Q())),
      reqSig(param(),
             conforms(reqParam(0), P()),
             conforms(reqParam(0), Q()))
    );
  });
  auto metadata2 = swift_getExtendedExistentialTypeMetadata_unique(shape2, nullptr);
  auto vwtable2 = metadata2->getValueWitnesses();
  EXPECT_NE(vwtable2, nullptr);
  EXPECT_EQ(vwtable2->size, sizeof(void*) + 2 * sizeof(void*));
  EXPECT_TRUE(vwtable2->isPOD());
}
