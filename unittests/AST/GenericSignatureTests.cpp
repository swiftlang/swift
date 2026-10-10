//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Types.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

TEST(GenericSignature, MetatypeRequirementQueries) {
  TestContext C;
  ASTContext &Context = C.Ctx;

  ProtocolDecl *protocol = C.makeProtocol("P");
  GenericTypeParamType *T = GenericTypeParamType::getType(0, 0, Context);
  Type metatype = MetatypeType::get(T);

  auto signature =
      buildGenericSignature(Context, nullptr, {T},
                            {Requirement(RequirementKind::Conformance, metatype,
                                         protocol->getDeclaredInterfaceType())},
                            DefaultRequirementOptions());

  EXPECT_TRUE(signature->requiresProtocol(metatype, protocol));

  auto protocols = signature->getRequiredProtocols(metatype);
  ASSERT_EQ(protocols.size(), 1u);
  ASSERT_EQ(protocols.front(), protocol);

  auto requirements = signature->getLocalRequirements(metatype);
  ASSERT_EQ(requirements.protos.size(), 1u);
  EXPECT_EQ(requirements.protos.front(), protocol);
  EXPECT_FALSE(requirements.superclass);
  EXPECT_FALSE(requirements.layout);
  EXPECT_FALSE(requirements.packShape);
}

TEST(GenericSignature, AbstractMetatypeConformance) {
  TestContext C;
  ASTContext &Context = C.Ctx;

  ProtocolDecl *protocol = C.makeProtocol("P");
  GenericTypeParamType *T = GenericTypeParamType::getType(0, 0, Context);
  Type metatype = MetatypeType::get(T);

  auto signature =
      buildGenericSignature(Context, nullptr, {T},
                            {Requirement(RequirementKind::Conformance, metatype,
                                         protocol->getDeclaredInterfaceType())},
                            DefaultRequirementOptions());

  // A metatype requirement can be represented by an abstract conformance.
  auto conformance = ProtocolConformanceRef::forAbstract(metatype, protocol);

  ASSERT_TRUE(conformance);
  EXPECT_TRUE(conformance.isAbstract());

  // Looking up the requirement before mapping it into an environment must
  // preserve the abstract metatype conformance.
  auto interfaceConformance = lookupConformance(metatype, protocol);
  ASSERT_TRUE(interfaceConformance);
  EXPECT_TRUE(interfaceConformance.isAbstract());

  // A substitution map can retrieve the conformance for a metatype subject.
  llvm::SmallVector<Type, 1> replacements{T};
  llvm::SmallVector<ProtocolConformanceRef, 1> conformances{conformance};

  auto substitutions =
      SubstitutionMap::get(signature, replacements, conformances);

  auto substituted =
      substitutions.lookupConformance(metatype->getCanonicalType(), protocol);
  ASSERT_TRUE(substituted);
  EXPECT_TRUE(substituted.isAbstract());

  auto *environment = signature.getGenericEnvironment();
  ASSERT_NE(environment, nullptr);

  Type archetype = environment->mapTypeIntoEnvironment(T);

  auto lookup = lookupConformance(MetatypeType::get(archetype), protocol);
  ASSERT_TRUE(lookup);
  EXPECT_TRUE(lookup.isAbstract());
}
