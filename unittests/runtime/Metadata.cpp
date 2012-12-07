//===- swift/unittests/runtime/Metadata.cpp - Metadata tests --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Metadata.h"
#include "gtest/gtest.h"

using namespace swift;

/// Some unique global pointers.
char Global1 = 0;
char Global2 = 0;
char Global3 = 0;

/// The general structure of a generic metadata.
template <unsigned NumFillOps, unsigned NumFields>
struct GenericMetadataTest {
  GenericMetadata Header;
  GenericMetadata::FillOp FillOps[NumFillOps];
  void *Fields[NumFields];
};

GenericMetadataTest<2,3> MetadataTest1 = {
  // Header
  {
    1, // num arguments
    2, // num fill ops
    3 * sizeof(void*), // metadata size
    0, // address point
    {} // private data
  },

  // Fill ops
  { 
    { 0, 0 },
    { 0, 2 }
  },

  // Fields
  {
    nullptr,
    &Global1,
    nullptr
  }
};

TEST(MetadataTest, getGenericMetadata) {
  auto metadataTemplate = (GenericMetadata*) &MetadataTest1;

  void *args[] = { &Global2 };

  auto inst1a = swift_getGenericMetadata(metadataTemplate, args);
  auto inst1b = swift_getGenericMetadata(metadataTemplate, args);
  ASSERT_EQ(inst1a, inst1b);

  void * const *fields = reinterpret_cast<void * const *>(inst1a);
  ASSERT_EQ(&Global2, fields[0]);
  ASSERT_EQ(&Global1, fields[1]);
  ASSERT_EQ(&Global2, fields[2]);

  args[0] = &Global3;
  auto inst2a = swift_getGenericMetadata(metadataTemplate, args);
  auto inst2b = swift_getGenericMetadata(metadataTemplate, args);
  ASSERT_EQ(inst2a, inst2b);
  ASSERT_NE(inst1a, inst2a);

  fields = reinterpret_cast<void * const *>(inst2a);
  ASSERT_EQ(&Global3, fields[0]);
  ASSERT_EQ(&Global1, fields[1]);
  ASSERT_EQ(&Global3, fields[2]);  
}

FullMetadata<ClassMetadata> MetadataTest2;

TEST(MetadataTest, getMetatypeMetadata) {
  auto inst1a = swift_getMetatypeMetadata(&_TMdBi64_.base);
  auto inst1b = swift_getMetatypeMetadata(&_TMdBi64_.base);
  ASSERT_EQ(inst1a, inst1b);

  auto inst2a = swift_getMetatypeMetadata(&_TMdBi32_.base);
  auto inst2b = swift_getMetatypeMetadata(&_TMdBi32_.base);
  ASSERT_EQ(inst2a, inst2b);

  // Both of these are trivial metatypes.
  ASSERT_EQ(size_t(0), inst1a->getValueWitnesses()->size);
  ASSERT_EQ(size_t(0), inst2a->getValueWitnesses()->size);

  // Fill out a fake class metadata.
  MetadataTest2.Kind = MetadataKind::Class;
  MetadataTest2.ValueWitnesses = &_TWVBo;
  MetadataTest2.destroy = nullptr;
  MetadataTest2.SuperClass = nullptr;

  auto inst3a = swift_getMetatypeMetadata(&MetadataTest2);
  auto inst3b = swift_getMetatypeMetadata(&MetadataTest2);
  ASSERT_EQ(inst3a, inst3b);

  // The representation here should be non-trivial.
  ASSERT_EQ(sizeof(void*), inst3a->getValueWitnesses()->size);

  // Going out another level of abstraction on the class metatype
  // should leave us with another non-trivial metatype.
  auto inst4a = swift_getMetatypeMetadata(inst3a);
  auto inst4b = swift_getMetatypeMetadata(inst3a);
  ASSERT_EQ(inst4a, inst4b);
  ASSERT_EQ(sizeof(void*), inst4a->getValueWitnesses()->size);

  // Similarly, going out a level of abstraction on a trivial
  // metatype should give us a trivial metatype.
  auto inst5a = swift_getMetatypeMetadata(inst1a);
  auto inst5b = swift_getMetatypeMetadata(inst1a);
  ASSERT_EQ(inst5a, inst5b);
  ASSERT_EQ(size_t(0), inst5a->getValueWitnesses()->size);

  // After all this, the instance type fields should still be valid.
  ASSERT_EQ(&_TMdBi64_.base, inst1a->InstanceType);
  ASSERT_EQ(&_TMdBi32_.base, inst2a->InstanceType);
  ASSERT_EQ(&MetadataTest2, inst3a->InstanceType);
  ASSERT_EQ(inst3a, inst4a->InstanceType);
  ASSERT_EQ(inst1a, inst5a->InstanceType);
}
