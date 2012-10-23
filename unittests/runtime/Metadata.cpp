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

#include "../runtime/LowLevel/Metadata.h"
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
