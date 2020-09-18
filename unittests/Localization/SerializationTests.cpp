//===--- LocalizationProducerTests.cpp -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "LocalizationTest.h"
#include "swift/Localization/LocalizationFormat.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"
#include <string>
#include <random>

using namespace swift::diag;
using namespace swift::unittests;

TEST_F(LocalizationTest, TestYAMLSerialization) {
  YAMLLocalizationProducer yaml(YAMLPath);

  auto dbFile = createTemporaryFile("en", "db");

  // First, let's serialize English translations
  {
    SerializedLocalizationWriter writer;

    yaml.forEachAvailable([&writer](swift::DiagID id, llvm::StringRef translation) {
                            writer.insert(id, translation);
                          });

    ASSERT_FALSE(writer.emit(dbFile));
  }

  // Now, let's make sure that serialized version matches "source" YAML
  auto dbContent = llvm::MemoryBuffer::getFile(dbFile);
  ASSERT_TRUE(dbContent);

  SerializedLocalizationProducer db(std::move(dbContent.get()));
  yaml.forEachAvailable([&db](swift::DiagID id, llvm::StringRef translation) {
    ASSERT_EQ(translation, db.getMessageOr(id, "<no-fallback>"));
  });
}

TEST_F(LocalizationTest, TestSerializationOfEmptyFile) {
  auto dbFile = createTemporaryFile("by", "db");
  SerializedLocalizationWriter writer;
  ASSERT_FALSE(writer.emit(dbFile));

  YAMLLocalizationProducer yaml(YAMLPath);

  // Reading of the empty `db` file should always return default message.
  {
    auto dbContent = llvm::MemoryBuffer::getFile(dbFile);
    ASSERT_TRUE(dbContent);

    SerializedLocalizationProducer db(std::move(dbContent.get()));
    yaml.forEachAvailable([&db](swift::DiagID id, llvm::StringRef translation) {
      ASSERT_EQ("<<<default-fallback>>>",
                db.getMessageOr(id, "<<<default-fallback>>>"));
    });
  }
}

TEST_F(LocalizationTest, TestSerializationWithGaps) {
  // Initially all of the messages are included.
  llvm::SmallBitVector includedMessages(LocalDiagID::NumDiags, true);

  // Let's punch some holes in the diagnostic content.
  for (unsigned i = 0, n = 200; i != n; ++i) {
    unsigned position = RandNumber(LocalDiagID::NumDiags);
    includedMessages.flip(position);
  }

  YAMLLocalizationProducer yaml(YAMLPath);
  auto dbFile = createTemporaryFile("en", "db");

  {
    SerializedLocalizationWriter writer;

    yaml.forEachAvailable([&](swift::DiagID id, llvm::StringRef translation) {
      if (includedMessages.test((unsigned)id))
        writer.insert(id, translation);
    });

    ASSERT_FALSE(writer.emit(dbFile));
  }


  {
    auto dbContent = llvm::MemoryBuffer::getFile(dbFile);
    ASSERT_TRUE(dbContent);

    SerializedLocalizationProducer db(std::move(dbContent.get()));
    yaml.forEachAvailable([&](swift::DiagID id, llvm::StringRef translation) {
      auto position = (unsigned)id;

      std::string expectedMessage = includedMessages.test(position)
                                        ? std::string(translation)
                                        : "<<<default-fallback>>>";

      ASSERT_EQ(expectedMessage, db.getMessageOr(id, "<<<default-fallback>>>"));
    });
  }
}
