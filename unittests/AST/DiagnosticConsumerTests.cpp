//===--- DiagnosticConsumerTests.cpp --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/Basic/SourceManager.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {
  using ExpectedDiagnostic = std::pair<SourceLoc, StringRef>;

  class ExpectationDiagnosticConsumer: public DiagnosticConsumer {
    ExpectationDiagnosticConsumer *previous;
    SmallVector<ExpectedDiagnostic, 4> expected;
    bool hasFinished = false;

  public:
    ExpectationDiagnosticConsumer(ExpectationDiagnosticConsumer *previous,
                                  ArrayRef<ExpectedDiagnostic> expected)
      : previous(previous), expected(expected.begin(), expected.end()) {}

    ~ExpectationDiagnosticConsumer() override {
      EXPECT_TRUE(hasFinished);
      EXPECT_TRUE(expected.empty());
    }

    void handleDiagnostic(SourceManager &SM, SourceLoc loc, DiagnosticKind kind,
                          StringRef formatString,
                          ArrayRef<DiagnosticArgument> formatArgs,
                          const DiagnosticInfo &info) override {
      ASSERT_FALSE(expected.empty());
      EXPECT_EQ(std::make_pair(loc, formatString), expected.front());
      expected.erase(expected.begin());
    }

    bool finishProcessing() override {
      EXPECT_FALSE(hasFinished);
      if (previous)
        EXPECT_TRUE(previous->hasFinished);
      hasFinished = true;
      return false;
    }
  };
} // end anonymous namespace

TEST(FileSpecificDiagnosticConsumer, SubConsumersFinishInOrder) {
  SourceManager sourceMgr;
  (void)sourceMgr.addMemBufferCopy("abcde", "A");
  (void)sourceMgr.addMemBufferCopy("vwxyz", "B");

  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, None);
  auto consumerUnaffiliated = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), None);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("", std::move(consumerUnaffiliated));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.finishProcessing();
}

TEST(FileSpecificDiagnosticConsumer, InvalidLocDiagsGoToEveryConsumer) {
  SourceManager sourceMgr;
  (void)sourceMgr.addMemBufferCopy("abcde", "A");
  (void)sourceMgr.addMemBufferCopy("vwxyz", "B");

  ExpectedDiagnostic expected[] = { {SourceLoc(), "dummy"} };
  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, expected);
  auto consumerUnaffiliated = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), expected);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("", std::move(consumerUnaffiliated));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.handleDiagnostic(sourceMgr, SourceLoc(), DiagnosticKind::Error,
                               "dummy", {}, DiagnosticInfo());
  topConsumer.finishProcessing();
}

TEST(FileSpecificDiagnosticConsumer, ErrorsWithLocationsGoToExpectedConsumers) {
  SourceManager sourceMgr;
  //                                             01234
  unsigned bufferA = sourceMgr.addMemBufferCopy("abcde", "A");
  unsigned bufferB = sourceMgr.addMemBufferCopy("vwxyz", "B");

  SourceLoc frontOfA = sourceMgr.getLocForOffset(bufferA, 0);
  SourceLoc middleOfA = sourceMgr.getLocForOffset(bufferA, 2);
  SourceLoc backOfA = sourceMgr.getLocForOffset(bufferA, 4);

  SourceLoc frontOfB = sourceMgr.getLocForOffset(bufferB, 0);
  SourceLoc middleOfB = sourceMgr.getLocForOffset(bufferB, 2);
  SourceLoc backOfB = sourceMgr.getLocForOffset(bufferB, 4);

  ExpectedDiagnostic expectedA[] = {
    {frontOfA, "front"},
    {middleOfA, "middle"},
    {backOfA, "back"},
  };
  ExpectedDiagnostic expectedB[] = {
    {frontOfB, "front"},
    {middleOfB, "middle"},
    {backOfB, "back"}
  };

  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, expectedA);
  auto consumerB = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), expectedB);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("B", std::move(consumerB));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "front", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfB, DiagnosticKind::Error,
                               "front", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfA, DiagnosticKind::Error,
                               "middle", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfB, DiagnosticKind::Error,
                               "middle", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Error,
                               "back", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfB, DiagnosticKind::Error,
                               "back", {}, DiagnosticInfo());
  topConsumer.finishProcessing();
}

TEST(FileSpecificDiagnosticConsumer,
     ErrorsInUnaffiliatedFilesGoToEveryConsumer) {
  SourceManager sourceMgr;
  //                                             01234
  unsigned bufferA = sourceMgr.addMemBufferCopy("abcde", "A");
  unsigned bufferB = sourceMgr.addMemBufferCopy("vwxyz", "B");

  SourceLoc frontOfA = sourceMgr.getLocForOffset(bufferA, 0);
  SourceLoc middleOfA = sourceMgr.getLocForOffset(bufferA, 2);
  SourceLoc backOfA = sourceMgr.getLocForOffset(bufferA, 4);

  SourceLoc frontOfB = sourceMgr.getLocForOffset(bufferB, 0);
  SourceLoc middleOfB = sourceMgr.getLocForOffset(bufferB, 2);
  SourceLoc backOfB = sourceMgr.getLocForOffset(bufferB, 4);

  ExpectedDiagnostic expectedA[] = {
    {frontOfA, "front"},
    {frontOfB, "front"},
    {middleOfA, "middle"},
    {middleOfB, "middle"},
    {backOfA, "back"},
    {backOfB, "back"}
  };
  ExpectedDiagnostic expectedUnaffiliated[] = {
    {frontOfB, "front"},
    {middleOfB, "middle"},
    {backOfB, "back"}
  };

  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, expectedA);
  auto consumerUnaffiliated = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), expectedUnaffiliated);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("", std::move(consumerUnaffiliated));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "front", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfB, DiagnosticKind::Error,
                               "front", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfA, DiagnosticKind::Error,
                               "middle", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfB, DiagnosticKind::Error,
                               "middle", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Error,
                               "back", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfB, DiagnosticKind::Error,
                               "back", {}, DiagnosticInfo());
  topConsumer.finishProcessing();
}

TEST(FileSpecificDiagnosticConsumer, WarningsAndRemarksAreTreatedLikeErrors) {
  SourceManager sourceMgr;
  //                                             01234
  unsigned bufferA = sourceMgr.addMemBufferCopy("abcde", "A");
  unsigned bufferB = sourceMgr.addMemBufferCopy("vwxyz", "B");

  SourceLoc frontOfA = sourceMgr.getLocForBufferStart(bufferA);
  SourceLoc frontOfB = sourceMgr.getLocForBufferStart(bufferB);

  ExpectedDiagnostic expectedA[] = {
    {frontOfA, "warning"},
    {frontOfB, "warning"},
    {frontOfA, "remark"},
    {frontOfB, "remark"},
  };
  ExpectedDiagnostic expectedUnaffiliated[] = {
    {frontOfB, "warning"},
    {frontOfB, "remark"},
  };

  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, expectedA);
  auto consumerUnaffiliated = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), expectedUnaffiliated);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("", std::move(consumerUnaffiliated));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Warning,
                               "warning", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfB, DiagnosticKind::Warning,
                               "warning", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Remark,
                               "remark", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfB, DiagnosticKind::Remark,
                               "remark", {}, DiagnosticInfo());
  topConsumer.finishProcessing();
}

TEST(FileSpecificDiagnosticConsumer, NotesAreAttachedToErrors) {
  SourceManager sourceMgr;
  //                                             01234
  unsigned bufferA = sourceMgr.addMemBufferCopy("abcde", "A");
  unsigned bufferB = sourceMgr.addMemBufferCopy("vwxyz", "B");

  SourceLoc frontOfA = sourceMgr.getLocForOffset(bufferA, 0);
  SourceLoc middleOfA = sourceMgr.getLocForOffset(bufferA, 2);
  SourceLoc backOfA = sourceMgr.getLocForOffset(bufferA, 4);

  SourceLoc frontOfB = sourceMgr.getLocForOffset(bufferB, 0);
  SourceLoc middleOfB = sourceMgr.getLocForOffset(bufferB, 2);
  SourceLoc backOfB = sourceMgr.getLocForOffset(bufferB, 4);

  ExpectedDiagnostic expectedA[] = {
    {frontOfA, "error"},
    {middleOfA, "note"},
    {backOfA, "note"},
    {frontOfB, "error"},
    {middleOfB, "note"},
    {backOfB, "note"},
    {frontOfA, "error"},
    {middleOfA, "note"},
    {backOfA, "note"},
  };
  ExpectedDiagnostic expectedUnaffiliated[] = {
    {frontOfB, "error"},
    {middleOfB, "note"},
    {backOfB, "note"},
  };

  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, expectedA);
  auto consumerUnaffiliated = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), expectedUnaffiliated);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("", std::move(consumerUnaffiliated));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfB, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.finishProcessing();
}

TEST(FileSpecificDiagnosticConsumer, NotesAreAttachedToWarningsAndRemarks) {
  SourceManager sourceMgr;
  //                                             01234
  unsigned bufferA = sourceMgr.addMemBufferCopy("abcde", "A");
  unsigned bufferB = sourceMgr.addMemBufferCopy("vwxyz", "B");

  SourceLoc frontOfA = sourceMgr.getLocForOffset(bufferA, 0);
  SourceLoc middleOfA = sourceMgr.getLocForOffset(bufferA, 2);
  SourceLoc backOfA = sourceMgr.getLocForOffset(bufferA, 4);

  SourceLoc frontOfB = sourceMgr.getLocForOffset(bufferB, 0);
  SourceLoc middleOfB = sourceMgr.getLocForOffset(bufferB, 2);
  SourceLoc backOfB = sourceMgr.getLocForOffset(bufferB, 4);

  ExpectedDiagnostic expectedA[] = {
    {frontOfA, "warning"},
    {middleOfA, "note"},
    {backOfA, "note"},
    {frontOfB, "warning"},
    {middleOfB, "note"},
    {backOfB, "note"},
    {frontOfA, "remark"},
    {middleOfA, "note"},
    {backOfA, "note"},
  };
  ExpectedDiagnostic expectedUnaffiliated[] = {
    {frontOfB, "warning"},
    {middleOfB, "note"},
    {backOfB, "note"},
  };

  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, expectedA);
  auto consumerUnaffiliated = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), expectedUnaffiliated);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("", std::move(consumerUnaffiliated));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Warning,
                               "warning", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfB, DiagnosticKind::Warning,
                               "warning", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Remark,
                               "remark", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.finishProcessing();
}

TEST(FileSpecificDiagnosticConsumer, NotesAreAttachedToErrorsEvenAcrossFiles) {
  SourceManager sourceMgr;
  //                                             01234
  unsigned bufferA = sourceMgr.addMemBufferCopy("abcde", "A");
  unsigned bufferB = sourceMgr.addMemBufferCopy("vwxyz", "B");

  SourceLoc frontOfA = sourceMgr.getLocForOffset(bufferA, 0);
  SourceLoc middleOfA = sourceMgr.getLocForOffset(bufferA, 2);
  SourceLoc backOfA = sourceMgr.getLocForOffset(bufferA, 4);

  SourceLoc frontOfB = sourceMgr.getLocForOffset(bufferB, 0);
  SourceLoc middleOfB = sourceMgr.getLocForOffset(bufferB, 2);
  SourceLoc backOfB = sourceMgr.getLocForOffset(bufferB, 4);

  ExpectedDiagnostic expectedA[] = {
    {frontOfA, "error"},
    {middleOfB, "note"},
    {backOfA, "note"},
    {frontOfA, "error"},
    {middleOfB, "note"},
    {backOfA, "note"},
  };
  ExpectedDiagnostic expectedB[] = {
    {frontOfB, "error"},
    {middleOfA, "note"},
    {backOfB, "note"},
  };

  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, expectedA);
  auto consumerB = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), expectedB);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("B", std::move(consumerB));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfB, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.finishProcessing();
}

TEST(FileSpecificDiagnosticConsumer,
     NotesAreAttachedToErrorsEvenAcrossFilesWithUnaffiliatedConsumer) {
  SourceManager sourceMgr;
  //                                             01234
  unsigned bufferA = sourceMgr.addMemBufferCopy("abcde", "A");
  unsigned bufferB = sourceMgr.addMemBufferCopy("vwxyz", "B");

  SourceLoc frontOfA = sourceMgr.getLocForOffset(bufferA, 0);
  SourceLoc middleOfA = sourceMgr.getLocForOffset(bufferA, 2);
  SourceLoc backOfA = sourceMgr.getLocForOffset(bufferA, 4);

  SourceLoc frontOfB = sourceMgr.getLocForOffset(bufferB, 0);
  SourceLoc middleOfB = sourceMgr.getLocForOffset(bufferB, 2);
  SourceLoc backOfB = sourceMgr.getLocForOffset(bufferB, 4);

  ExpectedDiagnostic expectedA[] = {
    {frontOfA, "error"},
    {middleOfB, "note"},
    {backOfA, "note"},
    {frontOfB, "error"},
    {middleOfA, "note"},
    {backOfB, "note"},
    {frontOfA, "error"},
    {middleOfB, "note"},
    {backOfA, "note"},
  };
  ExpectedDiagnostic expectedUnaffiliated[] = {
    {frontOfB, "error"},
    {middleOfA, "note"},
    {backOfB, "note"},
  };

  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, expectedA);
  auto consumerUnaffiliated = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), expectedUnaffiliated);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("", std::move(consumerUnaffiliated));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfB, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, middleOfB, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, backOfA, DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.finishProcessing();
}


TEST(FileSpecificDiagnosticConsumer,
     NotesWithInvalidLocsAreStillAttachedToErrors) {
  SourceManager sourceMgr;
  //                                             01234
  unsigned bufferA = sourceMgr.addMemBufferCopy("abcde", "A");
  unsigned bufferB = sourceMgr.addMemBufferCopy("vwxyz", "B");

  SourceLoc frontOfA = sourceMgr.getLocForBufferStart(bufferA);
  SourceLoc frontOfB = sourceMgr.getLocForBufferStart(bufferB);

  ExpectedDiagnostic expectedA[] = {
    {frontOfA, "error"},
    {SourceLoc(), "note"},
    {frontOfB, "error"},
    {SourceLoc(), "note"},
    {frontOfA, "error"},
    {SourceLoc(), "note"},
  };
  ExpectedDiagnostic expectedUnaffiliated[] = {
    {frontOfB, "error"},
    {SourceLoc(), "note"},
  };

  auto consumerA = llvm::make_unique<ExpectationDiagnosticConsumer>(
      nullptr, expectedA);
  auto consumerUnaffiliated = llvm::make_unique<ExpectationDiagnosticConsumer>(
      consumerA.get(), expectedUnaffiliated);

  SmallVector<FileSpecificDiagnosticConsumer::ConsumerPair, 2> consumers;
  consumers.emplace_back("A", std::move(consumerA));
  consumers.emplace_back("", std::move(consumerUnaffiliated));

  FileSpecificDiagnosticConsumer topConsumer(consumers);
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, SourceLoc(), DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfB, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, SourceLoc(), DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, frontOfA, DiagnosticKind::Error,
                               "error", {}, DiagnosticInfo());
  topConsumer.handleDiagnostic(sourceMgr, SourceLoc(), DiagnosticKind::Note,
                               "note", {}, DiagnosticInfo());
  topConsumer.finishProcessing();
}
