//===----------------------------------------------------------------------===//
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

#include "SourceKit/Support/ImmutableTextBuffer.h"
#include "gtest/gtest.h"

using namespace SourceKit;
using namespace llvm;

TEST(EditableTextBuffer, Updates) {
  const char *Text = "hello world";

  EditableTextBufferManager BufMgr;
  EditableTextBufferRef EdBuf = BufMgr.getOrCreateBuffer("/a/test", Text);
  ImmutableTextBufferRef Buf = EdBuf->getBuffer();

  EXPECT_EQ(Buf->getText(), Text);

  Buf = EdBuf->insert(6, "all ")->getBuffer();
  EXPECT_EQ(Buf->getText(), "hello all world");

  Buf = EdBuf->erase(9, 6)->getBuffer();
  EXPECT_EQ(Buf->getText(), "hello all");

  Buf = EdBuf->replace(0, 5, "yo")->getBuffer();
  EXPECT_EQ(Buf->getText(), "yo all");

  EdBuf = BufMgr.resetBuffer("/a/test", Text);
  EdBuf->insert(6, "all ");
  EdBuf->erase(9, 6);
  EdBuf->replace(0, 5, "yo");
  Buf = EdBuf->getSnapshot()->getBuffer();
  EXPECT_EQ(Buf->getText(), "yo all");

  EXPECT_EQ(Buf->getFilename(), "/a/test");
}
