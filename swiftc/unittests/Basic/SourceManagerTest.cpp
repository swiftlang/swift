//===--- SourceManagerTest.cpp - Tests for SourceManager -----------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Basic/SourceManager.h"
#include <gtest/gtest.h>
#include <llvm/Support/MemoryBuffer.h>

using namespace swiftc;

class SourceManagerTest : public ::testing::Test {
protected:
  SourceManager SM;
};

TEST_F(SourceManagerTest, BasicFileAddition) {
  std::string content = "let x = 42\nprint(x)";
  auto buffer = llvm::MemoryBuffer::getMemBuffer(content, "test.swift");
  
  SourceLoc startLoc = SM.addSourceFile(std::move(buffer), "test.swift");
  
  EXPECT_TRUE(startLoc.isValid());
  EXPECT_EQ(SM.getFilename(startLoc), "test.swift");
  EXPECT_EQ(SM.getBuffer(startLoc), "let x = 42\nprint(x)");
}

TEST_F(SourceManagerTest, MultipleFiles) {
  auto buffer1 = llvm::MemoryBuffer::getMemBuffer("file1 content", "file1.swift");
  auto buffer2 = llvm::MemoryBuffer::getMemBuffer("file2 content", "file2.swift");
  
  SourceLoc loc1 = SM.addSourceFile(std::move(buffer1), "file1.swift");
  SourceLoc loc2 = SM.addSourceFile(std::move(buffer2), "file2.swift");
  
  EXPECT_NE(loc1.getRawValue(), loc2.getRawValue());
  EXPECT_EQ(SM.getFilename(loc1), "file1.swift");
  EXPECT_EQ(SM.getFilename(loc2), "file2.swift");
  EXPECT_EQ(SM.getBuffer(loc1), "file1 content");
  EXPECT_EQ(SM.getBuffer(loc2), "file2 content");
}

TEST_F(SourceManagerTest, LineAndColumnCalculation) {
  std::string content = "line1\nline2\nline3";
  auto buffer = llvm::MemoryBuffer::getMemBuffer(content, "test.swift");
  
  SourceLoc startLoc = SM.addSourceFile(std::move(buffer), "test.swift");
  
  // Test first character
  auto [line1, col1] = SM.getLineAndColumn(startLoc);
  EXPECT_EQ(line1, 1u);
  EXPECT_EQ(col1, 1u);
  
  // Test character after first newline
  SourceLoc secondLineLoc(startLoc.getRawValue() + 6); // After "line1\n"
  auto [line2, col2] = SM.getLineAndColumn(secondLineLoc);
  EXPECT_EQ(line2, 2u);
  EXPECT_EQ(col2, 1u);
  
  // Test character in middle of second line
  SourceLoc midSecondLine(startLoc.getRawValue() + 8); // "line2" -> 'n'
  auto [line3, col3] = SM.getLineAndColumn(midSecondLine);
  EXPECT_EQ(line3, 2u);
  EXPECT_EQ(col3, 3u);
}

TEST_F(SourceManagerTest, CharacterAccess) {
  std::string content = "Hello, World!";
  auto buffer = llvm::MemoryBuffer::getMemBuffer(content, "test.swift");
  
  SourceLoc startLoc = SM.addSourceFile(std::move(buffer), "test.swift");
  
  EXPECT_EQ(SM.getCharacterAt(startLoc), 'H');
  EXPECT_EQ(SM.getCharacterAt(SourceLoc(startLoc.getRawValue() + 7)), 'W');
  EXPECT_EQ(SM.getCharacterAt(SourceLoc(startLoc.getRawValue() + 12)), '!');
}

TEST_F(SourceManagerTest, EndOfFileDetection) {
  std::string content = "test";
  auto buffer = llvm::MemoryBuffer::getMemBuffer(content, "test.swift");
  
  SourceLoc startLoc = SM.addSourceFile(std::move(buffer), "test.swift");
  
  EXPECT_FALSE(SM.isAtEndOfFile(startLoc));
  EXPECT_FALSE(SM.isAtEndOfFile(SourceLoc(startLoc.getRawValue() + 3)));
  EXPECT_TRUE(SM.isAtEndOfFile(SourceLoc(startLoc.getRawValue() + 4)));
  EXPECT_TRUE(SM.isAtEndOfFile(SourceLoc(startLoc.getRawValue() + 100)));
}

TEST_F(SourceManagerTest, InvalidLocations) {
  SourceLoc invalidLoc;
  
  EXPECT_EQ(SM.getSourceFile(invalidLoc), nullptr);
  EXPECT_EQ(SM.getBuffer(invalidLoc), "");
  EXPECT_EQ(SM.getFilename(invalidLoc), "");
  EXPECT_EQ(SM.getCharacterAt(invalidLoc), '\0');
  EXPECT_TRUE(SM.isAtEndOfFile(invalidLoc));
  
  auto [line, col] = SM.getLineAndColumn(invalidLoc);
  EXPECT_EQ(line, 0u);
  EXPECT_EQ(col, 0u);
}

TEST_F(SourceManagerTest, EmptyFile) {
  auto buffer = llvm::MemoryBuffer::getMemBuffer("", "empty.swift");
  
  SourceLoc startLoc = SM.addSourceFile(std::move(buffer), "empty.swift");
  
  EXPECT_TRUE(startLoc.isValid());
  EXPECT_EQ(SM.getBuffer(startLoc), "");
  EXPECT_TRUE(SM.isAtEndOfFile(startLoc));
  
  auto [line, col] = SM.getLineAndColumn(startLoc);
  EXPECT_EQ(line, 1u);
  EXPECT_EQ(col, 1u);
}