//===--- DiagnosticTest.cpp - Tests for DiagnosticEngine -----------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Basic/Diagnostic.h"
#include <gtest/gtest.h>

using namespace swiftc;

class DiagnosticTest : public ::testing::Test {
protected:
  DiagnosticEngine DE;
};

TEST_F(DiagnosticTest, BasicDiagnostics) {
  SourceLoc loc(100);
  
  DE.diagnoseNote(loc, "This is a note");
  DE.diagnoseWarning(loc, "This is a warning");
  DE.diagnoseError(loc, "This is an error");
  
  const auto& diagnostics = DE.getDiagnostics();
  EXPECT_EQ(diagnostics.size(), 3u);
  
  EXPECT_EQ(diagnostics[0].Level, DiagnosticLevel::Note);
  EXPECT_EQ(diagnostics[0].Location.getRawValue(), 100u);
  EXPECT_EQ(diagnostics[0].Message, "This is a note");
  
  EXPECT_EQ(diagnostics[1].Level, DiagnosticLevel::Warning);
  EXPECT_EQ(diagnostics[1].Message, "This is a warning");
  
  EXPECT_EQ(diagnostics[2].Level, DiagnosticLevel::Error);
  EXPECT_EQ(diagnostics[2].Message, "This is an error");
}

TEST_F(DiagnosticTest, ErrorTracking) {
  SourceLoc loc(50);
  
  EXPECT_FALSE(DE.hasErrors());
  
  DE.diagnoseNote(loc, "Just a note");
  EXPECT_FALSE(DE.hasErrors());
  
  DE.diagnoseWarning(loc, "A warning");
  EXPECT_FALSE(DE.hasErrors());
  
  DE.diagnoseError(loc, "An error occurred");
  EXPECT_TRUE(DE.hasErrors());
}

TEST_F(DiagnosticTest, ClearDiagnostics) {
  SourceLoc loc(25);
  
  DE.diagnoseError(loc, "Error message");
  EXPECT_TRUE(DE.hasErrors());
  EXPECT_EQ(DE.getDiagnostics().size(), 1u);
  
  DE.clear();
  EXPECT_FALSE(DE.hasErrors());
  EXPECT_EQ(DE.getDiagnostics().size(), 0u);
}

TEST_F(DiagnosticTest, MultipleDiagnostics) {
  SourceLoc loc1(10);
  SourceLoc loc2(20);
  SourceLoc loc3(30);
  
  DE.diagnose(DiagnosticLevel::Error, loc1, "First error");
  DE.diagnose(DiagnosticLevel::Warning, loc2, "Warning message");
  DE.diagnose(DiagnosticLevel::Error, loc3, "Second error");
  DE.diagnose(DiagnosticLevel::Note, loc1, "Related note");
  
  EXPECT_TRUE(DE.hasErrors());
  
  const auto& diagnostics = DE.getDiagnostics();
  EXPECT_EQ(diagnostics.size(), 4u);
  
  // Check ordering is preserved
  EXPECT_EQ(diagnostics[0].Level, DiagnosticLevel::Error);
  EXPECT_EQ(diagnostics[0].Location.getRawValue(), 10u);
  
  EXPECT_EQ(diagnostics[1].Level, DiagnosticLevel::Warning);
  EXPECT_EQ(diagnostics[1].Location.getRawValue(), 20u);
  
  EXPECT_EQ(diagnostics[2].Level, DiagnosticLevel::Error);
  EXPECT_EQ(diagnostics[2].Location.getRawValue(), 30u);
  
  EXPECT_EQ(diagnostics[3].Level, DiagnosticLevel::Note);
  EXPECT_EQ(diagnostics[3].Location.getRawValue(), 10u);
}

TEST_F(DiagnosticTest, InvalidSourceLocation) {
  SourceLoc invalidLoc;
  
  DE.diagnoseError(invalidLoc, "Error with invalid location");
  
  const auto& diagnostics = DE.getDiagnostics();
  EXPECT_EQ(diagnostics.size(), 1u);
  EXPECT_TRUE(diagnostics[0].Location.isInvalid());
  EXPECT_EQ(diagnostics[0].Message, "Error with invalid location");
}