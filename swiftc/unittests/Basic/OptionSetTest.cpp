//===--- OptionSetTest.cpp - Tests for option set utilities --------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include <gtest/gtest.h>
#include <cstdint>

using namespace std;

// Simple option set implementation for testing
enum class CompilerOptions : uint32_t {
  None = 0,
  Debug = 1 << 0,
  Optimize = 1 << 1,
  Warnings = 1 << 2,
  Verbose = 1 << 3,
  EmitIR = 1 << 4,
  EmitSIL = 1 << 5
};

class OptionSet {
  uint32_t value;
  
public:
  OptionSet(uint32_t val = 0) : value(val) {}
  OptionSet(CompilerOptions option) : value(static_cast<uint32_t>(option)) {}
  
  bool has(CompilerOptions option) const {
    return (value & static_cast<uint32_t>(option)) != 0;
  }
  
  void set(CompilerOptions option) {
    value |= static_cast<uint32_t>(option);
  }
  
  void clear(CompilerOptions option) {
    value &= ~static_cast<uint32_t>(option);
  }
  
  void toggle(CompilerOptions option) {
    value ^= static_cast<uint32_t>(option);
  }
  
  bool empty() const { return value == 0; }
  uint32_t raw() const { return value; }
  
  OptionSet operator|(CompilerOptions option) const {
    return OptionSet(value | static_cast<uint32_t>(option));
  }
  
  OptionSet operator&(CompilerOptions option) const {
    return OptionSet(value & static_cast<uint32_t>(option));
  }
};

TEST(OptionSetTest, BasicOperations) {
  OptionSet options;
  
  EXPECT_TRUE(options.empty());
  EXPECT_FALSE(options.has(CompilerOptions::Debug));
  
  options.set(CompilerOptions::Debug);
  EXPECT_FALSE(options.empty());
  EXPECT_TRUE(options.has(CompilerOptions::Debug));
  EXPECT_FALSE(options.has(CompilerOptions::Optimize));
  
  options.set(CompilerOptions::Optimize);
  EXPECT_TRUE(options.has(CompilerOptions::Debug));
  EXPECT_TRUE(options.has(CompilerOptions::Optimize));
}

TEST(OptionSetTest, ClearOperations) {
  OptionSet options;
  options.set(CompilerOptions::Debug);
  options.set(CompilerOptions::Warnings);
  
  EXPECT_TRUE(options.has(CompilerOptions::Debug));
  EXPECT_TRUE(options.has(CompilerOptions::Warnings));
  
  options.clear(CompilerOptions::Debug);
  EXPECT_FALSE(options.has(CompilerOptions::Debug));
  EXPECT_TRUE(options.has(CompilerOptions::Warnings));
  
  options.clear(CompilerOptions::Warnings);
  EXPECT_FALSE(options.has(CompilerOptions::Warnings));
  EXPECT_TRUE(options.empty());
}

TEST(OptionSetTest, ToggleOperations) {
  OptionSet options;
  
  EXPECT_FALSE(options.has(CompilerOptions::Verbose));
  
  options.toggle(CompilerOptions::Verbose);
  EXPECT_TRUE(options.has(CompilerOptions::Verbose));
  
  options.toggle(CompilerOptions::Verbose);
  EXPECT_FALSE(options.has(CompilerOptions::Verbose));
}

TEST(OptionSetTest, BitwiseOperators) {
  OptionSet options;
  
  auto debugOptions = options | CompilerOptions::Debug;
  EXPECT_TRUE(debugOptions.has(CompilerOptions::Debug));
  EXPECT_FALSE(options.has(CompilerOptions::Debug)); // Original unchanged
  
  options.set(CompilerOptions::Debug);
  options.set(CompilerOptions::Optimize);
  
  auto debugOnly = options & CompilerOptions::Debug;
  EXPECT_TRUE(debugOnly.has(CompilerOptions::Debug));
  EXPECT_FALSE(debugOnly.has(CompilerOptions::Optimize));
}

TEST(OptionSetTest, MultipleOptions) {
  OptionSet options;
  
  options.set(CompilerOptions::Debug);
  options.set(CompilerOptions::Warnings);
  options.set(CompilerOptions::Verbose);
  
  EXPECT_TRUE(options.has(CompilerOptions::Debug));
  EXPECT_TRUE(options.has(CompilerOptions::Warnings));
  EXPECT_TRUE(options.has(CompilerOptions::Verbose));
  EXPECT_FALSE(options.has(CompilerOptions::Optimize));
  EXPECT_FALSE(options.has(CompilerOptions::EmitIR));
}

TEST(OptionSetTest, RawValueAccess) {
  OptionSet options;
  
  EXPECT_EQ(options.raw(), 0u);
  
  options.set(CompilerOptions::Debug);  // bit 0
  EXPECT_EQ(options.raw(), 1u);
  
  options.set(CompilerOptions::Optimize);  // bit 1
  EXPECT_EQ(options.raw(), 3u);  // 0b11
  
  options.set(CompilerOptions::EmitIR);  // bit 4
  EXPECT_EQ(options.raw(), 19u);  // 0b10011
}