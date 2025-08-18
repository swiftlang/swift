//===--- UnicodeTest.cpp - Tests for Unicode utilities -------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include <gtest/gtest.h>
#include <string>
#include <vector>

using namespace std;

// Basic Unicode utility functions for testing
namespace {
  bool isValidUTF8(const string& str) {
    // Simple UTF-8 validation
    for (size_t i = 0; i < str.size(); ) {
      unsigned char c = str[i];
      
      if (c < 0x80) {
        // ASCII character
        i++;
      } else if ((c & 0xE0) == 0xC0) {
        // 2-byte sequence
        if (i + 1 >= str.size() || (str[i + 1] & 0xC0) != 0x80) {
          return false;
        }
        i += 2;
      } else if ((c & 0xF0) == 0xE0) {
        // 3-byte sequence
        if (i + 2 >= str.size() || 
            (str[i + 1] & 0xC0) != 0x80 || 
            (str[i + 2] & 0xC0) != 0x80) {
          return false;
        }
        i += 3;
      } else if ((c & 0xF8) == 0xF0) {
        // 4-byte sequence
        if (i + 3 >= str.size() || 
            (str[i + 1] & 0xC0) != 0x80 || 
            (str[i + 2] & 0xC0) != 0x80 || 
            (str[i + 3] & 0xC0) != 0x80) {
          return false;
        }
        i += 4;
      } else {
        return false;
      }
    }
    return true;
  }
  
  size_t countUTF8Characters(const string& str) {
    size_t count = 0;
    for (size_t i = 0; i < str.size(); ) {
      unsigned char c = str[i];
      
      if (c < 0x80) {
        i++;
      } else if ((c & 0xE0) == 0xC0) {
        i += 2;
      } else if ((c & 0xF0) == 0xE0) {
        i += 3;
      } else if ((c & 0xF8) == 0xF0) {
        i += 4;
      } else {
        i++; // Invalid byte, skip
      }
      count++;
    }
    return count;
  }
}

TEST(UnicodeTest, ASCIIValidation) {
  EXPECT_TRUE(isValidUTF8("Hello, World!"));
  EXPECT_TRUE(isValidUTF8(""));
  EXPECT_TRUE(isValidUTF8("123456789"));
  EXPECT_TRUE(isValidUTF8("func main() { print(\"test\") }"));
}

TEST(UnicodeTest, UTF8Validation) {
  // Valid UTF-8 sequences
  EXPECT_TRUE(isValidUTF8("café"));           // é = 0xC3 0xA9
  EXPECT_TRUE(isValidUTF8("🚀"));             // Rocket emoji
  EXPECT_TRUE(isValidUTF8("你好"));            // Chinese characters
  EXPECT_TRUE(isValidUTF8("Здравствуй"));     // Cyrillic
  
  // Test with Swift string literals
  EXPECT_TRUE(isValidUTF8("let message = \"Hello 世界\""));
}

TEST(UnicodeTest, CharacterCounting) {
  EXPECT_EQ(countUTF8Characters("Hello"), 5u);
  EXPECT_EQ(countUTF8Characters(""), 0u);
  EXPECT_EQ(countUTF8Characters("café"), 4u);      // 4 characters, not 5 bytes
  EXPECT_EQ(countUTF8Characters("🚀"), 1u);        // 1 character, 4 bytes
  EXPECT_EQ(countUTF8Characters("你好"), 2u);       // 2 characters
}

TEST(UnicodeTest, SwiftIdentifiers) {
  // Valid Swift identifier characters
  EXPECT_TRUE(isValidUTF8("variableName"));
  EXPECT_TRUE(isValidUTF8("_privateVar"));
  EXPECT_TRUE(isValidUTF8("MyClass"));
  EXPECT_TRUE(isValidUTF8("π"));               // Greek pi
  EXPECT_TRUE(isValidUTF8("함수"));             // Korean characters
}

TEST(UnicodeTest, StringLiterals) {
  // Test various string literal contents
  EXPECT_TRUE(isValidUTF8("\"Hello, 世界!\""));
  EXPECT_TRUE(isValidUTF8("\"Emoji: 🎉🚀💻\""));
  EXPECT_TRUE(isValidUTF8("\"Math: ∑∆π∞\""));
}

TEST(UnicodeTest, Comments) {
  // Test Unicode in comments
  EXPECT_TRUE(isValidUTF8("// This is a comment with émojis 🎯"));
  EXPECT_TRUE(isValidUTF8("/* Multi-line comment\n   with 中文 characters */"));
}