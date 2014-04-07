//===--- StringExtras.cpp - String Utilities ------------------------------===//
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
//
// This file implements utilities for working with words and camelCase
// names.
//
//===----------------------------------------------------------------------===//
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/Optional.h"
#include "clang/Basic/CharInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include <algorithm>

using namespace swift;
using namespace camel_case;

PrepositionKind swift::getPrepositionKind(StringRef word) {
#define DIRECTIONAL_PREPOSITION(Word)           \
  if (word.equals_lower(#Word))                 \
    return PK_Directional;
#define PREPOSITION(Word)                       \
  if (word.equals_lower(#Word))                 \
    return PK_Nondirectional;
#include "Prepositions.def"

  return PK_None;
}

void WordIterator::computeNextPosition() const {
  assert(Position < String.size() && "Already at end of string");

  // Skip over any uppercase letters at the beginning of the word.
  unsigned i = Position, n = String.size();
  while (i < n && clang::isUppercase(String[i]))
    ++i;

  // If there was more than one uppercase letter, this is an
  // acronym.
  if (i - Position > 1) {
    // If we hit the end of the string, that's it. Otherwise, this
    // word ends at the last uppercase letter, so that the next word
    // starts with the last uppercase letter.
    NextPosition = i == n? i : i-1;
    NextPositionValid = true;
    return;
  }

  // Skip non-uppercase letters.
  while (i < n && !clang::isUppercase(String[i]))
    ++i;

  NextPosition = i;
  NextPositionValid = true;
}

void WordIterator::computePrevPosition() const {
  assert(Position > 0 && "Already at beginning of string");

  // While we see non-uppercase letters, keep moving back.
  unsigned i = Position;
  while (i > 0 && !clang::isUppercase(String[i-1]))
    --i;

  // If we found any lowercase letters, this was a normal camel case
  // word (not an acronym).
  if (i < Position) {
    // If we hit the beginning of the string, that's it. Otherwise,
    // this word starts at the uppercase letter that terminated the
    // search above.
    PrevPosition = i == 0 ? 0 : i-1;
    PrevPositionValid = true;
    return;
  }

  // There were no lowercase letters, so this is an acronym. Keep
  // skipping uppercase letters.
  while (i > 0 && clang::isUppercase(String[i-1]))
    --i;

  PrevPosition = i;
  PrevPositionValid = true;
}

StringRef camel_case::getFirstWord(StringRef string) {
  if (string.empty())
    return "";

  return *WordIterator(string, 0);
}

StringRef camel_case::getLastWord(StringRef string) {
  if (string.empty())
    return "";

  return *--WordIterator(string, string.size());
}

bool camel_case::sameWordIgnoreFirstCase(StringRef word1, StringRef word2) {
  if (word1.size() != word2.size())
    return false;

  if (clang::toLowercase(word1[0]) != clang::toLowercase(word2[0]))
    return false;

  return word1.substr(1) == word2.substr(1);
}

StringRef camel_case::toLowercaseWord(StringRef string,
                                      SmallVectorImpl<char> &scratch) {
  if (string.empty())
    return string;

  // Already lowercase.
  if (!clang::isUppercase(string[0]))
    return string;

  // Acronym doesn't get lowercased.
  if (string.size() > 1 && clang::isUppercase(string[1]))
    return string;

  // Lowercase the first letter, append the rest.
  scratch.clear();
  scratch.push_back(clang::toLowercase(string[0]));
  scratch.append(string.begin() + 1, string.end());
  return StringRef(scratch.data(), scratch.size());
}

StringRef camel_case::toSentencecase(StringRef string, 
                                     SmallVectorImpl<char> &scratch) {
  if (string.empty())
    return string;

  // Can't be uppercased.
  if (!clang::isLowercase(string[0]))
    return string;

  // Uppercase the first letter, append the rest.
  scratch.clear();
  scratch.push_back(clang::toUppercase(string[0]));
  scratch.append(string.begin() + 1, string.end());
  return StringRef(scratch.data(), scratch.size());  
}

Words::reverse_iterator MultiWordMap::match(Words::reverse_iterator first,
                                            Words::reverse_iterator last) const{
  assert(first != last && "Empty sequence cannot be matched");

  // If the current word isn't at the start of the sequence, we're done.
  llvm::SmallString<32> scratch;
  auto known = Data.find(toLowercaseWord(*first, scratch));
  if (known == Data.end())
    return first;

  /// The best result (if we found anything) and the length of the match.
  Optional<std::pair<Words::reverse_iterator, unsigned>> bestResult;
  for (const auto &multiword : known->second) {
    // Match the words in the sequence to the words in this multiword.
    auto matchNext = first;
    ++matchNext;

    bool matched = true;
    for (const auto &word : multiword) {
      if (matchNext == last || !sameWordIgnoreFirstCase(word, *matchNext++)) {
        matched = false;
        break;
      }
    }

    if (!matched)
      continue;

    // If this is the first result, or the previous result was a shorter match,
    // we just found a better result.
    if (!bestResult || bestResult->second < multiword.size()) {
      bestResult = { std::prev(matchNext), multiword.size() };
    }
  }

  return bestResult ? bestResult->first : first;
}
