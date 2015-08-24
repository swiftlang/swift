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
#include "clang/Basic/CharInfo.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSwitch.h"
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

  unsigned i = Position, n = String.size();

  // Treat _ as a word on its own. Don't coalesce.
  if (String[i] == '_') {
    NextPosition = i + 1;
    NextPositionValid = true;
    return;
  }

  // Skip over any uppercase letters at the beginning of the word.
  while (i < n && clang::isUppercase(String[i]))
    ++i;

  // If there was more than one uppercase letter, this is an
  // acronym.
  if (i - Position > 1) {
    // If we hit the end of the string, that's it. Otherwise, this
    // word ends before the last uppercase letter if the next word is alphabetic
    // (URL_Loader) or after the last uppercase letter if it's not (UTF_8).
    NextPosition = (i == n || !clang::isLowercase(String[i])) ? i : i-1;
    NextPositionValid = true;
    return;
  }

  // Skip non-uppercase letters.
  while (i < n && !clang::isUppercase(String[i]) && String[i] != '_')
    ++i;

  NextPosition = i;
  NextPositionValid = true;
}

void WordIterator::computePrevPosition() const {
  assert(Position > 0 && "Already at beginning of string");

  unsigned i = Position;

  // While we see non-uppercase letters, keep moving back.
  while (i > 0 && !clang::isUppercase(String[i-1]) && String[i-1] != '_')
    --i;

  // If we found any lowercase letters, this was a normal camel case
  // word (not an acronym).
  if (i < Position) {
    // If we hit the beginning of the string, that's it. Otherwise, this
    // word starts with an uppercase letter if the next word is alphabetic
    // (URL_Loader) or after the last uppercase letter if it's not (UTF_8).
    PrevPosition = i;
    if (i != 0 && clang::isLowercase(String[i]) && String[i-1] != '_')
      --PrevPosition;
    PrevPositionValid = true;
    return;
  }

  // Treat _ as a word on its own. Don't coalesce.
  if (String[i-1] == '_') {
    PrevPosition = i - 1;
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

bool camel_case::startsWithIgnoreFirstCase(StringRef word1, StringRef word2) {
  if (word1.size() < word2.size())
    return false;

  if (clang::toLowercase(word1[0]) != clang::toLowercase(word2[0]))
    return false;

  return word1.substr(1) == word2.substr(1, word1.size() - 1);
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

StringRef camel_case::dropPrefix(StringRef string) {

  unsigned firstLower = 0, n = string.size();

  if (n < 4)
    return string;

  for (; firstLower < n; ++firstLower) {
    if (!clang::isUppercase(string[firstLower]))
      break;
  }

  if (firstLower == n)
    return string;

  if (firstLower >= 3 && firstLower <= 4)
    return string.substr(firstLower - 1);

  return string;
}

StringRef camel_case::appendSentenceCase(SmallVectorImpl<char> &buffer,
                                         StringRef string) {
  // Trivial case: empty string.
  if (string.empty())
    return StringRef(buffer.data(), buffer.size());

  // Uppercase the first letter, append the rest.
  buffer.push_back(clang::toUppercase(string[0]));
  buffer.append(string.begin() + 1, string.end());
  return StringRef(buffer.data(), buffer.size());
}

size_t camel_case::findWord(StringRef string, StringRef word) {
  assert(!word.empty());
  assert(clang::isUppercase(word[0]));

  // Scan forward until we find the word as a complete word.
  size_t startingIndex = 0;
  while (true) {
    size_t index = string.find(word, startingIndex);
    if (index == StringRef::npos)
      return StringRef::npos;

    // If any of the following checks fail, we want to start searching
    // past the end of the match.  (This assumes that the word doesn't
    // end with a prefix of itself, e.g. "LikeableLike".)
    startingIndex = index + word.size();

    // We assume that we don't have to check if the match starts a new
    // word in the string.

    // If we find the word, check whether it's a valid match.
    StringRef suffix = string.substr(index);
    if (!suffix.empty() && clang::isLowercase(suffix[0]))
      continue;

    return index;
  }
}

/// Determine whether the given identifier is a keyword.
static bool isKeyword(StringRef identifier) {
  return llvm::StringSwitch<bool>(identifier)
#define KEYWORD(kw) .Case(#kw, true)
#define SIL_KEYWORD(kw)
#include "swift/Parse/Tokens.def"
    .Default(false);
}

/// Skip a type suffix that can be dropped.
static Optional<StringRef> skipTypeSuffix(StringRef typeName) {
  if (typeName.empty()) return None;

  // "Ptr" and "Ref" are common suffixes we can ignore.
  if (typeName.endswith("Ref") || typeName.endswith("Ptr")) {
    return typeName.drop_back(3);
  }

  /// \d+D for dimensionality.
  if (typeName.back() == 'D' && typeName.size() > 1) {
    unsigned firstDigit = typeName.size() - 1;
    while (firstDigit > 0) {
      if (!isdigit(typeName[firstDigit-1])) break;
      --firstDigit;
    }

    if (firstDigit < typeName.size()-1) {
      return typeName.substr(0, firstDigit);
    }
  }

  return None;
}

StringRef swift::omitNeedlessWords(StringRef name, StringRef typeName,
                                   NameRole role) {
  if (name.empty() || typeName.empty()) return name;

  // Get the camel-case words in the name and type name.
  auto nameWords = camel_case::getWords(name);
  auto typeWords = camel_case::getWords(typeName);

  // Match the last words in the type name to the last words in the
  // name.
  auto nameWordRevIter = nameWords.rbegin(),
    nameWordRevIterBegin = nameWordRevIter,
    nameWordRevIterEnd = nameWords.rend();
  auto typeWordRevIter = typeWords.rbegin(),
    typeWordRevIterEnd = typeWords.rend();
  bool anyMatches = false;
  while (nameWordRevIter != nameWordRevIterEnd &&
         typeWordRevIter != typeWordRevIterEnd) {
    // If the names match, continue.
    if (camel_case::sameWordIgnoreFirstCase(*nameWordRevIter,
                                            *typeWordRevIter)) {
      anyMatches = true;
      ++nameWordRevIter;
      ++typeWordRevIter;
      continue;
    }

    // Special case: "Indexes" and "Indices" in the name match
    // "IndexSet" in the type.
    if ((camel_case::sameWordIgnoreFirstCase(*nameWordRevIter, "Indexes") ||
         camel_case::sameWordIgnoreFirstCase(*nameWordRevIter, "Indices")) &&
        *typeWordRevIter == "Set") {
      auto nextTypeWordRevIter = typeWordRevIter;
      ++nextTypeWordRevIter;
      if (nextTypeWordRevIter != typeWordRevIterEnd &&
          camel_case::sameWordIgnoreFirstCase(*nextTypeWordRevIter, "Index")) {
        anyMatches = true;
        ++nameWordRevIter;
        typeWordRevIter = nextTypeWordRevIter;
        ++typeWordRevIter;
        continue;
      }
    }

    // If this is a skippable suffix, skip it and keep looking.
    if (nameWordRevIter == nameWordRevIterBegin) {
      if (auto withoutSuffix = skipTypeSuffix(typeName)) {
        typeName = *withoutSuffix;
        typeWords = camel_case::getWords(typeName);
        typeWordRevIter = typeWords.rbegin();
        typeWordRevIterEnd = typeWords.rend();
        continue;
      }
    }

    break;
  }

  // If nothing matched, there is nothing to omit.
  if (!anyMatches) return name;

  // Handle complete name matches.
  if (nameWordRevIter == nameWordRevIterEnd) {
    // If this is the first parameter, it's okay to drop the name
    // entirely.
    if (role == NameRole::FirstParameter) return "";

    // Otherwise, leave the name alone.
    return name;
  }

  // If the word preceding the match is "With", and it isn't the first
  // word, we can drop it as well.
  auto nextNameWordRevIter = nameWordRevIter;
  ++nextNameWordRevIter;
  if (*nameWordRevIter == "With") {
    ++nameWordRevIter;

    // If we hit the beginning of the word, step back to keep the
    // "with". This will only actually happen if the name isn't
    // following the camel-casing rules correctly.
    if (nameWordRevIter == nameWordRevIterEnd) --nameWordRevIter;
  }

  // Go back to the last matching word and chop off the name at that
  // point.
  StringRef newName = name.substr(0, nameWordRevIter.base().getPosition());

  // If we ended up with a keyword or a name like "get" or "set", do nothing.
  if (isKeyword(newName) || newName == "get" || newName == "set")
    return name;

  // We're done.
  return newName;
}

bool swift::omitNeedlessWords(StringRef &baseName,
                              MutableArrayRef<StringRef> argNames,
                              StringRef resultType,
                              StringRef contextType,
                              ArrayRef<StringRef> paramTypes,
                              bool returnsSelf,
                              bool failableInitializer) {
  // For zero-parameter methods that return 'Self' or a result type
  // that matches the declaration context, omit needless words from
  // the base name.
  if (paramTypes.empty()) {
    if (returnsSelf || (resultType == contextType)) {
      StringRef typeName = returnsSelf ? contextType : resultType;
      if (typeName.empty())
        return false;

      StringRef oldBaseName = baseName;
      baseName = omitNeedlessWords(baseName, typeName, NameRole::BaseName);
      return baseName != oldBaseName;
    }

    return false;
  }

  // Omit needless words based on parameter types.
  bool anyChanges = false;
  for (unsigned i = 0, n = argNames.size(); i != n; ++i) {
    // If there is no corresponding parameter, there is nothing to
    // omit.
    if (i >= paramTypes.size()) continue;

    // Omit needless words based on the type of the parameter.
    NameRole role = i > 0 ? NameRole::SubsequentParameter
      : argNames[0].empty() ? NameRole::BaseName
      : failableInitializer ? NameRole::SubsequentParameter
      : NameRole::FirstParameter;

    StringRef name = role == NameRole::BaseName ? baseName : argNames[i];
    StringRef newName = omitNeedlessWords(name, paramTypes[i], role);

    if (name == newName) continue;

    anyChanges = true;

    // Record this change.
    if (role == NameRole::BaseName) {
      baseName = newName;
    } else {
      argNames[i] = newName;
    }
  }

  return anyChanges;
}
