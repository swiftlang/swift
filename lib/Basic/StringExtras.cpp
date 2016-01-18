//===--- StringExtras.cpp - String Utilities ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "swift/Basic/Fallthrough.h"
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

bool swift::canBeArgumentLabel(StringRef identifier) {
  if (identifier == "var" || identifier == "let" || identifier == "inout")
    return false;

  return true;
}

PrepositionKind swift::getPrepositionKind(StringRef word) {
#define DIRECTIONAL_PREPOSITION(Word)           \
  if (word.equals_lower(#Word))                 \
    return PK_Directional;
#define PREPOSITION(Word)                       \
  if (word.equals_lower(#Word))                 \
    return PK_Nondirectional;
#include "PartsOfSpeech.def"

  return PK_None;
}

PartOfSpeech swift::getPartOfSpeech(StringRef word) {
  // FIXME: This implementation is woefully inefficient.
#define PREPOSITION(Word)                       \
  if (word.equals_lower(#Word))                 \
    return PartOfSpeech::Preposition;
#define VERB(Word)                              \
  if (word.equals_lower(#Word))                 \
    return PartOfSpeech::Verb;
#include "PartsOfSpeech.def"

  // Identify gerunds, which always end in "ing".
  if (word.endswith("ing") && word.size() > 4) {
    StringRef possibleVerb = word.substr(0, word.size()-3);

    // If what remains is a verb, we have a gerund.
    if (getPartOfSpeech(possibleVerb) == PartOfSpeech::Verb)
      return PartOfSpeech::Gerund;

    // Try adding an "e" and look for that as a verb.
    if (possibleVerb.back() != 'e') {
      SmallString<16> possibleVerbWithE;
      possibleVerbWithE += possibleVerb;
      possibleVerbWithE += 'e';
      if (getPartOfSpeech(possibleVerbWithE) == PartOfSpeech::Verb)
        return PartOfSpeech::Gerund;
    }

    // If there is a repeated letter at the back, drop that second
    // instance of that letter and try again.
    unsigned count = possibleVerb.size();
    if (possibleVerb[count-1] == possibleVerb[count-2] &&
        getPartOfSpeech(possibleVerb.substr(0, count-1)) == PartOfSpeech::Verb)
      return PartOfSpeech::Gerund;
  }

  // "auto" tends to be used as a verb prefix.
  if (word.startswith("auto") && word.size() > 4) {
    if (getPartOfSpeech(word.substr(4)) == PartOfSpeech::Verb)
      return PartOfSpeech::Verb;
  }

  // "re" can prefix a verb.
  if (word.startswith("re") && word.size() > 2) {
    if (getPartOfSpeech(word.substr(2)) == PartOfSpeech::Verb)
      return PartOfSpeech::Verb;
  }

  return PartOfSpeech::Unknown;
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

  return word1.substr(1, word2.size() - 1) == word2.substr(1);
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

  auto lastWord = camel_case::getLastWord(typeName);

  // "Type" suffix.
  if (lastWord == "Type" && typeName.size() > 4) {
    return typeName.drop_back(4);
  }

  // "Ref" suffix.
  if (lastWord == "Ref" && typeName.size() > 3) {
    return typeName.drop_back(3);
  }

  // \d+D for dimensionality.
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

  // _t.
  if (typeName.size() > 2 && typeName.endswith("_t")) {
    return typeName.drop_back(2);
  }
  return None;
}

/// Match a word within a name to a word within a type.
static bool matchNameWordToTypeWord(StringRef nameWord, StringRef typeWord) {
  // If the name word is longer, there's no match.
  if (nameWord.size() > typeWord.size()) return false;

  // If the name word is shorter, try for a partial match.
  if (nameWord.size() < typeWord.size()) {
    // We can match the suffix of the type so long as everything preceding the
    // match is neither a lowercase letter nor a '_'. This ignores type
    // prefixes for acronyms, e.g., the 'NS' in 'NSURL'.
    if (typeWord.endswith(nameWord)) {
      // Check that everything preceding the match is neither a lowercase letter
      // nor a '_'.
      for (unsigned i = 0, n = nameWord.size(); i != n; ++i) {
        if (clang::isLowercase(typeWord[i]) || typeWord[i] == '_') return false;
      }

      return true;
    }

    // We can match a prefix so long as everything following the match is
    // a number.
    if (camel_case::startsWithIgnoreFirstCase(typeWord, nameWord)) {
      for (unsigned i = nameWord.size(), n = typeWord.size(); i != n; ++i) {
        if (!clang::isDigit(typeWord[i])) return false;
      }

      return true;
    }

    return false;
  }

  // Check for an exact match.
  return camel_case::sameWordIgnoreFirstCase(nameWord, typeWord);
}

/// Match the beginning of the name to the given type name.
StringRef swift::matchLeadingTypeName(StringRef name,
                                      OmissionTypeName typeName) {
  // Match the camelCase beginning of the name to the
  // ending of the type name.
  auto nameWords = camel_case::getWords(name);
  auto typeWords = camel_case::getWords(typeName.Name);
  auto nameWordIter = nameWords.begin(),
    nameWordIterEnd = nameWords.end();
  auto typeWordRevIter = typeWords.rbegin(),
    typeWordRevIterEnd = typeWords.rend();

  // Find the last instance of the first word in the name within
  // the words in the type name.
  while (typeWordRevIter != typeWordRevIterEnd &&
         !matchNameWordToTypeWord(*nameWordIter, *typeWordRevIter)) {
    ++typeWordRevIter;
  }

  // If we didn't find the first word in the name at all, we're
  // done.
  if (typeWordRevIter == typeWordRevIterEnd)
    return name;

  // Now, match from the first word up until the end of the type name.
  auto typeWordIter = typeWordRevIter.base(),
    typeWordIterEnd = typeWords.end();
  ++nameWordIter;
  while (typeWordIter != typeWordIterEnd &&
         nameWordIter != nameWordIterEnd &&
         matchNameWordToTypeWord(*nameWordIter, *typeWordIter)) {
    ++typeWordIter;
    ++nameWordIter;
  }

  // If we didn't reach the end of the type name, don't match.
  if (typeWordIter != typeWordIterEnd)
    return name;

  // Chop of the beginning of the name.
  return name.substr(nameWordIter.getPosition());
}

StringRef StringScratchSpace::copyString(StringRef string) {
  void *memory = Allocator.Allocate(string.size(), alignof(char));
  memcpy(memory, string.data(), string.size());
  return StringRef(static_cast<char *>(memory), string.size());
}

void InheritedNameSet::add(StringRef name) {
  Names.insert(name);
}

bool InheritedNameSet::contains(StringRef name) const {
  auto set = this;
  do {
    if (set->Names.count(name) > 0) return true;
    set = set->Parent;
  } while (set);

  return false;
}

/// Wrapper for camel_case::toLowercaseWord that uses string scratch space.
StringRef camel_case::toLowercaseWord(StringRef string,
                                      StringScratchSpace &scratch){
  llvm::SmallString<32> scratchStr;
  StringRef result = toLowercaseWord(string, scratchStr);
  if (string == result)
    return string;

  return scratch.copyString(result);
}

/// Omit needless words from the beginning of a name.
static StringRef omitNeedlessWordsFromPrefix(StringRef name,
                                             OmissionTypeName type,
                                             StringScratchSpace &scratch){
  if (type.empty())
    return name;

  // Match the result type to the beginning of the name.
  StringRef newName = matchLeadingTypeName(name, type);
  if (newName == name)
    return name;

  auto firstWord = camel_case::getFirstWord(newName);

  // If we have a preposition, we can chop off type information at the
  // beginning of the name.
  if (getPartOfSpeech(firstWord) == PartOfSpeech::Preposition &&
      newName.size() > firstWord.size()) {
    // If the preposition was "by" and is followed by a gerund, also remove
    // "by".
    if (firstWord == "By") {
      StringRef nextWord = camel_case::getFirstWord(
                             newName.substr(firstWord.size()));
      if (getPartOfSpeech(nextWord) == PartOfSpeech::Gerund) {
        return toLowercaseWord(newName.substr(firstWord.size()), scratch);
      }
    }

    return toLowercaseWord(newName, scratch);
  }

  return name;
}

/// Identify certain vacuous names to which we do not want to reduce any name.
static bool isVacuousName(StringRef name) {
  return camel_case::sameWordIgnoreFirstCase(name, "get") ||
         camel_case::sameWordIgnoreFirstCase(name, "for") ||
         camel_case::sameWordIgnoreFirstCase(name, "set") ||
         camel_case::sameWordIgnoreFirstCase(name, "using") ||
         camel_case::sameWordIgnoreFirstCase(name, "with");
}

static StringRef omitNeedlessWords(StringRef name,
                                   OmissionTypeName typeName,
                                   NameRole role,
                                   const InheritedNameSet *allPropertyNames,
                                   StringScratchSpace &scratch) {
  // If we have no name or no type name, there is nothing to do.
  if (name.empty() || typeName.empty()) return name;

  // Get the camel-case words in the name and type name.
  auto nameWords = camel_case::getWords(name);
  auto typeWords = camel_case::getWords(typeName.Name);

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
    auto nameWord = *nameWordRevIter;
    if (matchNameWordToTypeWord(nameWord, *typeWordRevIter)) {
      anyMatches = true;
      ++nameWordRevIter;
      ++typeWordRevIter;
      continue;
    }

    // Special case: "Indexes" and "Indices" in the name match
    // "IndexSet" in the type.
    if ((matchNameWordToTypeWord(nameWord, "Indexes") ||
         matchNameWordToTypeWord(nameWord, "Indices")) &&
        *typeWordRevIter == "Set") {
      auto nextTypeWordRevIter = typeWordRevIter;
      ++nextTypeWordRevIter;
      if (nextTypeWordRevIter != typeWordRevIterEnd &&
          matchNameWordToTypeWord("Index", *nextTypeWordRevIter)) {
        anyMatches = true;
        ++nameWordRevIter;
        typeWordRevIter = nextTypeWordRevIter;
        ++typeWordRevIter;
        continue;
      }
    }

    // Special case: "Index" in the name matches "Int" or "Integer" in the type.
    if (matchNameWordToTypeWord(nameWord, "Index") &&
        (matchNameWordToTypeWord("Int", *typeWordRevIter) ||
         matchNameWordToTypeWord("Integer", *typeWordRevIter))) {
      anyMatches = true;
      ++nameWordRevIter;
      ++typeWordRevIter;
      continue;
    }

    // Special case: if the word in the name ends in 's', and we have
    // a collection element type, see if this is a plural.
    if (!typeName.CollectionElement.empty() && nameWord.size() > 2 &&
        nameWord.back() == 's') {
      // Check <element name>s.
      auto shortenedNameWord
        = name.substr(0, nameWordRevIter.base().getPosition()-1);
      auto newShortenedNameWord
        = omitNeedlessWords(shortenedNameWord, typeName.CollectionElement,
                            NameRole::Partial, allPropertyNames, scratch);
      if (shortenedNameWord != newShortenedNameWord) {
        anyMatches = true;
        unsigned targetSize = newShortenedNameWord.size();
        while (nameWordRevIter.base().getPosition() > targetSize)
          ++nameWordRevIter;
        continue;
      }
    }

    // If this is a skippable suffix, skip it and keep looking.
    if (nameWordRevIter == nameWordRevIterBegin) {
      if (auto withoutSuffix = skipTypeSuffix(typeName.Name)) {
        typeName.Name = *withoutSuffix;
        typeWords = camel_case::getWords(typeName.Name);
        typeWordRevIter = typeWords.rbegin();
        typeWordRevIterEnd = typeWords.rend();
        continue;
      }
    }

    break;
  }

  StringRef origName = name;

  // If we matched anything above, update the name appropriately.
  if (anyMatches) {
    // Handle complete name matches.
    if (nameWordRevIter == nameWordRevIterEnd) {
      // If we're doing a partial match, return the empty string.
      if (role == NameRole::Partial) return "";

      // Leave the name alone.
      return name;
    }

    switch (role) {
    case NameRole::Property:
      // Always strip off type information.
      name = name.substr(0, nameWordRevIter.base().getPosition());
      break;

    case NameRole::BaseName:
    case NameRole::FirstParameter:
    case NameRole::Partial:
    case NameRole::SubsequentParameter:
      // Classify the part of speech of the word before the type
      // information we would strip off.
      switch (getPartOfSpeech(*nameWordRevIter)) {
      case PartOfSpeech::Preposition:
        if (role == NameRole::BaseName) {
          // Strip off the part of the name that is redundant with
          // type information, so long as there's something preceding the
          // preposition.
          if (std::next(nameWordRevIter) != nameWordRevIterEnd)
            name = name.substr(0, nameWordRevIter.base().getPosition());
          break;
        }

        SWIFT_FALLTHROUGH;

      case PartOfSpeech::Verb:
      case PartOfSpeech::Gerund:
        // Don't prune redundant type information from the base name if
        // there is a corresponding property (either singular or plural).
        if (allPropertyNames && role == NameRole::BaseName) {
          SmallString<16> localScratch;
          auto removedText = name.substr(nameWordRevIter.base().getPosition());
          auto removedName = camel_case::toLowercaseWord(removedText,
                                                         localScratch);

          // A property with exactly this name.
          if (allPropertyNames->contains(removedName)) return name;

          // From here on, we'll be working with scratch space.
          if (removedName.data() != localScratch.data())
            localScratch = removedName;

          if (localScratch.back() == 'y') {
            // If the last letter is a 'y', try 'ies'.
            localScratch.pop_back();
            localScratch += "ies";
            if (allPropertyNames->contains(localScratch)) return name;
          } else {
            // Otherwise, add an 's' and try again.
            localScratch += 's';
            if (allPropertyNames->contains(localScratch)) return name;

            // Alternatively, try to add 'es'.
            localScratch.pop_back();
            localScratch += "es";
            if (allPropertyNames->contains(localScratch)) return name;
          }
        }

        // Strip off the part of the name that is redundant with
        // type information.
        name = name.substr(0, nameWordRevIter.base().getPosition());
        break;

      case PartOfSpeech::Unknown:
        // Assume it's a noun or adjective; don't strip anything.
        break;
      }
      break;
    }

  }

  // If we ended up with a vacuous name like "get" or "set", do nothing.
  if (isVacuousName(name))
    return origName;

  switch (role) {
  case NameRole::BaseName:
  case NameRole::Property:
    // If we ended up with a keyword for a property name or base name,
    // do nothing.
    if (isKeyword(name))
      return origName;
    break;

  case NameRole::SubsequentParameter:
  case NameRole::FirstParameter:
  case NameRole::Partial:
    break;
  }

  // We're done.
  return name;
}

/// A form of toLowercaseWord that also lowercases acronyms.
static StringRef toLowercaseWordAndAcronym(StringRef string,
                                           StringScratchSpace &scratch) {
  if (string.empty())
    return string;

  // Already lowercase.
  if (!clang::isUppercase(string[0]))
    return string;

  // Lowercase until we hit the an uppercase letter followed by a
  // non-uppercase letter.
  llvm::SmallString<32> scratchStr;
  for (unsigned i = 0, n = string.size(); i != n; ++i) {
    // If the next character is not uppercase, stop.
    if (i < n - 1 && !clang::isUppercase(string[i+1])) {
      // If the next non-uppercase character was alphanumeric, we should
      // still lowercase the character we're on.
      if (!clang::isLetter(string[i+1])) {
        scratchStr.push_back(clang::toLowercase(string[i]));
        ++i;
      }

      scratchStr.append(string.substr(i));
      break;
    }

    scratchStr.push_back(clang::toLowercase(string[i]));
  }

  return scratch.copyString(scratchStr);
}

bool swift::omitNeedlessWords(StringRef &baseName,
                              MutableArrayRef<StringRef> argNames,
                              StringRef firstParamName,
                              OmissionTypeName resultType,
                              OmissionTypeName contextType,
                              ArrayRef<OmissionTypeName> paramTypes,
                              bool returnsSelf,
                              bool isProperty,
                              const InheritedNameSet *allPropertyNames,
                              StringScratchSpace &scratch) {
  bool anyChanges = false;

  /// Local function that lowercases all of the base names and
  /// argument names before returning.
  auto lowercaseAcronymsForReturn = [&] {
    StringRef newBaseName = toLowercaseWordAndAcronym(baseName, scratch);
    if (baseName.data() != newBaseName.data()) {
      baseName = newBaseName;
      anyChanges = true;
    }

    for (StringRef &argName : argNames) {
      StringRef newArgName = toLowercaseWordAndAcronym(argName, scratch);
      if (argName.data() != newArgName.data()) {
        argName = newArgName;
        anyChanges = true;
      }
    }

    return anyChanges;
  };

  // If the result type matches the context, remove the context type from the
  // prefix of the name.
  bool resultTypeMatchesContext = returnsSelf || (resultType == contextType);
  if (resultTypeMatchesContext) {
    StringRef newBaseName = omitNeedlessWordsFromPrefix(baseName, contextType,
                                                        scratch);
    if (newBaseName != baseName) {
      baseName = newBaseName;
      anyChanges = true;
    }
  }

  // Treat zero-parameter methods and properties the same way.
  if (paramTypes.empty()) {
    if (resultTypeMatchesContext) {
      StringRef newBaseName = ::omitNeedlessWords(
                                baseName,
                                returnsSelf ? contextType : resultType,
                                NameRole::Property,
                                allPropertyNames,
                                scratch);
      if (newBaseName != baseName) {
        baseName = newBaseName;
        anyChanges = true;
      }
    }

    return lowercaseAcronymsForReturn();
  }

  // Omit needless words based on parameter types.
  for (unsigned i = 0, n = argNames.size(); i != n; ++i) {
    // If there is no corresponding parameter, there is nothing to
    // omit.
    if (i >= paramTypes.size()) continue;

    // Omit needless words based on the type of the parameter.
    NameRole role = i > 0 ? NameRole::SubsequentParameter
      : argNames[0].empty() ? NameRole::BaseName
      : NameRole::FirstParameter;

    // Omit needless words from the name.
    StringRef name = role == NameRole::BaseName ? baseName : argNames[i];
    StringRef newName = ::omitNeedlessWords(name, paramTypes[i], role,
                                            role == NameRole::BaseName 
                                              ? allPropertyNames
                                              : nullptr,
                                            scratch);

    // Did the name change?
    if (name != newName)
      anyChanges = true;

    // If the first parameter has a default argument, and there is a
    // preposition in the base name, split the base name at that preposition.
    if (role == NameRole::BaseName && argNames[0].empty() &&
        paramTypes[0].hasDefaultArgument()) {
      // Scan backwards for a preposition.
      auto nameWords = camel_case::getWords(newName);
      auto nameWordRevIter = nameWords.rbegin(),
        nameWordRevIterEnd = nameWords.rend();
      bool found = false, done = false;
      while (nameWordRevIter != nameWordRevIterEnd && !done) {
        switch (getPartOfSpeech(*nameWordRevIter)) {
        case PartOfSpeech::Preposition:
          found = true;
          done = true;
          break;

        case PartOfSpeech::Verb:
        case PartOfSpeech::Gerund:
          // Don't skip over verbs or gerunds.
          done = true;
          break;

        case PartOfSpeech::Unknown:
          ++nameWordRevIter;
          break;
        }
      }

      // If we found a split point that's not at the beginning of the
      // name, split there.
      if (found) {
        ++nameWordRevIter;
        unsigned splitPos = nameWordRevIter.base().getPosition();
        if (splitPos > 0) {
          unsigned afterSplitPos = splitPos;

          // Create a first argument name with the remainder of the base name,
          // lowercased. If we would end up with a vacuous name, go
          // back and get the original.
          StringRef newArgName = newName.substr(afterSplitPos);
          if (isVacuousName(newArgName)) {
            size_t pos = name.rfind(newArgName);
            newArgName = name.substr(pos);
          }

          // If there is a leading "with" on the first argument, drop it.
          if (newArgName.size() > 4 &&
              camel_case::sameWordIgnoreFirstCase(
                camel_case::getFirstWord(newArgName),
                "with")) {
            newArgName = newArgName.substr(4);
          }

          argNames[0] = toLowercaseWord(newArgName, scratch);

          // Update the base name by splitting at the preposition.
          newName = newName.substr(0, splitPos);

          anyChanges = true;
        }
      }
    }

    if (name == newName) continue;

    // Record this change.
    if (role == NameRole::BaseName) {
      baseName = newName;
    } else {
      argNames[i] = newName;
    }
  }

  return lowercaseAcronymsForReturn();
}
