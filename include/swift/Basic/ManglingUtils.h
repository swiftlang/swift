//===--- ManglingUtils.h - Utilities for Swift name mangling ----*- C++ -*-===//
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

#ifndef SWIFT_BASIC_MANGLINGUTILS_H
#define SWIFT_BASIC_MANGLINGUTILS_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/ArrayRef.h"
#include "swift/Basic/Punycode.h"

namespace swift {
namespace NewMangling {

inline bool isLowerLetter(char ch) {
  return ch >= 'a' && ch <= 'z';
}

inline bool isUpperLetter(char ch) {
  return ch >= 'A' && ch <= 'Z';
}

inline bool isDigit(char ch) {
  return ch >= '0' && ch <= '9';
}

inline bool isLetter(char ch) {
  return isLowerLetter(ch) || isUpperLetter(ch);
}

/// Returns true if \p ch is a character which defines the begin of a
/// substitution word.
inline bool isWordStart(char ch) {
  return !isDigit(ch) && ch != '_' && ch != 0;
}

/// Returns true if \p ch is a character (following \p prevCh) which defines
/// the end of a substitution word.
inline bool isWordEnd(char ch, char prevCh) {
  if (ch == '_' || ch == 0)
    return true;

  if (!isUpperLetter(prevCh) && isUpperLetter(ch))
    return true;

  return false;
}

/// Returns true if \p ch is a valid character which may appear in a symbol
/// mangling.
inline bool isValidSymbolChar(char ch) {
  return isLetter(ch) || isDigit(ch) || ch == '_' || ch == '$';
}

/// Returns true if \p str contains any character which may not appear in a
/// mangled symbol string and therefore must be punycode encoded.
bool needsPunycodeEncoding(StringRef str);

/// Returns true if \p str contains any non-ASCII character.
bool isNonAscii(StringRef str);

/// Describes a Word in a mangled identifier.
struct SubstitutionWord {

  /// The position of the first word character in the mangled string.
  size_t start;

  /// The length of the word.
  size_t length;
};

/// Helper struct which represents a word substitution.
struct WordReplacement {
  /// The position in the identifier where the word is substituted.
  size_t StringPos;

  /// The index into the mangler's Words array (-1 if invalid).
  int WordIdx;
};

/// Translate the given operator character into its mangled form.
///
/// Current operator characters:   @/=-+*%<>!&|^~ and the special operator '..'
char translateOperatorChar(char op);

/// Returns a string where all characters of the operator \p Op are translated
/// to their mangled form.
std::string translateOperator(StringRef Op);

/// Mangles an identifier using a generic Mangler class.
///
/// The Mangler class must provide the following:
/// *) Words: An array of SubstitutionWord which holds the current list of
///           found words which can be used for substitutions.
/// *) SubstWordsInIdent: An array of WordReplacement, which is just used
///           as a temporary storage during mangling. Must be empty.
/// *) Buffer: A stream where the mangled identifier is written to.
/// *) getBufferStr(): Returns a StringRef of the current content of Buffer.
/// *) UsePunycode: A flag indicating if punycode encoding should be done.
template <typename Mangler>
void mangleIdentifier(Mangler &M, StringRef ident) {

  size_t WordsInBuffer = M.Words.size();
  assert(M.SubstWordsInIdent.empty());
  if (M.UsePunycode && needsPunycodeEncoding(ident)) {
    // If the identifier contains non-ASCII character, we mangle
    // with an initial '00' and Punycode the identifier string.
    std::string punycodeBuf;
    Punycode::encodePunycodeUTF8(ident, punycodeBuf,
                                 /*mapNonSymbolChars*/ true);
    StringRef pcIdent = punycodeBuf;
    M.Buffer << "00" << pcIdent.size();
    if (isDigit(pcIdent[0]) || pcIdent[0] == '_')
      M.Buffer << '_';
    M.Buffer << pcIdent;
    return;
  }
  // Search for word substitutions and for new words.
  const size_t NotInsideWord = ~0;
  size_t wordStartPos = NotInsideWord;
  for (size_t Pos = 0, Len = ident.size(); Pos <= Len; ++Pos) {
    char ch = (Pos < Len ? ident[Pos] : 0);
    if (wordStartPos != NotInsideWord && isWordEnd(ch, ident[Pos - 1])) {
      // This position is the end of a word, i.e. the next character after a
      // word.
      assert(Pos > wordStartPos);
      size_t wordLen = Pos - wordStartPos;
      StringRef Word = ident.substr(wordStartPos, wordLen);

      // Helper function to lookup the Word in a string.
      auto lookupWord = [&] (StringRef Str,
                             size_t FromWordIdx, size_t ToWordIdx) -> int {
        for (size_t Idx = FromWordIdx; Idx < ToWordIdx; ++Idx) {
          const SubstitutionWord &w = M.Words[Idx];
          StringRef existingWord =  Str.substr(w.start, w.length);
          if (Word == existingWord)
            return (int)Idx;
        }
        return -1;
      };

      // Is the word already present in the so far mangled string?
      int WordIdx = lookupWord(M.getBufferStr(), 0, WordsInBuffer);
      // Otherwise, is the word already present in this identifier?
      if (WordIdx < 0)
        WordIdx = lookupWord(ident, WordsInBuffer, M.Words.size());

      if (WordIdx >= 0) {
        // We found a word substitution!
        assert(WordIdx < 26);
        M.SubstWordsInIdent.push_back({wordStartPos, WordIdx});
      } else if (wordLen >= 2 && M.Words.size() < 26) {
        // It's a new word: remember it.
        // Note: at this time the word's start position is relative to the
        // begin of the identifier. We must update it afterwards so that it is
        // relative to the begin of the whole mangled Buffer.
        M.Words.push_back({wordStartPos, wordLen});
      }
      wordStartPos = NotInsideWord;
    }
    if (wordStartPos == NotInsideWord && isWordStart(ch)) {
      // This position is the begin of a word.
      wordStartPos = Pos;
    }
  }
  // If we have word substitutions mangle an initial '0'.
  if (!M.SubstWordsInIdent.empty())
    M.Buffer << '0';

  size_t Pos = 0;
  // Add a dummy-word at the end of the list.
  M.SubstWordsInIdent.push_back({ident.size(), -1});

  // Mangle a sequence of word substitutions and sub-strings.
  for (size_t Idx = 0, End = M.SubstWordsInIdent.size(); Idx < End; ++Idx) {
    const WordReplacement &Repl = M.SubstWordsInIdent[Idx];
    if (Pos < Repl.StringPos) {
      // Mangle the sub-string up to the next word substitution (or to the end
      // of the identifier - that's why we added the dummy-word).
      // The first thing: we add the encoded sub-string length.
      M.Buffer << (Repl.StringPos - Pos);
      assert(!isDigit(ident[Pos]) &&
             "first char of sub-string may not be a digit");
      do {
        // Update the start position of new added words, so that they refer to
        // the begin of the whole mangled Buffer.
        if (WordsInBuffer < M.Words.size() &&
            M.Words[WordsInBuffer].start == Pos) {
          M.Words[WordsInBuffer].start = M.getBufferStr().size();
          WordsInBuffer++;
        }
        // Add a literal character of the sub-string.
        M.Buffer << ident[Pos];
        Pos++;
      } while (Pos < Repl.StringPos);
    }
    // Is it a "real" word substitution (and not the dummy-word)?
    if (Repl.WordIdx >= 0) {
      assert(Repl.WordIdx <= (int)WordsInBuffer);
      Pos += M.Words[Repl.WordIdx].length;
      if (Idx < End - 2) {
        M.Buffer << (char)(Repl.WordIdx + 'a');
      } else {
        // The last word substitution is a capital letter.
        M.Buffer << (char)(Repl.WordIdx + 'A');
        if (Pos == ident.size())
          M.Buffer << '0';
      }
    }
  }
  M.SubstWordsInIdent.clear();
}

} // end namespace Mangle
} // end namespace swift

#endif // SWIFT_BASIC_MANGLINGUTILS_H

