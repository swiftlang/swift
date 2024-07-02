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

#ifndef SWIFT_DEMANGLING_MANGLINGUTILS_H
#define SWIFT_DEMANGLING_MANGLINGUTILS_H

#include "swift/Demangling/NamespaceMacros.h"
#include "swift/Demangling/Punycode.h"
#include "llvm/ADT/StringRef.h"
#include <optional>

namespace swift {
namespace Mangle {
SWIFT_BEGIN_INLINE_NAMESPACE

using llvm::StringRef;

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

/// Returns true if \p ch is a valid character which may appear at the start
/// of a symbol mangling.
inline bool isValidSymbolStart(char ch) {
  return isLetter(ch) || ch == '_' || ch == '$';
}

/// Returns true if \p ch is a valid character which may appear in a symbol
/// mangling anywhere other than the first character.
inline bool isValidSymbolChar(char ch) {
  return isValidSymbolStart(ch) || isDigit(ch);
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

/// Returns the standard type kind for an 'S' substitution, e.g. 'i' for "Int".
///
/// \param allowConcurrencyManglings When true, allows the standard
/// substitutions for types in the _Concurrency module that were introduced in
/// Swift 5.5.
std::optional<StringRef> getStandardTypeSubst(StringRef TypeName,
                                              bool allowConcurrencyManglings);

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
        M.addSubstWordsInIdent({wordStartPos, WordIdx});
      } else if (wordLen >= 2 && M.Words.size() < M.MaxNumWords) {
        // It's a new word: remember it.
        // Note: at this time the word's start position is relative to the
        // begin of the identifier. We must update it afterwards so that it is
        // relative to the begin of the whole mangled Buffer.
        M.addWord({wordStartPos, wordLen});
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
  M.addSubstWordsInIdent({ident.size(), -1});

  // Mangle a sequence of word substitutions and sub-strings.
  for (size_t Idx = 0, End = M.SubstWordsInIdent.size(); Idx < End; ++Idx) {
    const WordReplacement &Repl = M.SubstWordsInIdent[Idx];
    if (Pos < Repl.StringPos) {
      // Mangle the sub-string up to the next word substitution (or to the end
      // of the identifier - that's why we added the dummy-word).
      // The first thing: we add the encoded sub-string length.
      bool first = true;
      M.Buffer << (Repl.StringPos - Pos);
      do {
        // Update the start position of new added words, so that they refer to
        // the begin of the whole mangled Buffer.
        if (WordsInBuffer < M.Words.size() &&
            M.Words[WordsInBuffer].start == Pos) {
          M.Words[WordsInBuffer].start = M.getBufferStr().size();
          WordsInBuffer++;
        }
        // Error recovery. We sometimes need to mangle identifiers coming
        // from invalid code.
        if (first && isDigit(ident[Pos]))
          M.Buffer << 'X';
        // Add a literal character of the sub-string.
        else
          M.Buffer << ident[Pos];

        Pos++;
        first = false;
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

/// Utility class for mangling merged substitutions.
///
/// Used in the Mangler and Remangler.
class SubstitutionMerging {

  /// The position of the last substitution mangling,
  /// e.g. 3 for 'AabC' and 'Aab4C'
  size_t lastSubstPosition = 0;

  /// The size of the last substitution mangling,
  /// e.g. 1 for 'AabC' or 2 for 'Aab4C'
  size_t lastSubstSize = 0;

  /// The repeat count of the last substitution,
  /// e.g. 1 for 'AabC' or 4 for 'Aab4C'
  size_t lastNumSubsts = 0;

  /// True if the last substitution is an 'S' substitution,
  /// false if the last substitution is an 'A' substitution.
  bool lastSubstIsStandardSubst = false;

public:

  // The only reason to limit the number of repeated substitutions is that we
  // don't want that the demangler blows up on a bogus substitution, e.g.
  // ...A832456823746582B...
  enum { MaxRepeatCount = 2048 };

  void clear() {
    lastNumSubsts = 0;
  }

  /// Tries to merge the substitution \p Subst with a previously mangled
  /// substitution.
  ///
  /// Returns true on success. In case of false, the caller must mangle the
  /// substitution separately in the form 'S<Subst>' or 'A<Subst>'.
  ///
  /// The Mangler class must provide the following:
  /// *) Buffer: A stream where the mangled identifier is written to.
  /// *) getBufferStr(): Returns a StringRef of the current content of Buffer.
  /// *) resetBuffer(size_t): Resets the buffer to an old position.
  template <typename Mangler>
  bool tryMergeSubst(Mangler &M, StringRef Subst, bool isStandardSubst) {
    assert(isUpperLetter(Subst.back()) ||
           (isStandardSubst && isLowerLetter(Subst.back())));
    StringRef BufferStr = M.getBufferStr();
    if (lastNumSubsts > 0 && lastNumSubsts < MaxRepeatCount
        && BufferStr.size() == lastSubstPosition + lastSubstSize
        && lastSubstIsStandardSubst == isStandardSubst) {

      // The last mangled thing is a substitution.
      assert(lastSubstPosition > 0 && lastSubstPosition < BufferStr.size());
      assert(lastSubstSize > 0);
      StringRef lastSubst = BufferStr.take_back(lastSubstSize)
            .drop_while([](char c) {
        return isDigit(c);
      });
      assert(isUpperLetter(lastSubst.back())
             || (isStandardSubst && isLowerLetter(lastSubst.back())));
      if (lastSubst != Subst && !isStandardSubst) {
        // We can merge with a different 'A' substitution,
        // e.g. 'AB' -> 'AbC'.
        lastSubstPosition = BufferStr.size();
        lastNumSubsts = 1;
        M.resetBuffer(BufferStr.size() - 1);
        assert(isUpperLetter(lastSubst.back()));
        M.Buffer << (char)(lastSubst.back() - 'A' + 'a') << Subst;
        lastSubstSize = 1;
        return true;
      }
      if (lastSubst == Subst) {
        // We can merge with the same 'A' or 'S' substitution,
        // e.g. 'AB' -> 'A2B', or 'S3i' -> 'S4i'
        lastNumSubsts++;
        M.resetBuffer(lastSubstPosition);
        M.Buffer << lastNumSubsts;
        M.Buffer << Subst;
        lastSubstSize = M.getBufferStr().size() - lastSubstPosition;
        return true;
      }
    }
    // We can't merge with the previous substitution, but let's remember this
    // substitution which will be mangled by the caller.
    lastSubstPosition = BufferStr.size() + 1;
    lastSubstSize = Subst.size();
    lastNumSubsts = 1;
    lastSubstIsStandardSubst = isStandardSubst;
    return false;
  }
};

SWIFT_END_INLINE_NAMESPACE
} // end namespace Mangle
} // end namespace swift

#endif // SWIFT_DEMANGLING_MANGLINGUTILS_H

