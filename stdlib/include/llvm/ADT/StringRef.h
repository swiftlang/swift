//===- StringRef.h - Constant String Reference Wrapper ----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_STRINGREF_H
#define LLVM_ADT_STRINGREF_H

#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Compiler.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <limits>
#include <string>
#if __cplusplus > 201402L
#include <string_view>
#endif
#include <type_traits>
#include <utility>

// Declare the __builtin_strlen intrinsic for MSVC so it can be used in
// constexpr context.
#if defined(_MSC_VER)
extern "C" size_t __builtin_strlen(const char *);
#endif

inline namespace __swift { inline namespace __runtime {
namespace llvm {

  template <typename T> class SmallVectorImpl;
  template <typename T> struct DenseMapInfo;
  class StringRef;

  /// Helper functions for StringRef::getAsInteger.
  bool getAsUnsignedInteger(StringRef Str, unsigned Radix,
                            unsigned long long &Result);

  bool getAsSignedInteger(StringRef Str, unsigned Radix, long long &Result);

  bool consumeUnsignedInteger(StringRef &Str, unsigned Radix,
                              unsigned long long &Result);
  bool consumeSignedInteger(StringRef &Str, unsigned Radix, long long &Result);

  /// StringRef - Represent a constant reference to a string, i.e. a character
  /// array and a length, which need not be null terminated.
  ///
  /// This class does not own the string data, it is expected to be used in
  /// situations where the character data resides in some other buffer, whose
  /// lifetime extends past that of the StringRef. For this reason, it is not in
  /// general safe to store a StringRef.
  class LLVM_GSL_POINTER StringRef {
  public:
    static constexpr size_t npos = ~size_t(0);

    using iterator = const char *;
    using const_iterator = const char *;
    using size_type = size_t;

  private:
    /// The start of the string, in an external buffer.
    const char *Data = nullptr;

    /// The length of the string.
    size_t Length = 0;

    // Workaround memcmp issue with null pointers (undefined behavior)
    // by providing a specialized version
    static int compareMemory(const char *Lhs, const char *Rhs, size_t Length) {
      if (Length == 0) { return 0; }
      return ::memcmp(Lhs,Rhs,Length);
    }

    // Constexpr version of std::strlen.
    static constexpr size_t strLen(const char *Str) {
#if __cplusplus > 201402L
      return std::char_traits<char>::length(Str);
#elif __has_builtin(__builtin_strlen) || defined(__GNUC__) || \
    (defined(_MSC_VER) && _MSC_VER >= 1916)
      return __builtin_strlen(Str);
#else
      const char *Begin = Str;
      while (*Str != '\0')
        ++Str;
      return Str - Begin;
#endif
    }

  public:
    /// @name Constructors
    /// @{

    /// Construct an empty string ref.
    /*implicit*/ StringRef() = default;

    /// Disable conversion from nullptr.  This prevents things like
    /// if (S == nullptr)
    StringRef(std::nullptr_t) = delete;

    /// Construct a string ref from a cstring.
    /*implicit*/ constexpr StringRef(const char *Str)
        : Data(Str), Length(Str ? strLen(Str) : 0) {}

    /// Construct a string ref from a pointer and length.
    /*implicit*/ constexpr StringRef(const char *data, size_t length)
        : Data(data), Length(length) {}

    /// Construct a string ref from an std::string.
    /*implicit*/ StringRef(const std::string &Str)
      : Data(Str.data()), Length(Str.length()) {}

#if __cplusplus > 201402L
    /// Construct a string ref from an std::string_view.
    /*implicit*/ constexpr StringRef(std::string_view Str)
        : Data(Str.data()), Length(Str.size()) {}
#endif

    /// @}
    /// @name Iterators
    /// @{

    iterator begin() const { return Data; }

    iterator end() const { return Data + Length; }

    const unsigned char *bytes_begin() const {
      return reinterpret_cast<const unsigned char *>(begin());
    }
    const unsigned char *bytes_end() const {
      return reinterpret_cast<const unsigned char *>(end());
    }
    iterator_range<const unsigned char *> bytes() const {
      return make_range(bytes_begin(), bytes_end());
    }

    /// @}
    /// @name String Operations
    /// @{

    /// data - Get a pointer to the start of the string (which may not be null
    /// terminated).
    [[nodiscard]]
    const char *data() const { return Data; }

    /// empty - Check if the string is empty.
    [[nodiscard]]
    bool empty() const { return Length == 0; }

    /// size - Get the string size.
    [[nodiscard]]
    size_t size() const { return Length; }

    /// front - Get the first character in the string.
    [[nodiscard]]
    char front() const {
      assert(!empty());
      return Data[0];
    }

    /// back - Get the last character in the string.
    [[nodiscard]]
    char back() const {
      assert(!empty());
      return Data[Length-1];
    }

    // copy - Allocate copy in Allocator and return StringRef to it.
    template <typename Allocator>
    [[nodiscard]] StringRef copy(Allocator &A) const {
      // Don't request a length 0 copy from the allocator.
      if (empty())
        return StringRef();
      char *S = A.template Allocate<char>(Length);
      std::copy(begin(), end(), S);
      return StringRef(S, Length);
    }

    /// equals - Check for string equality, this is more efficient than
    /// compare() when the relative ordering of inequal strings isn't needed.
    [[nodiscard]]
    bool equals(StringRef RHS) const {
      return (Length == RHS.Length &&
              compareMemory(Data, RHS.Data, RHS.Length) == 0);
    }

    /// Check for string equality, ignoring case.
    [[nodiscard]]
    bool equals_insensitive(StringRef RHS) const {
      return Length == RHS.Length && compare_insensitive(RHS) == 0;
    }

    /// compare - Compare two strings; the result is -1, 0, or 1 if this string
    /// is lexicographically less than, equal to, or greater than the \p RHS.
    [[nodiscard]]
    int compare(StringRef RHS) const {
      // Check the prefix for a mismatch.
      if (int Res = compareMemory(Data, RHS.Data, std::min(Length, RHS.Length)))
        return Res < 0 ? -1 : 1;

      // Otherwise the prefixes match, so we only need to check the lengths.
      if (Length == RHS.Length)
        return 0;
      return Length < RHS.Length ? -1 : 1;
    }

    /// Compare two strings, ignoring case.
    [[nodiscard]]
    int compare_insensitive(StringRef RHS) const;

    /// compare_numeric - Compare two strings, treating sequences of digits as
    /// numbers.
    [[nodiscard]]
    int compare_numeric(StringRef RHS) const;

    /// str - Get the contents as an std::string.
    [[nodiscard]]
    std::string str() const {
      if (!Data) return std::string();
      return std::string(Data, Length);
    }

    /// @}
    /// @name Operator Overloads
    /// @{

    [[nodiscard]]
    char operator[](size_t Index) const {
      assert(Index < Length && "Invalid index!");
      return Data[Index];
    }

    /// Disallow accidental assignment from a temporary std::string.
    ///
    /// The declaration here is extra complicated so that `stringRef = {}`
    /// and `stringRef = "abc"` continue to select the move assignment operator.
    template <typename T>
    std::enable_if_t<std::is_same<T, std::string>::value, StringRef> &
    operator=(T &&Str) = delete;

    /// @}
    /// @name Type Conversions
    /// @{

    explicit operator std::string() const { return str(); }

#if __cplusplus > 201402L
    operator std::string_view() const {
      return std::string_view(data(), size());
    }
#endif

    /// @}
    /// @name String Predicates
    /// @{

    /// Check if this string starts with the given \p Prefix.
    [[nodiscard]]
    bool starts_with(StringRef Prefix) const {
      return Length >= Prefix.Length &&
             compareMemory(Data, Prefix.Data, Prefix.Length) == 0;
    }

    /// Check if this string starts with the given \p Prefix, ignoring case.
    [[nodiscard]]
    bool starts_with_insensitive(StringRef Prefix) const;

    /// Check if this string ends with the given \p Suffix.
    [[nodiscard]]
    bool ends_with(StringRef Suffix) const {
      return Length >= Suffix.Length &&
        compareMemory(end() - Suffix.Length, Suffix.Data, Suffix.Length) == 0;
    }

    /// Check if this string ends with the given \p Suffix, ignoring case.
    [[nodiscard]]
    bool ends_with_insensitive(StringRef Suffix) const;

    /// @}
    /// @name String Searching
    /// @{

    /// Search for the first character \p C in the string.
    ///
    /// \returns The index of the first occurrence of \p C, or npos if not
    /// found.
    [[nodiscard]]
    size_t find(char C, size_t From = 0) const {
      size_t FindBegin = std::min(From, Length);
      if (FindBegin < Length) { // Avoid calling memchr with nullptr.
        // Just forward to memchr, which is faster than a hand-rolled loop.
        if (const void *P = ::memchr(Data + FindBegin, C, Length - FindBegin))
          return static_cast<const char *>(P) - Data;
      }
      return npos;
    }

    /// Search for the first character \p C in the string, ignoring case.
    ///
    /// \returns The index of the first occurrence of \p C, or npos if not
    /// found.
    [[nodiscard]]
    size_t find_insensitive(char C, size_t From = 0) const;

    /// Search for the first character satisfying the predicate \p F
    ///
    /// \returns The index of the first character satisfying \p F starting from
    /// \p From, or npos if not found.
    [[nodiscard]]
    size_t find_if(function_ref<bool(char)> F, size_t From = 0) const {
      StringRef S = drop_front(From);
      while (!S.empty()) {
        if (F(S.front()))
          return size() - S.size();
        S = S.drop_front();
      }
      return npos;
    }

    /// Search for the first character not satisfying the predicate \p F
    ///
    /// \returns The index of the first character not satisfying \p F starting
    /// from \p From, or npos if not found.
    [[nodiscard]]
    size_t find_if_not(function_ref<bool(char)> F, size_t From = 0) const {
      return find_if([F](char c) { return !F(c); }, From);
    }

    /// Search for the first string \p Str in the string.
    ///
    /// \returns The index of the first occurrence of \p Str, or npos if not
    /// found.
    [[nodiscard]]
    size_t find(StringRef Str, size_t From = 0) const;

    /// Search for the first string \p Str in the string, ignoring case.
    ///
    /// \returns The index of the first occurrence of \p Str, or npos if not
    /// found.
    [[nodiscard]]
    size_t find_insensitive(StringRef Str, size_t From = 0) const;

    /// Search for the last character \p C in the string.
    ///
    /// \returns The index of the last occurrence of \p C, or npos if not
    /// found.
    [[nodiscard]]
    size_t rfind(char C, size_t From = npos) const {
      From = std::min(From, Length);
      size_t i = From;
      while (i != 0) {
        --i;
        if (Data[i] == C)
          return i;
      }
      return npos;
    }

    /// Search for the last character \p C in the string, ignoring case.
    ///
    /// \returns The index of the last occurrence of \p C, or npos if not
    /// found.
    [[nodiscard]]
    size_t rfind_insensitive(char C, size_t From = npos) const;

    /// Search for the last string \p Str in the string.
    ///
    /// \returns The index of the last occurrence of \p Str, or npos if not
    /// found.
    [[nodiscard]]
    size_t rfind(StringRef Str) const;

    /// Search for the last string \p Str in the string, ignoring case.
    ///
    /// \returns The index of the last occurrence of \p Str, or npos if not
    /// found.
    [[nodiscard]]
    size_t rfind_insensitive(StringRef Str) const;

    /// Find the first character in the string that is \p C, or npos if not
    /// found. Same as find.
    [[nodiscard]]
    size_t find_first_of(char C, size_t From = 0) const {
      return find(C, From);
    }

    /// Find the first character in the string that is in \p Chars, or npos if
    /// not found.
    ///
    /// Complexity: O(size() + Chars.size())
    [[nodiscard]]
    size_t find_first_of(StringRef Chars, size_t From = 0) const;

    /// Find the first character in the string that is not \p C or npos if not
    /// found.
    [[nodiscard]]
    size_t find_first_not_of(char C, size_t From = 0) const;

    /// Find the first character in the string that is not in the string
    /// \p Chars, or npos if not found.
    ///
    /// Complexity: O(size() + Chars.size())
    [[nodiscard]]
    size_t find_first_not_of(StringRef Chars, size_t From = 0) const;

    /// Find the last character in the string that is \p C, or npos if not
    /// found.
    [[nodiscard]]
    size_t find_last_of(char C, size_t From = npos) const {
      return rfind(C, From);
    }

    /// Find the last character in the string that is in \p C, or npos if not
    /// found.
    ///
    /// Complexity: O(size() + Chars.size())
    [[nodiscard]]
    size_t find_last_of(StringRef Chars, size_t From = npos) const;

    /// Find the last character in the string that is not \p C, or npos if not
    /// found.
    [[nodiscard]]
    size_t find_last_not_of(char C, size_t From = npos) const;

    /// Find the last character in the string that is not in \p Chars, or
    /// npos if not found.
    ///
    /// Complexity: O(size() + Chars.size())
    [[nodiscard]]
    size_t find_last_not_of(StringRef Chars, size_t From = npos) const;

    /// Return true if the given string is a substring of *this, and false
    /// otherwise.
    [[nodiscard]]
    bool contains(StringRef Other) const { return find(Other) != npos; }

    /// Return true if the given character is contained in *this, and false
    /// otherwise.
    [[nodiscard]]
    bool contains(char C) const { return find_first_of(C) != npos; }

    /// Return true if the given string is a substring of *this, and false
    /// otherwise.
    [[nodiscard]]
    bool contains_insensitive(StringRef Other) const {
      return find_insensitive(Other) != npos;
    }

    /// Return true if the given character is contained in *this, and false
    /// otherwise.
    [[nodiscard]]
    bool contains_insensitive(char C) const {
      return find_insensitive(C) != npos;
    }

    /// @}
    /// @name Helpful Algorithms
    /// @{

    /// Return the number of occurrences of \p C in the string.
    [[nodiscard]]
    size_t count(char C) const {
      size_t Count = 0;
      for (size_t i = 0, e = Length; i != e; ++i)
        if (Data[i] == C)
          ++Count;
      return Count;
    }

    /// Return the number of non-overlapped occurrences of \p Str in
    /// the string.
    size_t count(StringRef Str) const;

    /// Parse the current string as an integer of the specified radix.  If
    /// \p Radix is specified as zero, this does radix autosensing using
    /// extended C rules: 0 is octal, 0x is hex, 0b is binary.
    ///
    /// If the string is invalid or if only a subset of the string is valid,
    /// this returns true to signify the error.  The string is considered
    /// erroneous if empty or if it overflows T.
    template <typename T>
    std::enable_if_t<std::numeric_limits<T>::is_signed, bool>
    getAsInteger(unsigned Radix, T &Result) const {
      long long LLVal;
      if (getAsSignedInteger(*this, Radix, LLVal) ||
            static_cast<T>(LLVal) != LLVal)
        return true;
      Result = LLVal;
      return false;
    }

    template <typename T>
    std::enable_if_t<!std::numeric_limits<T>::is_signed, bool>
    getAsInteger(unsigned Radix, T &Result) const {
      unsigned long long ULLVal;
      // The additional cast to unsigned long long is required to avoid the
      // Visual C++ warning C4805: '!=' : unsafe mix of type 'bool' and type
      // 'unsigned __int64' when instantiating getAsInteger with T = bool.
      if (getAsUnsignedInteger(*this, Radix, ULLVal) ||
          static_cast<unsigned long long>(static_cast<T>(ULLVal)) != ULLVal)
        return true;
      Result = ULLVal;
      return false;
    }

    /// Parse the current string as an integer of the specified radix.  If
    /// \p Radix is specified as zero, this does radix autosensing using
    /// extended C rules: 0 is octal, 0x is hex, 0b is binary.
    ///
    /// If the string does not begin with a number of the specified radix,
    /// this returns true to signify the error. The string is considered
    /// erroneous if empty or if it overflows T.
    /// The portion of the string representing the discovered numeric value
    /// is removed from the beginning of the string.
    template <typename T>
    std::enable_if_t<std::numeric_limits<T>::is_signed, bool>
    consumeInteger(unsigned Radix, T &Result) {
      long long LLVal;
      if (consumeSignedInteger(*this, Radix, LLVal) ||
          static_cast<long long>(static_cast<T>(LLVal)) != LLVal)
        return true;
      Result = LLVal;
      return false;
    }

    template <typename T>
    std::enable_if_t<!std::numeric_limits<T>::is_signed, bool>
    consumeInteger(unsigned Radix, T &Result) {
      unsigned long long ULLVal;
      if (consumeUnsignedInteger(*this, Radix, ULLVal) ||
          static_cast<unsigned long long>(static_cast<T>(ULLVal)) != ULLVal)
        return true;
      Result = ULLVal;
      return false;
    }

    /// @}
    /// @name String Operations
    /// @{

    // Convert the given ASCII string to lowercase.
    [[nodiscard]]
    std::string lower() const;

    /// Convert the given ASCII string to uppercase.
    [[nodiscard]]
    std::string upper() const;

    /// @}
    /// @name Substring Operations
    /// @{

    /// Return a reference to the substring from [Start, Start + N).
    ///
    /// \param Start The index of the starting character in the substring; if
    /// the index is npos or greater than the length of the string then the
    /// empty substring will be returned.
    ///
    /// \param N The number of characters to included in the substring. If N
    /// exceeds the number of characters remaining in the string, the string
    /// suffix (starting with \p Start) will be returned.
    [[nodiscard]]
    StringRef substr(size_t Start, size_t N = npos) const {
      Start = std::min(Start, Length);
      return StringRef(Data + Start, std::min(N, Length - Start));
    }

    /// Return a StringRef equal to 'this' but with only the first \p N
    /// elements remaining.  If \p N is greater than the length of the
    /// string, the entire string is returned.
    [[nodiscard]]
    StringRef take_front(size_t N = 1) const {
      if (N >= size())
        return *this;
      return drop_back(size() - N);
    }

    /// Return a StringRef equal to 'this' but with only the last \p N
    /// elements remaining.  If \p N is greater than the length of the
    /// string, the entire string is returned.
    [[nodiscard]]
    StringRef take_back(size_t N = 1) const {
      if (N >= size())
        return *this;
      return drop_front(size() - N);
    }

    /// Return the longest prefix of 'this' such that every character
    /// in the prefix satisfies the given predicate.
    [[nodiscard]]
    StringRef take_while(function_ref<bool(char)> F) const {
      return substr(0, find_if_not(F));
    }

    /// Return the longest prefix of 'this' such that no character in
    /// the prefix satisfies the given predicate.
    [[nodiscard]]
    StringRef take_until(function_ref<bool(char)> F) const {
      return substr(0, find_if(F));
    }

    /// Return a StringRef equal to 'this' but with the first \p N elements
    /// dropped.
    [[nodiscard]]
    StringRef drop_front(size_t N = 1) const {
      assert(size() >= N && "Dropping more elements than exist");
      return substr(N);
    }

    /// Return a StringRef equal to 'this' but with the last \p N elements
    /// dropped.
    [[nodiscard]]
    StringRef drop_back(size_t N = 1) const {
      assert(size() >= N && "Dropping more elements than exist");
      return substr(0, size()-N);
    }

    /// Return a StringRef equal to 'this', but with all characters satisfying
    /// the given predicate dropped from the beginning of the string.
    [[nodiscard]]
    StringRef drop_while(function_ref<bool(char)> F) const {
      return substr(find_if_not(F));
    }

    /// Return a StringRef equal to 'this', but with all characters not
    /// satisfying the given predicate dropped from the beginning of the string.
    [[nodiscard]]
    StringRef drop_until(function_ref<bool(char)> F) const {
      return substr(find_if(F));
    }

    /// Returns true if this StringRef has the given prefix and removes that
    /// prefix.
    bool consume_front(StringRef Prefix) {
      if (!starts_with(Prefix))
        return false;

      *this = drop_front(Prefix.size());
      return true;
    }

    /// Returns true if this StringRef has the given prefix, ignoring case,
    /// and removes that prefix.
    bool consume_front_insensitive(StringRef Prefix) {
      if (!starts_with_insensitive(Prefix))
        return false;

      *this = drop_front(Prefix.size());
      return true;
    }

    /// Returns true if this StringRef has the given suffix and removes that
    /// suffix.
    bool consume_back(StringRef Suffix) {
      if (!ends_with(Suffix))
        return false;

      *this = drop_back(Suffix.size());
      return true;
    }

    /// Returns true if this StringRef has the given suffix, ignoring case,
    /// and removes that suffix.
    bool consume_back_insensitive(StringRef Suffix) {
      if (!ends_with_insensitive(Suffix))
        return false;

      *this = drop_back(Suffix.size());
      return true;
    }

    /// Return a reference to the substring from [Start, End).
    ///
    /// \param Start The index of the starting character in the substring; if
    /// the index is npos or greater than the length of the string then the
    /// empty substring will be returned.
    ///
    /// \param End The index following the last character to include in the
    /// substring. If this is npos or exceeds the number of characters
    /// remaining in the string, the string suffix (starting with \p Start)
    /// will be returned. If this is less than \p Start, an empty string will
    /// be returned.
    [[nodiscard]]
    StringRef slice(size_t Start, size_t End) const {
      Start = std::min(Start, Length);
      End = std::min(std::max(Start, End), Length);
      return StringRef(Data + Start, End - Start);
    }

    /// Split into two substrings around the first occurrence of a separator
    /// character.
    ///
    /// If \p Separator is in the string, then the result is a pair (LHS, RHS)
    /// such that (*this == LHS + Separator + RHS) is true and RHS is
    /// maximal. If \p Separator is not in the string, then the result is a
    /// pair (LHS, RHS) where (*this == LHS) and (RHS == "").
    ///
    /// \param Separator The character to split on.
    /// \returns The split substrings.
    [[nodiscard]]
    std::pair<StringRef, StringRef> split(char Separator) const {
      return split(StringRef(&Separator, 1));
    }

    /// Split into two substrings around the first occurrence of a separator
    /// string.
    ///
    /// If \p Separator is in the string, then the result is a pair (LHS, RHS)
    /// such that (*this == LHS + Separator + RHS) is true and RHS is
    /// maximal. If \p Separator is not in the string, then the result is a
    /// pair (LHS, RHS) where (*this == LHS) and (RHS == "").
    ///
    /// \param Separator - The string to split on.
    /// \return - The split substrings.
    [[nodiscard]]
    std::pair<StringRef, StringRef> split(StringRef Separator) const {
      size_t Idx = find(Separator);
      if (Idx == npos)
        return std::make_pair(*this, StringRef());
      return std::make_pair(slice(0, Idx), slice(Idx + Separator.size(), npos));
    }

    /// Split into two substrings around the last occurrence of a separator
    /// string.
    ///
    /// If \p Separator is in the string, then the result is a pair (LHS, RHS)
    /// such that (*this == LHS + Separator + RHS) is true and RHS is
    /// minimal. If \p Separator is not in the string, then the result is a
    /// pair (LHS, RHS) where (*this == LHS) and (RHS == "").
    ///
    /// \param Separator - The string to split on.
    /// \return - The split substrings.
    [[nodiscard]]
    std::pair<StringRef, StringRef> rsplit(StringRef Separator) const {
      size_t Idx = rfind(Separator);
      if (Idx == npos)
        return std::make_pair(*this, StringRef());
      return std::make_pair(slice(0, Idx), slice(Idx + Separator.size(), npos));
    }

    /// Split into substrings around the occurrences of a separator string.
    ///
    /// Each substring is stored in \p A. If \p MaxSplit is >= 0, at most
    /// \p MaxSplit splits are done and consequently <= \p MaxSplit + 1
    /// elements are added to A.
    /// If \p KeepEmpty is false, empty strings are not added to \p A. They
    /// still count when considering \p MaxSplit
    /// An useful invariant is that
    /// Separator.join(A) == *this if MaxSplit == -1 and KeepEmpty == true
    ///
    /// \param A - Where to put the substrings.
    /// \param Separator - The string to split on.
    /// \param MaxSplit - The maximum number of times the string is split.
    /// \param KeepEmpty - True if empty substring should be added.
    void split(SmallVectorImpl<StringRef> &A,
               StringRef Separator, int MaxSplit = -1,
               bool KeepEmpty = true) const;

    /// Split into substrings around the occurrences of a separator character.
    ///
    /// Each substring is stored in \p A. If \p MaxSplit is >= 0, at most
    /// \p MaxSplit splits are done and consequently <= \p MaxSplit + 1
    /// elements are added to A.
    /// If \p KeepEmpty is false, empty strings are not added to \p A. They
    /// still count when considering \p MaxSplit
    /// An useful invariant is that
    /// Separator.join(A) == *this if MaxSplit == -1 and KeepEmpty == true
    ///
    /// \param A - Where to put the substrings.
    /// \param Separator - The string to split on.
    /// \param MaxSplit - The maximum number of times the string is split.
    /// \param KeepEmpty - True if empty substring should be added.
    void split(SmallVectorImpl<StringRef> &A, char Separator, int MaxSplit = -1,
               bool KeepEmpty = true) const;

    /// Split into two substrings around the last occurrence of a separator
    /// character.
    ///
    /// If \p Separator is in the string, then the result is a pair (LHS, RHS)
    /// such that (*this == LHS + Separator + RHS) is true and RHS is
    /// minimal. If \p Separator is not in the string, then the result is a
    /// pair (LHS, RHS) where (*this == LHS) and (RHS == "").
    ///
    /// \param Separator - The character to split on.
    /// \return - The split substrings.
    [[nodiscard]]
    std::pair<StringRef, StringRef> rsplit(char Separator) const {
      return rsplit(StringRef(&Separator, 1));
    }

    /// Return string with consecutive \p Char characters starting from the
    /// the left removed.
    [[nodiscard]]
    StringRef ltrim(char Char) const {
      return drop_front(std::min(Length, find_first_not_of(Char)));
    }

    /// Return string with consecutive characters in \p Chars starting from
    /// the left removed.
    [[nodiscard]]
    StringRef ltrim(StringRef Chars = " \t\n\v\f\r") const {
      return drop_front(std::min(Length, find_first_not_of(Chars)));
    }

    /// Return string with consecutive \p Char characters starting from the
    /// right removed.
    [[nodiscard]]
    StringRef rtrim(char Char) const {
      return drop_back(Length - std::min(Length, find_last_not_of(Char) + 1));
    }

    /// Return string with consecutive characters in \p Chars starting from
    /// the right removed.
    [[nodiscard]]
    StringRef rtrim(StringRef Chars = " \t\n\v\f\r") const {
      return drop_back(Length - std::min(Length, find_last_not_of(Chars) + 1));
    }

    /// Return string with consecutive \p Char characters starting from the
    /// left and right removed.
    [[nodiscard]]
    StringRef trim(char Char) const {
      return ltrim(Char).rtrim(Char);
    }

    /// Return string with consecutive characters in \p Chars starting from
    /// the left and right removed.
    [[nodiscard]]
    StringRef trim(StringRef Chars = " \t\n\v\f\r") const {
      return ltrim(Chars).rtrim(Chars);
    }

    /// @}
  };

  /// A wrapper around a string literal that serves as a proxy for constructing
  /// global tables of StringRefs with the length computed at compile time.
  /// In order to avoid the invocation of a global constructor, StringLiteral
  /// should *only* be used in a constexpr context, as such:
  ///
  /// constexpr StringLiteral S("test");
  ///
  class StringLiteral : public StringRef {
  private:
    constexpr StringLiteral(const char *Str, size_t N) : StringRef(Str, N) {
    }

  public:
    template <size_t N>
    constexpr StringLiteral(const char (&Str)[N])
#if defined(__clang__) && __has_attribute(enable_if)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wgcc-compat"
        __attribute((enable_if(__builtin_strlen(Str) == N - 1,
                               "invalid string literal")))
#pragma clang diagnostic pop
#endif
        : StringRef(Str, N - 1) {
    }

    // Explicit construction for strings like "foo\0bar".
    template <size_t N>
    static constexpr StringLiteral withInnerNUL(const char (&Str)[N]) {
      return StringLiteral(Str, N - 1);
    }
  };

  /// @name StringRef Comparison Operators
  /// @{

  inline bool operator==(StringRef LHS, StringRef RHS) {
    return LHS.equals(RHS);
  }

  inline bool operator!=(StringRef LHS, StringRef RHS) { return !(LHS == RHS); }

  inline bool operator<(StringRef LHS, StringRef RHS) {
    return LHS.compare(RHS) == -1;
  }

  inline bool operator<=(StringRef LHS, StringRef RHS) {
    return LHS.compare(RHS) != 1;
  }

  inline bool operator>(StringRef LHS, StringRef RHS) {
    return LHS.compare(RHS) == 1;
  }

  inline bool operator>=(StringRef LHS, StringRef RHS) {
    return LHS.compare(RHS) != -1;
  }

  inline std::string &operator+=(std::string &buffer, StringRef string) {
    return buffer.append(string.data(), string.size());
  }

  /// @}

  /// Compute a hash_code for a StringRef.
  [[nodiscard]]
  hash_code hash_value(StringRef S);

  // Provide DenseMapInfo for StringRefs.
  template <> struct DenseMapInfo<StringRef> {
    static inline StringRef getEmptyKey() {
      return StringRef(
          reinterpret_cast<const char *>(~static_cast<uintptr_t>(0)), 0);
    }

    static inline StringRef getTombstoneKey() {
      return StringRef(
          reinterpret_cast<const char *>(~static_cast<uintptr_t>(1)), 0);
    }

    static unsigned getHashValue(StringRef Val) {
      assert(Val.data() != getEmptyKey().data() &&
             "Cannot hash the empty key!");
      assert(Val.data() != getTombstoneKey().data() &&
             "Cannot hash the tombstone key!");
      return (unsigned)(hash_value(Val));
    }

    static bool isEqual(StringRef LHS, StringRef RHS) {
      if (RHS.data() == getEmptyKey().data())
        return LHS.data() == getEmptyKey().data();
      if (RHS.data() == getTombstoneKey().data())
        return LHS.data() == getTombstoneKey().data();
      return LHS == RHS;
    }
  };

} // end namespace llvm
}} // namespace swift::runtime

#endif // LLVM_ADT_STRINGREF_H
