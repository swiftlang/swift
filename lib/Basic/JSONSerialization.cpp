//===--- JSONSerialization.cpp - JSON serialization support ---------------===//
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

#include "swift/Basic/JSONSerialization.h"
#include "llvm/Support/Format.h"

using namespace swift::json;
using namespace swift;

unsigned Output::beginArray() {
  StateStack.push_back(ArrayFirstValue);
  Stream << '[';
  if (PrettyPrint) {
    Stream << '\n';
  }
  return 0;
}

bool Output::preflightElement(unsigned, void *&) {
  if (StateStack.back() != ArrayFirstValue) {
    assert(StateStack.back() == ArrayOtherValue && "We must be in a sequence!");
    Stream << ',';
    if (PrettyPrint)
      Stream << '\n';
  }
  if (PrettyPrint)
    indent();
  return true;
}

void Output::postflightElement(void*) {
  if (StateStack.back() == ArrayFirstValue) {
    StateStack.pop_back();
    StateStack.push_back(ArrayOtherValue);
  }
}

void Output::endArray() {
  StateStack.pop_back();
  if (PrettyPrint) {
    Stream << '\n';
    indent();
  }
  Stream << ']';
}

bool Output::canElideEmptyArray() {
  if (StateStack.size() < 2)
    return true;
  if (StateStack.back() != ObjectFirstKey)
    return true;
  State checkedState = StateStack[StateStack.size() - 2];
  return (checkedState != ArrayFirstValue && checkedState != ArrayOtherValue);
}

void Output::beginObject() {
  StateStack.push_back(ObjectFirstKey);
  Stream << "{";
  if (PrettyPrint)
    Stream << '\n';
}

void Output::endObject() {
  StateStack.pop_back();
  if (PrettyPrint) {
    Stream << '\n';
    indent();
  }
  Stream << "}";
}

bool Output::preflightKey(const char *Key, bool Required, bool SameAsDefault,
                          bool &UseDefault, void *&) {
  UseDefault = false;
  if (Required || !SameAsDefault) {
    if (StateStack.back() != ObjectFirstKey) {
      assert(StateStack.back() == ObjectOtherKey && "We must be in an object!");
      Stream << ',';
      if (PrettyPrint)
        Stream << '\n';
    }
    if (PrettyPrint)
      indent();
    Stream << '"' << Key << "\":";
    if (PrettyPrint)
      Stream << ' ';
    return true;
  }
  return false;
}

void Output::postflightKey(void*) {
  if (StateStack.back() == ObjectFirstKey) {
    StateStack.pop_back();
    StateStack.push_back(ObjectOtherKey);
  }
}

void Output::beginEnumScalar() {
  EnumerationMatchFound = false;
}

bool Output::matchEnumScalar(const char *Str, bool Match) {
  if (Match && !EnumerationMatchFound) {
    StringRef StrRef(Str);
    scalarString(StrRef, true);
    EnumerationMatchFound = true;
  }
  return false;
}

void Output::endEnumScalar() {
  if (!EnumerationMatchFound)
    llvm_unreachable("bad runtime enum value");
}

bool Output::beginBitSetScalar(bool &DoClear) {
  Stream << '[';
  if (PrettyPrint)
    Stream << ' ';
  NeedBitValueComma = false;
  DoClear = false;
  return true;
}

bool Output::bitSetMatch(const char *Str, bool Matches) {
  if (Matches) {
    if (NeedBitValueComma) {
      Stream << ',';
      if (PrettyPrint)
        Stream << ' ';
    }
    StringRef StrRef(Str);
    scalarString(StrRef, true);
  }
  return false;
}

void Output::endBitSetScalar() {
  if (PrettyPrint)
    Stream << ' ';
  Stream << ']';
}

void Output::scalarString(StringRef &S, bool MustQuote) {
  if (MustQuote) {
    Stream << '"';
    Stream.write_escaped(S);
    Stream << '"';
  }
  else
    Stream << S;
}

void Output::indent() {
  Stream.indent(StateStack.size() * 2);
}

//===----------------------------------------------------------------------===//
//  traits for built-in types
//===----------------------------------------------------------------------===//

void ScalarTraits<bool>::output(const bool &Val, raw_ostream &Out) {
  Out << (Val ? "true" : "false");
}

void ScalarTraits<StringRef>::output(const StringRef &Val,
                                     raw_ostream &Out) {
  Out << Val;
}

void ScalarTraits<std::string>::output(const std::string &Val,
                                       raw_ostream &Out) {
  Out << Val;
}

void ScalarTraits<uint8_t>::output(const uint8_t &Val,
                                   raw_ostream &Out) {
  // use temp uin32_t because ostream thinks uint8_t is a character
  uint32_t Num = Val;
  Out << Num;
}

void ScalarTraits<uint16_t>::output(const uint16_t &Val,
                                    raw_ostream &Out) {
  Out << Val;
}

void ScalarTraits<uint32_t>::output(const uint32_t &Val,
                                    raw_ostream &Out) {
  Out << Val;
}

void ScalarTraits<uint64_t>::output(const uint64_t &Val,
                                    raw_ostream &Out) {
  Out << Val;
}

void ScalarTraits<int8_t>::output(const int8_t &Val, raw_ostream &Out) {
  // use temp in32_t because ostream thinks int8_t is a character
  int32_t Num = Val;
  Out << Num;
}

void ScalarTraits<int16_t>::output(const int16_t &Val,
                                   raw_ostream &Out) {
  Out << Val;
}

void ScalarTraits<int32_t>::output(const int32_t &Val,
                                   raw_ostream &Out) {
  Out << Val;
}

void ScalarTraits<int64_t>::output(const int64_t &Val,
                                   raw_ostream &Out) {
  Out << Val;
}

void ScalarTraits<double>::output(const double &Val, raw_ostream &Out) {
  Out << llvm::format("%g", Val);
}

void ScalarTraits<float>::output(const float &Val, raw_ostream &Out) {
  Out << llvm::format("%g", Val);
}
