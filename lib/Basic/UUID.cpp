//===--- UUID.cpp - UUID generation ---------------------------------------===//
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
//
// This is an interface over the standard OSF uuid library that gives UUIDs
// sane value semantics and operators.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/UUID.h"

// WIN32 doesn't natively support <uuid/uuid.h>. Instead, we use Win32 APIs.
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <objbase.h>
#include <string>
#else
#include <uuid/uuid.h>
#endif

using namespace swift;

swift::UUID::UUID(FromRandom_t) {
#if defined(_WIN32)
  ::UUID uuid;
  ::CoCreateGuid(&uuid);

  memcpy(Value, &uuid, Size);
#else
  uuid_generate_random(Value);
#endif
}

swift::UUID::UUID(FromTime_t) {
#if defined(_WIN32)
  ::GUID uuid;
  ::CoCreateGuid(&uuid);

  memcpy(Value, &uuid, Size);
#else
  uuid_generate_time(Value);
#endif
}

swift::UUID::UUID() {
#if defined(_WIN32)
  ::GUID uuid = GUID();

  memcpy(Value, &uuid, Size);
#else
  uuid_clear(Value);
#endif
}

Optional<swift::UUID> swift::UUID::fromString(const char *s) {
#if defined(_WIN32)
  int length = strlen(s) + 1;
  wchar_t *unicodeString = new wchar_t[length];

  size_t convertedChars = 0;
  errno_t conversionResult =
    mbstowcs_s(&convertedChars, unicodeString, length, s, length);
  assert(conversionResult == 0 &&
    "expected successful conversion of char* to wchar_t*");

  ::GUID uuid;
  HRESULT parseResult = CLSIDFromString(unicodeString, &uuid);
  if (parseResult != 0) {
    return None;
  }

  swift::UUID result = UUID();
  memcpy(result.Value, &uuid, Size);
  return result;
#else
  swift::UUID result;
  if (uuid_parse(s, result.Value))
    return None;
  return result;
#endif
}

void swift::UUID::toString(llvm::SmallVectorImpl<char> &out) const {
  out.resize(UUID::StringBufferSize);
#if defined(_WIN32)
  ::GUID uuid;
  memcpy(&uuid, Value, Size);

  LPOLESTR unicodeStr;
  StringFromCLSID(uuid, &unicodeStr);

  char str[StringBufferSize];
  int strLen = wcstombs(str, unicodeStr, sizeof(str));

  assert(strLen == 37 && "expected ascii convertible output from StringFromCLSID.");
  (void)strLen;

  memcpy(out.data(), str, StringBufferSize);
#else
  uuid_unparse_upper(Value, out.data());
#endif
  // Pop off the null terminator.
  assert(out.back() == '\0' && "did not null-terminate?!");
  out.pop_back();
}

int swift::UUID::compare(UUID y) const {
#if defined(_WIN32)
  ::GUID uuid1;
  memcpy(&uuid1, Value, Size);

  ::GUID uuid2;
  memcpy(&uuid2, y.Value, Size);

  return memcmp(Value, y.Value, Size);
#else
  return uuid_compare(Value, y.Value);
#endif
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os, UUID uuid) {
  llvm::SmallString<UUID::StringBufferSize> buf;
  uuid.toString(buf);
  os << buf;
  return os;
}
