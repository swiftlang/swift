//===--- UUID.h - UUID generation -----------------------------------------===//
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
// This is an interface over the standard OSF uuid library that gives UUIDs
// sane value semantics and operators.
//
//===----------------------------------------------------------------------===//

#include <uuid/uuid.h>
#include "swift/Basic/UUID.h"

using namespace swift;

UUID::UUID(FromRandom_t) {
  uuid_generate_random(Value);
}

UUID::UUID(FromTime_t) {
  uuid_generate_time(Value);
}

UUID::UUID() {
  uuid_clear(Value);
}

Optional<UUID> UUID::fromString(const char *s) {
  UUID result;
  if (uuid_parse(s, result.Value))
    return None;
  return result;
}

void UUID::toString(llvm::SmallVectorImpl<char> &out) const {
  out.resize(UUID::StringBufferSize);
  uuid_unparse(Value, out.data());
  // Pop off the null terminator.
  assert(out.back() == '\0' && "did not null-terminate?!");
  out.pop_back();
}

int UUID::compare(UUID y) const {
  return uuid_compare(Value, y.Value);
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os, UUID uuid) {
  llvm::SmallString<UUID::StringBufferSize> buf;
  uuid.toString(buf);
  os << buf;
  return os;
}
