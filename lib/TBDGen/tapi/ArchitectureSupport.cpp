//===- tapi/Core/ArchitectureSupport.cpp - Architecture Support -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements architecture specific helper functions.
///
//===----------------------------------------------------------------------===//

#include "ArchitectureSupport.h"
#include "LLVM.h"
#include "clang/Basic/Diagnostic.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

TAPI_NAMESPACE_INTERNAL_BEGIN

bool PackedVersion::parse32(StringRef str) {
  _version = 0;

  if (str.empty())
    return false;

  SmallVector<StringRef, 3> parts;
  SplitString(str, parts, ".");

  if (parts.size() > 3)
    return false;

  unsigned long long num;
  if (getAsUnsignedInteger(parts[0], 10, num))
    return false;

  if (num > UINT16_MAX)
    return false;

  _version = num << 16;

  for (unsigned i = 1, shiftNum = 8; i < parts.size(); ++i, shiftNum -= 8) {
    if (getAsUnsignedInteger(parts[i], 10, num))
      return false;

    if (num > UINT8_MAX)
      return false;

    _version |= (num << shiftNum);
  }

  return true;
}

std::pair<bool, bool> PackedVersion::parse64(StringRef str) {
  bool truncated = false;
  _version = 0;

  if (str.empty())
    return std::make_pair(false, truncated);

  SmallVector<StringRef, 5> parts;
  SplitString(str, parts, ".");

  if (parts.size() > 5)
    return std::make_pair(false, truncated);

  unsigned long long num;
  if (getAsUnsignedInteger(parts[0], 10, num))
    return std::make_pair(false, truncated);

  if (num > 0xFFFFFFULL)
    return std::make_pair(false, truncated);

  if (num > 0xFFFFULL) {
    num = 0xFFFFULL;
    truncated = true;
  }
  _version = num << 16;

  for (unsigned i = 1, shiftNum = 8; i < parts.size() && i < 3;
       ++i, shiftNum -= 8) {
    if (getAsUnsignedInteger(parts[i], 10, num))
      return std::make_pair(false, truncated);

    if (num > 0x3FFULL)
      return std::make_pair(false, truncated);

    if (num > 0xFFULL) {
      num = 0xFFULL;
      truncated = true;
    }
    _version |= (num << shiftNum);
  }

  if (parts.size() > 3)
    truncated = true;

  return std::make_pair(true, truncated);
}

void PackedVersion::print(raw_ostream &os) const {
  os << format("%d", getMajor());
  if (getMinor() || getSubminor())
    os << format(".%d", getMinor());
  if (getSubminor())
    os << format(".%d", getSubminor());
}

const DiagnosticBuilder &operator<<(const DiagnosticBuilder &db,
                                    const PackedVersion &version) {
  SmallString<32> string;
  raw_svector_ostream os(string);
  os << version;
  db.AddString(string);
  return db;
}

TAPI_NAMESPACE_INTERNAL_END
