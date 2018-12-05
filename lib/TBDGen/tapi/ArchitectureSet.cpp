//===- tapi/Core/ArchitectureSet.cpp - Architecture Set -*- C++ -*---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the architecture set.
///
//===----------------------------------------------------------------------===//

#include "ArchitectureSet.h"
#include "LLVM.h"
#include "Defines.h"
#include "clang/Basic/Diagnostic.h"
#include "llvm/Support/raw_ostream.h"
#include <string>

using namespace llvm;

TAPI_NAMESPACE_INTERNAL_BEGIN

bool ArchitectureSet::hasABICompatibleSlice(Architecture arch) const {
  uint32_t cpuType;
  std::tie(cpuType, std::ignore) = getCPUType(arch);

  for (auto arch2 : *this) {
    uint32_t cpuType2;
    std::tie(cpuType2, std::ignore) = getCPUType(arch2);

    if (cpuType == cpuType2)
      return true;
  }

  return false;
}

Architecture ArchitectureSet::getABICompatibleSlice(Architecture arch) const {
  uint32_t cpuType;
  std::tie(cpuType, std::ignore) = getCPUType(arch);

  for (auto arch2 : *this) {
    uint32_t cpuType2;
    std::tie(cpuType2, std::ignore) = getCPUType(arch2);

    if (cpuType == cpuType2)
      return arch2;
  }

  return Architecture::unknown;
}

ArchitectureSet::operator std::string() const {
  if (empty())
    return "[(empty)]";

  std::string result;
  auto size = count();
  for (auto arch : *this) {
    result.append(getArchName(arch));
    size -= 1;
    if (size)
      result.append(" ");
  }
  return result;
}

ArchitectureSet::operator std::vector<Architecture>() const {
  std::vector<Architecture> archs;
  for (auto arch : *this) {
    if (arch == Architecture::unknown)
      continue;
    archs.emplace_back(arch);
  }
  return archs;
}

void ArchitectureSet::print(raw_ostream &os) const { os << std::string(*this); }

raw_ostream &operator<<(raw_ostream &os, ArchitectureSet set) {
  set.print(os);
  return os;
}

const DiagnosticBuilder &operator<<(const DiagnosticBuilder &db,
                                    ArchitectureSet architectureSet) {
  db.AddString(std::string(architectureSet));
  return db;
}

ArchitectureSet mapToArchitectureSet(const std::vector<Triple> &targets) {
  ArchitectureSet result;
  for (const auto &target : targets)
    result |= getArchType(target.getArchName());
  return result;
}

TAPI_NAMESPACE_INTERNAL_END
