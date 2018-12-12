//===- tapi/Core/YAML.h - YAML ----------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines common YAML mappings
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_YAML_H
#define TAPI_CORE_YAML_H

#include "Architecture.h"
#include "ArchitectureSet.h"
#include "ArchitectureSupport.h"
#include "AvailabilityInfo.h"
#include "Platform.h"
#include "YAMLReaderWriter.h"
#include "clang/Frontend/FrontendOptions.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/YAMLTraits.h"

using UUID = std::pair<TAPI_INTERNAL::Architecture, std::string>;

LLVM_YAML_STRONG_TYPEDEF(llvm::StringRef, FlowStringRef)
LLVM_YAML_STRONG_TYPEDEF(uint8_t, SwiftVersion)
LLVM_YAML_IS_FLOW_SEQUENCE_VECTOR(UUID)
LLVM_YAML_IS_FLOW_SEQUENCE_VECTOR(FlowStringRef)

namespace llvm {
namespace yaml {

template <> struct ScalarTraits<FlowStringRef> {
  static void output(const FlowStringRef &value, void *ctx, raw_ostream &os);
  static StringRef input(StringRef value, void *ctx, FlowStringRef &out);
  static QuotingType mustQuote(StringRef name);
};

using tapi::ObjCConstraint;
template <> struct ScalarEnumerationTraits<ObjCConstraint> {
  static void enumeration(IO &io, ObjCConstraint &constraint);
};

using TAPI_INTERNAL::Platform;
template <> struct ScalarEnumerationTraits<Platform> {
  static void enumeration(IO &io, Platform &platform);
};

using TAPI_INTERNAL::Architecture;
using TAPI_INTERNAL::ArchitectureSet;
template <> struct ScalarBitSetTraits<ArchitectureSet> {
  static void bitset(IO &io, ArchitectureSet &archs);
};

using TAPI_INTERNAL::getArchType;
template <> struct ScalarTraits<Architecture> {
  static void output(const Architecture &value, void *, raw_ostream &os);
  static StringRef input(StringRef scalar, void *, Architecture &value);
  static QuotingType mustQuote(StringRef);
};

using TAPI_INTERNAL::PackedVersion;
template <> struct ScalarTraits<PackedVersion> {
  static void output(const PackedVersion &value, void *, raw_ostream &os);
  static StringRef input(StringRef scalar, void *, PackedVersion &value);
  static QuotingType mustQuote(StringRef);
};

template <> struct ScalarTraits<SwiftVersion> {
  static void output(const SwiftVersion &value, void *, raw_ostream &os);
  static StringRef input(StringRef scalar, void *, SwiftVersion &value);
  static QuotingType mustQuote(StringRef);
};

using TAPI_INTERNAL::AvailabilityInfo;
template <> struct ScalarTraits<AvailabilityInfo> {
  static void output(const AvailabilityInfo &value, void *, raw_ostream &os);
  static StringRef input(StringRef scalar, void *, AvailabilityInfo &value);
  static QuotingType mustQuote(StringRef);
};

template <> struct ScalarTraits<UUID> {
  static void output(const UUID &value, void *, raw_ostream &os);
  static StringRef input(StringRef scalar, void *, UUID &value);
  static QuotingType mustQuote(StringRef);
};

using clang::InputKind;
template <> struct ScalarEnumerationTraits<InputKind::Language> {
  static void enumeration(IO &io, InputKind::Language &kind);
};

} // end namespace yaml.
} // end namespace llvm.

#endif // TAPI_CORE_YAML_H
