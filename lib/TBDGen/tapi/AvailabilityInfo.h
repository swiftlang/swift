//===- tapi/Core/AvailabilityInfo.h - TAPI Availability Info ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines the Availability Info.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_AVAILABILITY_INFO_H
#define TAPI_CORE_AVAILABILITY_INFO_H

#include "ArchitectureSupport.h"
#include "LLVM.h"
#include "Defines.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Error.h"

TAPI_NAMESPACE_INTERNAL_BEGIN

struct AvailabilityInfo {
  PackedVersion _introduced{0};
  PackedVersion _obsoleted{0};
  bool _unavailable{false};

  constexpr AvailabilityInfo(bool unavailable = false)
      : _unavailable(unavailable) {}

  constexpr AvailabilityInfo(PackedVersion i, PackedVersion o, bool u)
      : _introduced(i), _obsoleted(o), _unavailable(u) {}

  bool isDefault() const { return *this == AvailabilityInfo(); }

  llvm::Error merge(const AvailabilityInfo &other) {
    if (*this == other || other.isDefault())
      return llvm::Error::success();

    if (isDefault()) {
      *this = other;
      return llvm::Error::success();
    }

    return llvm::make_error<llvm::StringError>(
        "availabilities do not match",
        std::make_error_code(std::errc::not_supported));
  }

  void print(raw_ostream &os) const;

  friend bool operator==(const AvailabilityInfo &lhs,
                         const AvailabilityInfo &rhs);
  friend bool operator!=(const AvailabilityInfo &lhs,
                         const AvailabilityInfo &rhs);
  friend bool operator<(const AvailabilityInfo &lhs,
                        const AvailabilityInfo &rhs);
};

inline bool operator==(const AvailabilityInfo &lhs,
                       const AvailabilityInfo &rhs) {
  return std::tie(lhs._introduced, lhs._obsoleted, lhs._unavailable) ==
         std::tie(rhs._introduced, rhs._obsoleted, rhs._unavailable);
}

inline bool operator!=(const AvailabilityInfo &lhs,
                       const AvailabilityInfo &rhs) {
  return !(lhs == rhs);
}

inline bool operator<(const AvailabilityInfo &lhs,
                      const AvailabilityInfo &rhs) {
  return std::tie(lhs._introduced, lhs._obsoleted, lhs._unavailable) <
         std::tie(rhs._introduced, rhs._obsoleted, rhs._unavailable);
}

inline raw_ostream &operator<<(raw_ostream &os, const AvailabilityInfo &avail) {
  avail.print(os);
  return os;
}

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_AVAILABILITY_INFO_H
