//===- llvm/Support/FloatingPointMode.h -------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Utilities for dealing with flags related to floating point mode controls.
//
//===----------------------------------------------------------------------===/

#ifndef LLVM_FLOATINGPOINTMODE_H
#define LLVM_FLOATINGPOINTMODE_H

#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {

/// Rounding mode.
///
/// Enumerates supported rounding modes, as well as some special values. The set
/// of the modes must agree with IEEE-754, 4.3.1 and 4.3.2. The constants
/// assigned to the IEEE rounding modes must agree with the values used by
/// FLT_ROUNDS (C11, 5.2.4.2.2p8).
///
/// This value is packed into bitfield in some cases, including \c FPOptions, so
/// the rounding mode values and the special value \c Dynamic must fit into the
/// the bit field (now - 3 bits). The value \c Invalid is used only in values
/// returned by intrinsics to indicate errors, it should never be stored as
/// rounding mode value, so it does not need to fit the bit fields.
///
enum class RoundingMode : int8_t {
  // Rounding mode defined in IEEE-754.
  TowardZero        = 0,    ///< roundTowardZero.
  NearestTiesToEven = 1,    ///< roundTiesToEven.
  TowardPositive    = 2,    ///< roundTowardPositive.
  TowardNegative    = 3,    ///< roundTowardNegative.
  NearestTiesToAway = 4,    ///< roundTiesToAway.

  // Special values.
  Dynamic = 7,    ///< Denotes mode unknown at compile time.
  Invalid = -1    ///< Denotes invalid value.
};

/// Represent ssubnormal handling kind for floating point instruction inputs and
/// outputs.
struct DenormalMode {
  /// Represent handled modes for denormal (aka subnormal) modes in the floating
  /// point environment.
  enum DenormalModeKind : int8_t {
    Invalid = -1,

    /// IEEE-754 denormal numbers preserved.
    IEEE,

    /// The sign of a flushed-to-zero number is preserved in the sign of 0
    PreserveSign,

    /// Denormals are flushed to positive zero.
    PositiveZero
  };

  /// Denormal flushing mode for floating point instruction results in the
  /// default floating point environment.
  DenormalModeKind Output = DenormalModeKind::Invalid;

  /// Denormal treatment kind for floating point instruction inputs in the
  /// default floating-point environment. If this is not DenormalModeKind::IEEE,
  /// floating-point instructions implicitly treat the input value as 0.
  DenormalModeKind Input = DenormalModeKind::Invalid;

  constexpr DenormalMode() = default;
  constexpr DenormalMode(DenormalModeKind Out, DenormalModeKind In) :
    Output(Out), Input(In) {}


  static constexpr DenormalMode getInvalid() {
    return DenormalMode(DenormalModeKind::Invalid, DenormalModeKind::Invalid);
  }

  static constexpr DenormalMode getIEEE() {
    return DenormalMode(DenormalModeKind::IEEE, DenormalModeKind::IEEE);
  }

  static constexpr DenormalMode getPreserveSign() {
    return DenormalMode(DenormalModeKind::PreserveSign,
                        DenormalModeKind::PreserveSign);
  }

  static constexpr DenormalMode getPositiveZero() {
    return DenormalMode(DenormalModeKind::PositiveZero,
                        DenormalModeKind::PositiveZero);
  }

  bool operator==(DenormalMode Other) const {
    return Output == Other.Output && Input == Other.Input;
  }

  bool operator!=(DenormalMode Other) const {
    return !(*this == Other);
  }

  bool isSimple() const {
    return Input == Output;
  }

  bool isValid() const {
    return Output != DenormalModeKind::Invalid &&
           Input != DenormalModeKind::Invalid;
  }

  inline void print(raw_ostream &OS) const;

  inline std::string str() const {
    std::string storage;
    raw_string_ostream OS(storage);
    print(OS);
    return OS.str();
  }
};

inline raw_ostream& operator<<(raw_ostream &OS, DenormalMode Mode) {
  Mode.print(OS);
  return OS;
}

/// Parse the expected names from the denormal-fp-math attribute.
inline DenormalMode::DenormalModeKind
parseDenormalFPAttributeComponent(StringRef Str) {
  // Assume ieee on unspecified attribute.
  return StringSwitch<DenormalMode::DenormalModeKind>(Str)
    .Cases("", "ieee", DenormalMode::IEEE)
    .Case("preserve-sign", DenormalMode::PreserveSign)
    .Case("positive-zero", DenormalMode::PositiveZero)
    .Default(DenormalMode::Invalid);
}

/// Return the name used for the denormal handling mode used by the the
/// expected names from the denormal-fp-math attribute.
inline StringRef denormalModeKindName(DenormalMode::DenormalModeKind Mode) {
  switch (Mode) {
  case DenormalMode::IEEE:
    return "ieee";
  case DenormalMode::PreserveSign:
    return "preserve-sign";
  case DenormalMode::PositiveZero:
    return "positive-zero";
  default:
    return "";
  }
}

/// Returns the denormal mode to use for inputs and outputs.
inline DenormalMode parseDenormalFPAttribute(StringRef Str) {
  StringRef OutputStr, InputStr;
  std::tie(OutputStr, InputStr) = Str.split(',');

  DenormalMode Mode;
  Mode.Output = parseDenormalFPAttributeComponent(OutputStr);

  // Maintain compatability with old form of the attribute which only specified
  // one component.
  Mode.Input = InputStr.empty() ? Mode.Output  :
               parseDenormalFPAttributeComponent(InputStr);

  return Mode;
}

void DenormalMode::print(raw_ostream &OS) const {
  OS << denormalModeKindName(Output) << ',' << denormalModeKindName(Input);
}

}

#endif // LLVM_FLOATINGPOINTMODE_H
