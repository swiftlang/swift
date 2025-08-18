#ifndef SWIFTC_BASIC_SOURCELOC_H
#define SWIFTC_BASIC_SOURCELOC_H

#include "swiftc/Basic/LLVM.h"
#include <cstdint>

namespace swiftc {

/// A source location in a Swift source file.
class SourceLoc {
  uint32_t Value;

public:
  SourceLoc() : Value(0) {}
  explicit SourceLoc(uint32_t value) : Value(value) {}

  bool isValid() const { return Value != 0; }
  bool isInvalid() const { return Value == 0; }

  uint32_t getRawValue() const { return Value; }

  bool operator==(SourceLoc other) const { return Value == other.Value; }
  bool operator!=(SourceLoc other) const { return Value != other.Value; }
  bool operator<(SourceLoc other) const { return Value < other.Value; }
  bool operator<=(SourceLoc other) const { return Value <= other.Value; }
  bool operator>(SourceLoc other) const { return Value > other.Value; }
  bool operator>=(SourceLoc other) const { return Value >= other.Value; }

  static SourceLoc getInvalidLoc() { return SourceLoc(); }
};

/// A source range in a Swift source file.
class SourceRange {
  SourceLoc Start, End;

public:
  SourceRange() = default;
  SourceRange(SourceLoc start, SourceLoc end) : Start(start), End(end) {}
  explicit SourceRange(SourceLoc loc) : Start(loc), End(loc) {}

  SourceLoc getStart() const { return Start; }
  SourceLoc getEnd() const { return End; }

  bool isValid() const { return Start.isValid() && End.isValid(); }
  bool isInvalid() const { return !isValid(); }

  bool operator==(const SourceRange &other) const {
    return Start == other.Start && End == other.End;
  }
  bool operator!=(const SourceRange &other) const {
    return !(*this == other);
  }
};

} // namespace swiftc

#endif // SWIFTC_BASIC_SOURCELOC_H