//===- tapi/Core/ArchitectureSet.h - Architecture Set -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the architecture set.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_ARCHITECTURE_SET_H
#define TAPI_CORE_ARCHITECTURE_SET_H

#include "Architecture.h"
#include "ArchitectureConfig.h"
#include "LLVM.h"
#include "Defines.h"
#include "llvm/ADT/Triple.h"
#include <iterator>
#include <limits>
#include <stddef.h>
#include <tuple>
#include <vector>

TAPI_NAMESPACE_INTERNAL_BEGIN

class ArchitectureSet {
private:
  using ArchSetType = uint32_t;

  const static ArchSetType _endIndexVal =
      std::numeric_limits<ArchSetType>::max();
  ArchSetType _archSet{0};

public:
  constexpr ArchitectureSet() = default;
  ArchitectureSet(ArchSetType raw) : _archSet(raw) {}
  ArchitectureSet(Architecture arch) : ArchitectureSet() { set(arch); }
  ArchitectureSet(const std::vector<Architecture> &archs) : ArchitectureSet() {
    for (auto arch : archs) {
      if (arch == Architecture::unknown)
        continue;
      set(arch);
    }
  }

  static ArchitectureSet All() {
    return ArchitectureSet(_endIndexVal);
  }

  void set(Architecture arch) {
    if (arch == Architecture::unknown)
      return;
    _archSet |= 1U << static_cast<int>(arch);
  }
  void clear(Architecture arch) { _archSet &= ~(1U << static_cast<int>(arch)); }
  bool has(Architecture arch) const {
    return _archSet & (1U << static_cast<int>(arch));
  }
  bool contains(ArchitectureSet archs) const {
    return (_archSet & archs._archSet) == archs._archSet;
  }

  size_t count() const {
    // popcnt
    size_t cnt = 0;
    for (unsigned i = 0; i < sizeof(ArchSetType) * 8; ++i)
      if (_archSet & (1U << i))
        ++cnt;
    return cnt;
  }

  bool empty() const { return _archSet == 0; }

  ArchSetType rawValue() const { return _archSet; }

  bool hasX86() const {
#ifdef SUPPORT_ARCH_I386
    if (has(Architecture::i386))
      return true;
#endif

#ifdef SUPPORT_ARCH_X86_64
    if (has(Architecture::x86_64))
      return true;
#endif

#ifdef SUPPORT_ARCH_X86_64H
    if (has(Architecture::x86_64h))
      return true;
#endif

    return false;
  }

  bool hasABICompatibleSlice(Architecture arch) const;

  Architecture getABICompatibleSlice(Architecture arch) const;

  template <typename Ty>
  class arch_iterator
      : public std::iterator<std::forward_iterator_tag, Architecture, size_t> {
  private:
    ArchSetType _index;
    Ty *_archSet;

    void findNextSetBit() {
      if (_index == _endIndexVal)
        return;

      do {
        if (*_archSet & (1UL << ++_index))
          return;
      } while (_index < sizeof(Ty) * 8);

      _index = _endIndexVal;
    }

  public:
    arch_iterator(Ty *archSet, ArchSetType index = 0)
        : _index(index), _archSet(archSet) {
      if (index != _endIndexVal && !(*_archSet & (1UL << index)))
        findNextSetBit();
    }

    Architecture operator*() const { return static_cast<Architecture>(_index); }

    arch_iterator &operator++() {
      findNextSetBit();
      return *this;
    }

    arch_iterator operator++(int) {
      auto tmp = *this;
      findNextSetBit();
      return tmp;
    }

    bool operator==(const arch_iterator &o) const {
      return std::tie(_index, _archSet) == std::tie(o._index, o._archSet);
    }

    bool operator!=(const arch_iterator &o) const { return !(*this == o); }
  };

  ArchitectureSet operator&(const ArchitectureSet &o) {
    return {_archSet & o._archSet};
  }

  ArchitectureSet operator|(const ArchitectureSet &o) {
    return {_archSet | o._archSet};
  }

  ArchitectureSet &operator|=(const ArchitectureSet &o) {
    _archSet |= o._archSet;
    return *this;
  }

  bool operator==(const ArchitectureSet &o) const {
    return _archSet == o._archSet;
  }

  bool operator!=(const ArchitectureSet &o) const {
    return _archSet != o._archSet;
  }

  bool operator<(const ArchitectureSet &o) const {
    return _archSet < o._archSet;
  }

  using iterator = arch_iterator<ArchSetType>;
  using const_iterator = arch_iterator<const ArchSetType>;

  iterator begin() { return {&_archSet}; }
  iterator end() { return {&_archSet, _endIndexVal}; }

  const_iterator begin() const { return {&_archSet}; }
  const_iterator end() const { return {&_archSet, _endIndexVal}; }

  operator std::string() const;
  operator std::vector<Architecture>() const;
  void print(raw_ostream &os) const;
};

ArchitectureSet mapToArchitectureSet(const std::vector<llvm::Triple> &targets);

raw_ostream &operator<<(raw_ostream &os, ArchitectureSet set);

const DiagnosticBuilder &operator<<(const DiagnosticBuilder &db,
                                    ArchitectureSet architectureSet);

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_ARCHITECTURE_SET_H
