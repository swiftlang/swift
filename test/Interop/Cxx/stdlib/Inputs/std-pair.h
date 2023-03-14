#pragma once

#include <utility>

using PairInts = std::pair<int, int>;

// FIXME: return pair by value, but it causes IRGen crash atm.
inline const PairInts &getIntPair() {
    static PairInts value = { -5, 12 };
    return value;
}
