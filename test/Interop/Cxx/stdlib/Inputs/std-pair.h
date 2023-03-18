#pragma once

#include <utility>

using PairInts = std::pair<int, int>;

inline const PairInts &getIntPairPointer() {
    static PairInts value = { 4, 9 };
    return value;
}

inline PairInts getIntPair() {
    return { -5, 12 };
}

struct StructInPair {
    int x;
    int y;
};

using PairStructInt = std::pair<StructInPair, int>;

inline PairStructInt getPairStructInt(int x) {
    return { { x * 2, -x}, x };
}
