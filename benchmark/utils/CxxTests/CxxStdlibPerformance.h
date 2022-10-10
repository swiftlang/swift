#pragma once

#include <cstdint>
#include <vector>

using VectorOfU32 = std::vector<uint32_t>;

inline VectorOfU32 makeVector32(size_t size) {
    auto result = VectorOfU32();
    result.reserve(size);
    for (size_t i = 0; i < size; ++i) {
        result.push_back(uint32_t(i));
    }
    return result;
}

inline uint32_t testVector32Sum(size_t vectorSize, size_t iters) {
    auto vector = makeVector32(vectorSize);
    auto sum = uint32_t(0);
    for (size_t i = 0; i < iters; ++i) {
        for (auto x : vector) {
            sum += x;
        }
    }
    return sum;
}

// FIXME: remove when the templated operator == is correctly bridged.
inline bool operator ==(const VectorOfU32::const_iterator &lhs, const VectorOfU32::const_iterator &rhs) { return lhs.base() == rhs.base(); }
