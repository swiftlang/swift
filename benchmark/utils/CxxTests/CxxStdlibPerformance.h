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

inline uint32_t testVector32Sum(VectorOfU32 &vector) {
    auto sum = uint32_t(0);
    for (auto i : vector) {
        sum += i;
    }
    return sum;
}

template<class T>
inline T next(const T& i) { return i + 1; }

template<class T>
inline bool cmp(const T &lhs, const T &rhs) { return lhs == rhs; }
