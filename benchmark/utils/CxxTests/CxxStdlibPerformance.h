#pragma once

#include <cstdint>
#include <vector>
#include <set>

// FIXME swift-ci linux tests do not support std::span
#ifndef __linux__
#include <span>
#endif

static const size_t spanSize = 50000;

using VectorOfU32 = std::vector<uint32_t>;
using SetOfU32 = std::set<uint32_t>;
#ifndef __linux__
using ArrayOfU32 = uint32_t[spanSize];
using SpanOfU32 = std::span<uint32_t>;
#endif

static inline VectorOfU32 vec;
static inline SetOfU32 set;
#ifndef __linux__
static inline ArrayOfU32 array;
static inline SpanOfU32 span;
#endif

inline void initVector(size_t size) {
    if (!vec.empty()) {
        return;
    }
    vec.reserve(size);
    for (size_t i = 0; i < size; ++i) {
        vec.push_back(uint32_t(i));
    }
}

inline void initSet(size_t size) {
    if (!set.empty()) {
        return;
    }
    for (size_t i = 0; i < size; ++i) {
        set.insert(uint32_t(i));
    }
}

#ifndef __linux__
inline void initSpan() {
    if (!span.empty()) {
        return;
    }
    for (size_t i = 0; i < spanSize; ++i) {
        array[i] = uint32_t(i);
    }
    span = SpanOfU32(array);
}
#endif

inline VectorOfU32 makeVector32(size_t size) {
    initVector(size);
    return vec;
}

inline SetOfU32 makeSet32(size_t size) {
    initSet(size);
    return set;
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
