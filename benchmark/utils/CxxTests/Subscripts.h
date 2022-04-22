#ifndef BENCHMARK_SUBSCRIPTS_H
#define BENCHMARK_SUBSCRIPTS_H
#include <vector>

using TwoDimensionalVector = std::vector<std::vector<int>>;

inline TwoDimensionalVector initVector() { return {100, std::vector<int>{1000, 0}}; }

#endif

