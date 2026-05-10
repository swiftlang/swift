#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_MAP_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_MAP_H

#include <map>
#include <string>
#include <unordered_map>
#include <vector>

using Map = std::map<int, int>;
using MapStrings = std::map<std::string, std::string>;
using NestedMap = std::map<int, Map>;
using MapGroup = std::map<int, std::vector<int>>;
using MapIntString = std::map<int, std::string>;
using UnorderedMap = std::unordered_map<int, int>;
using UnorderedMapGroup = std::unordered_map<int, std::vector<int>>;
using UnorderedIntString = std::unordered_map<int, std::string>;
inline Map initMap() { return {{1, 3}, {2, 2}, {3, 3}}; }
inline UnorderedMap initUnorderedMap() { return {{1, 3}, {3, 3}, {2, 2}}; }
inline Map initEmptyMap() { return {}; }
inline UnorderedMap initEmptyUnorderedMap() { return {}; }

struct NonCopyable {
  NonCopyable(int n) : number(n) {}
  NonCopyable(const NonCopyable &other) = delete;
  NonCopyable(NonCopyable &&other) = default;
  ~NonCopyable() {}

  int number;
};

using MapNonCopyableKey = std::map<NonCopyable, int>;
using MapNonCopyableValue = std::map<int, NonCopyable>;

inline MapNonCopyableValue initMapNonCopyableValue() {
  MapNonCopyableValue m;
  m.emplace(1, 1);
  m.emplace(3, 3);
  m.emplace(7, 7);
  return m;
}

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_MAP_H
