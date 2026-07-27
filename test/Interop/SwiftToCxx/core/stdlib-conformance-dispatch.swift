// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/ConformLib.swift -module-name ConformLib -clang-header-expose-decls=all-public -emit-module -emit-module-path %t/ConformLib.swiftmodule -emit-clang-header-path %t/ConformLib.h -cxx-interoperability-mode=upcoming-swift -parse-as-library -enable-experimental-feature CxxExistentialInterop

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %t/test.cpp -I %t -o %t/test.o
// RUN: %target-interop-build-swift %t/ConformLib.swift -o %t/ConformLib.o -c -module-name ConformLib -cxx-interoperability-mode=default -parse-as-library
// RUN: %target-interop-build-clangxx %t/test.o %t/ConformLib.o -o %t/test
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExistentialInterop

//--- ConformLib.swift

public struct Point: Equatable, Hashable {
    public var x: Int
    public var y: Int
    public init(x: Int, y: Int) { self.x = x; self.y = y }
}

public struct Temperature: Comparable {
    public var degrees: Int
    public init(degrees: Int) { self.degrees = degrees }
    public static func < (lhs: Temperature, rhs: Temperature) -> Bool {
        return lhs.degrees < rhs.degrees
    }
    public static func == (lhs: Temperature, rhs: Temperature) -> Bool {
        return lhs.degrees == rhs.degrees
    }
}

//--- test.cpp

#include "ConformLib.h"
#include <algorithm>
#include <cassert>
#include <set>
#include <vector>

int main() {
  using namespace ConformLib;

  // --- Direct operator dispatch ---

  auto p1 = Point::init(1, 2);
  auto p2 = Point::init(1, 2);
  auto p3 = Point::init(3, 4);

  assert(p1 == p2);
  assert(!(p1 == p3));
  assert(p1 != p3);
  assert(!(p1 != p2));

  auto t1 = Temperature::init(20);
  auto t2 = Temperature::init(30);
  auto t3 = Temperature::init(20);

  assert(t1 < t2);
  assert(!(t2 < t1));
  assert(t1 <= t2);
  assert(t1 <= t3);
  assert(t2 > t1);
  assert(!(t1 > t2));
  assert(t2 >= t1);
  assert(t1 >= t3);

  assert(t1 == t3);
  assert(!(t1 == t2));
  assert(t1 != t2);

  // --- STL algorithms over Equatable types ---

  std::vector<Point> points = {
    Point::init(3, 4),
    Point::init(1, 2),
    Point::init(3, 4),
    Point::init(5, 6),
    Point::init(1, 2),
  };

  // std::find dispatches through operator==.
  auto it = std::find(points.begin(), points.end(), Point::init(5, 6));
  assert(it != points.end());
  assert(*it == Point::init(5, 6));

  auto missing = std::find(points.begin(), points.end(), Point::init(9, 9));
  assert(missing == points.end());

  // std::count dispatches through operator==.
  assert(std::count(points.begin(), points.end(), Point::init(3, 4)) == 2);
  assert(std::count(points.begin(), points.end(), Point::init(1, 2)) == 2);
  assert(std::count(points.begin(), points.end(), Point::init(5, 6)) == 1);

  // --- STL algorithms over Comparable types ---

  std::vector<Temperature> temps = {
    Temperature::init(30),
    Temperature::init(10),
    Temperature::init(20),
    Temperature::init(10),
    Temperature::init(40),
  };

  // std::sort dispatches through operator<.
  std::sort(temps.begin(), temps.end());
  assert(temps[0] == Temperature::init(10));
  assert(temps[1] == Temperature::init(10));
  assert(temps[2] == Temperature::init(20));
  assert(temps[3] == Temperature::init(30));
  assert(temps[4] == Temperature::init(40));

  // std::unique dispatches through operator==.
  auto last = std::unique(temps.begin(), temps.end());
  temps.erase(last, temps.end());
  assert(temps.size() == 4);
  assert(temps[0] == Temperature::init(10));
  assert(temps[1] == Temperature::init(20));
  assert(temps[2] == Temperature::init(30));
  assert(temps[3] == Temperature::init(40));

  // std::min / std::max dispatch through operator<.
  auto lo = std::min(Temperature::init(15), Temperature::init(25));
  auto hi = std::max(Temperature::init(15), Temperature::init(25));
  assert(lo == Temperature::init(15));
  assert(hi == Temperature::init(25));

  // std::min_element / std::max_element.
  auto minIt = std::min_element(temps.begin(), temps.end());
  auto maxIt = std::max_element(temps.begin(), temps.end());
  assert(*minIt == Temperature::init(10));
  assert(*maxIt == Temperature::init(40));

  // std::lower_bound (binary search) dispatches through operator<.
  auto lb = std::lower_bound(temps.begin(), temps.end(), Temperature::init(20));
  assert(lb != temps.end());
  assert(*lb == Temperature::init(20));

  auto ub = std::upper_bound(temps.begin(), temps.end(), Temperature::init(20));
  assert(ub != temps.end());
  assert(*ub == Temperature::init(30));

  // std::binary_search.
  assert(std::binary_search(temps.begin(), temps.end(), Temperature::init(30)));
  assert(!std::binary_search(temps.begin(), temps.end(), Temperature::init(35)));

  // --- STL ordered containers ---

  // std::set uses operator< for ordering.
  std::set<Temperature> tempSet;
  tempSet.insert(Temperature::init(30));
  tempSet.insert(Temperature::init(10));
  tempSet.insert(Temperature::init(20));
  tempSet.insert(Temperature::init(10)); // duplicate
  assert(tempSet.size() == 3);
  assert(tempSet.count(Temperature::init(10)) == 1);
  assert(tempSet.count(Temperature::init(99)) == 0);

  auto setIt = tempSet.begin();
  assert(*setIt == Temperature::init(10)); ++setIt;
  assert(*setIt == Temperature::init(20)); ++setIt;
  assert(*setIt == Temperature::init(30));

  return 0;
}
