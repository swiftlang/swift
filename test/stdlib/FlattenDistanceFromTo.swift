//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift(-parse-as-library)
// REQUIRES: executable_test
// REQUIRES: reflection
// UNSUPPORTED: use_os_stdlib
// END.
//
//===----------------------------------------------------------------------===//

import StdlibUnittest

@main
final class FlattenDistanceFromToTests {

  static func main() {
    let tests = FlattenDistanceFromToTests()
    let suite = TestSuite("FlattenDistanceFromToTests")
    suite.test("EachIndexPair", tests.testEachIndexPair)
    runAllTests()
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Each Index Pair
//===----------------------------------------------------------------------===//

extension FlattenDistanceFromToTests {
  
  /// Performs one `action` per lane size case through `limits`.
  ///
  ///     limits: [0,1,2,3]
  ///     ─────────────────
  ///     [][ ][   ][     ]
  ///     [][1][   ][     ]
  ///     [][ ][2  ][     ]
  ///     [][1][   ][     ]
  ///     [][ ][2,2][     ]
  ///     [][1][2,2][     ]
  ///     [][ ][   ][3    ]
  ///     ─────────────────
  ///     [][1][2,2][3,3,3]
  ///
  func forEachLaneSizeCase(
    through limits: [Int],
    perform action: ([[Int]]) -> Void
  ) {
    var array = Array(repeating: [Int](), count: limits.count)
    var index = array.startIndex
    while index < limits.endIndex {
      action(array)
      
      if array[index].count < limits[index] {
        array[index].append(index)
        continue
      }
      
      while index < limits.endIndex, array[index].count == limits[index] {
        array.formIndex(after: &index)
      }
      
      if index < limits.endIndex {
        array[index].append(index)
        
        while index > array.startIndex {
          array.formIndex(before: &index)
          array[index].removeAll(keepingCapacity: true)
        }
      }
    }
  }
  
  /// Performs one `action` per offset-index pair in `collection`.
  ///
  ///     collection: [[0],[1,2]].joined()
  ///     ────────────────────────────────
  ///     offset: 0, index: 0,0
  ///     offset: 1, index: 1,0
  ///     offset: 2, index: 1,1
  ///     offset: 3, index: 2
  ///
  func forEachEnumeratedIndexIncludingEndIndex<T: Collection>(
    in collection: T,
    perform action: ((offset: Int, index: T.Index)) -> Void
  ) {
    var state = (offset: 0, index: collection.startIndex)
    while true {
      action(state)
      
      if state.index == collection.endIndex {
        return
      }
      
      state.offset += 1
      collection.formIndex(after: &state.index)
    }
  }
  
  /// Checks the distance between each index pair in various cases.
  ///
  /// You need three lanes to exercise the first, the middle, the last region.
  /// The past-the-end index is a separate lane, so the middle region contains
  /// one additional lane when the past-the-end index is selected.
  ///
  func testEachIndexPair() {
    var invocations = 0 as Int
    
    for lanes in 0 ... 3 {
      let limits = Array(repeating: 3, count: lanes)
      
      forEachLaneSizeCase(through: limits) { base in
        let collection: FlattenSequence = base.joined()
        
        forEachEnumeratedIndexIncludingEndIndex(in: collection) { start in
          forEachEnumeratedIndexIncludingEndIndex(in: collection) { end in
            let pair = (from: start.offset, to: end.offset)
            
            invocations += 1
            
            expectEqual(
              collection.distance(from: start.index, to: end.index),
              end.offset - start.offset,
              """
              index distance != offset distance for \(pair) in \(base).joined()
              """
            )
          }
        }
      }
    }
    
    expectEqual(invocations, 2502, "unexpected workload")
  }
}
