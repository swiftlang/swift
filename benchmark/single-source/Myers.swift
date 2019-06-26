//===--- Myers.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let Myers = [
  BenchmarkInfo(name: "Myers", runFunction: run_Myers, tags: [.algorithm]),
]

let loremShort = Array("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
let loremLong = Array("Sed ut perspiciatis, unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam eaque ipsa, quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt, explicabo. Nemo enim ipsam voluptatem, quia voluptas sit, aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos, qui ratione voluptatem sequi nesciunt, neque porro quisquam est, qui dolorem ipsum, quia dolor sit amet consectetur adipisci[ng] velit, sed quia non-numquam [do] eius modi tempora inci[di]dunt, ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum[d] exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit, qui in ea voluptate velit esse, quam nihil molestiae consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla pariatur?")

@inline(never)
public func run_Myers(N: Int) {
  if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) { // FIXME(availability-5.1)
    for _ in 1...N {
      let _ = myers(from: loremShort, to: loremLong)
    }
  }
}

// Extremely rudimentary type made to represent the rows of the triangular matrix type used by the Myer's algorithm
//
// This type is basically an array that only supports indexes in the set stride(from: -d, through: d, by: 2) where d is the depth of this row in the matrix
fileprivate struct V {
    private var a: [Int]
    
    // The way negative indexes are implemented is by interleaving them in the empty slots between the valid positive indexes
    @inline(__always) private static func transform(_ index: Int) -> Int {
        // 0 -> 0
        // 1 -> 0
        // -3, -1, 1, 3 -> 3, 1, 0, 2 -> 0...3
        // -2, 0, 2 -> 2, 0, 1 -> 0...2
        return (index <= 0 ? -index : index &- 1)
    }
    
    init(maxIndex largest: Int) {
        a = [Int](repeating: 0, count: largest + 1)
    }
    
    subscript(index: Int) -> Int {
        get {
            return a[V.transform(index)]
        }
        set(newValue) {
            a[V.transform(index)] = newValue
        }
    }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) // FIXME(availability-5.1)
fileprivate func myers<C>(
    from old: C, to new: C,
    using cmp: (C.Element, C.Element) -> Bool = (==)
) -> CollectionDifference<C.Element>
    where
    C : BidirectionalCollection,
    C.Element : Equatable
{
    
    func descent(from a: [C.Element], to b: [C.Element]) -> [V] {
        let n = a.count
        let m = b.count
        let max = n + m
        
        var result = [V]()
        var v = V(maxIndex: 1)
        v[1] = 0
        
        var x = 0
        var y = 0
        iterator: for d in 0...max {
            let prev_v = v
            result.append(v)
            v = V(maxIndex: d)
            
            // The code in this loop is very hot—the loop bounds increases in terms
            // of the iterator of the outer loop!
            for k in stride(from: -d, through: d, by: 2) {
                if k == -d {
                    x = prev_v[k &+ 1]
                } else {
                    let km = prev_v[k &- 1]
                    
                    if k != d {
                        let kp = prev_v[k &+ 1]
                        if km < kp {
                            x = kp
                        } else {
                            x = km &+ 1
                        }
                    } else {
                        x = km &+ 1
                    }
                }
                y = x &- k
                
                while x < n && y < m {
                    if !cmp(a[x], b[y]) {
                        break;
                    }
                    x &+= 1
                    y &+= 1
                }
                
                v[k] = x
                
                if x >= n && y >= m { break }
            }
            if x >= n && y >= m { break }
        }
        
        assert(x >= n && y >= m)
        
        return result
    }
    
    /* Splatting the collections into arrays here has two advantages:
     *
     *     1) Subscript access becomes inlined
     *     2) Subscript index becomes Int, matching the iterator types in the algorithm
     *
     * Combined, these effects dramatically improves performance when
     * collections differ significantly, without unduly degrading runtime when
     * the parameters are very similar.
     *
     * In terms of memory use, the linear cost is significantly less than the
     * worst-case n² memory use of the descent algorithm.
     */
    let a = Array(old)
    let b = Array(new)

    let trace = descent(from: a, to: b)
    
    var changes = [CollectionDifference<C.Element>.Change]()
    changes.reserveCapacity(trace.count)
    
    var x = a.count
    var y = b.count
    for d in stride(from: trace.count &- 1, to: 0, by: -1) {
        let v = trace[d]
        let k = x &- y
        let prev_k = (k == -d || (k != d && v[k &- 1] < v[k &+ 1])) ? k &+ 1 : k &- 1
        let prev_x = v[prev_k]
        let prev_y = prev_x &- prev_k
        
        while x > prev_x && y > prev_y {
            // No change at this position.
            x &-= 1
            y &-= 1
        }
        
        assert((x == prev_x && y > prev_y) || (y == prev_y && x > prev_x))
        if y != prev_y {
            changes.append(.insert(offset: prev_y, element: b[prev_y], associatedWith: nil))
        } else {
            changes.append(.remove(offset: prev_x, element: a[prev_x], associatedWith: nil))
        }
        
        x = prev_x
        y = prev_y
    }
    
    return CollectionDifference(changes)!
}
