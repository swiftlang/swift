//===--- CaptureProp.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

func sum(x:Int, y:Int) -> Int {
  return x + y
}

@inline(never)
func benchCaptureProp<S : SequenceType
>(
  s:S, _ f:(S.Generator.Element, S.Generator.Element)->S.Generator.Element) -> S.Generator.Element {

  var g = s.generate()
  let initial = g.next()!
  return GeneratorSequence(g).reduce(initial, combine: f)
}

public func run_CaptureProp(N: Int) {
  let a = 1...10_000
  for _ in 1...100*N {
    benchCaptureProp(a, sum)
  }
}
