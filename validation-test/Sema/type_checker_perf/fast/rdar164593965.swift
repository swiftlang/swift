// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000 -solver-enable-extreme-edge-contraction -solver-enable-promote-supertypes

func test0() -> [Double] {
  return [1.0/8.0, 1.0/4.0, 1.0/3.0, 1.0/2.0, 2.0/3.0, 3.0/4.0, 1.0, 5.0/4.0, 4.0/3.0, 3.0/2.0, 2.0, 4.0, 8.0].map{ $0/8.0 }
}

func test1() -> [Double] {
  return [1/8.0, 1/4.0, 1/3.0, 1/2.0, 2/3.0, 3/4.0, 1, 5/4.0, 4/3.0, 3/2.0, 2, 4, 8].map{ $0/8.0 }
}

func test2() -> [Double] {
  return [1/8, 1/4, 1/3, 1/2, 2/3, 3/4, 1, 5/4, 4/3, 3/2, 2, 4, 8].map{ $0/8.0 }
}

func test3() -> [Float] {
  return [1.0/8.0, 1.0/4.0, 1.0/3.0, 1.0/2.0, 2.0/3.0, 3.0/4.0, 1.0, 5.0/4.0, 4.0/3.0, 3.0/2.0, 2.0, 4.0, 8.0].map{ $0/8.0 }
}

func test4() -> [Float] {
  return [1/8.0, 1/4.0, 1/3.0, 1/2.0, 2/3.0, 3/4.0, 1, 5/4.0, 4/3.0, 3/2.0, 2, 4, 8].map{ $0/8.0 }
}

func test5() -> [Float] {
  return [1/8, 1/4, 1/3, 1/2, 2/3, 3/4, 1, 5/4, 4/3, 3/2, 2, 4, 8].map{ $0/8.0 }
}

func test6() -> [Int] {
  return [1/8, 1/4, 1/3, 1/2, 2/3, 3/4, 1, 5/4, 4/3, 3/2, 2, 4, 8].map{ $0/8 }
}

func test7() -> [Int32] {
  return [1/8, 1/4, 1/3, 1/2, 2/3, 3/4, 1, 5/4, 4/3, 3/2, 2, 4, 8].map{ $0/8 }
}
