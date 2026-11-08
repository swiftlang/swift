// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

// https://github.com/swiftlang/swift/issues/94098

enum IndirectCaseBox<let dimensions: Int> {
  case point([dimensions of Double])
  indirect case box(IndirectCaseBox)
}

enum ArrayBox<let dimensions: Int> {
  case point([dimensions of Double])
  case boxes([ArrayBox])
}

enum IndirectArrayBox<let dimensions: Int> {
  case point([dimensions of Double])
  indirect case boxes([IndirectArrayBox])
}

func sum<let n: Int>(_ point: [n of Double]) -> Double {
  var result = 0.0
  for i in point.indices {
    result += point[i]
  }
  return result
}

func depth<let n: Int>(_ box: IndirectCaseBox<n>) -> (Int, Double) {
  switch box {
  case .point(let p):
    return (0, sum(p))
  case .box(let inner):
    let (d, s) = depth(inner)
    return (d + 1, s)
  }
}

func total<let n: Int>(_ box: ArrayBox<n>) -> Double {
  switch box {
  case .point(let p):
    return sum(p)
  case .boxes(let boxes):
    return boxes.reduce(0) { $0 + total($1) }
  }
}

func total<let n: Int>(_ box: IndirectArrayBox<n>) -> Double {
  switch box {
  case .point(let p):
    return sum(p)
  case .boxes(let boxes):
    return boxes.reduce(0) { $0 + total($1) }
  }
}

do {
  var box = IndirectCaseBox<3>.box(.box(.point([1, 2, 3])))
  // CHECK: depth 2, sum 6.0
  let (d, s) = depth(box)
  print("depth \(d), sum \(s)")

  // Reassignment exercises the value witnesses that destroy the old value.
  box = .box(.point([4, 5, 6]))
  // CHECK: depth 1, sum 15.0
  let (d2, s2) = depth(box)
  print("depth \(d2), sum \(s2)")
}

do {
  var box = ArrayBox<2>.boxes([.point([1, 2]), .boxes([.point([3, 4])])])
  // CHECK: array total 10.0
  print("array total \(total(box))")
  box = .point([5, 6])
  // CHECK: array total 11.0
  print("array total \(total(box))")
}

do {
  var box = IndirectArrayBox<2>.boxes([.point([1, 2]), .boxes([.point([3, 4])])])
  // CHECK: indirect array total 10.0
  print("indirect array total \(total(box))")
  box = .point([5, 6])
  // CHECK: indirect array total 11.0
  print("indirect array total \(total(box))")
}
