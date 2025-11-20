// RUN: %target-swift-frontend %s -typecheck

// https://github.com/apple/swift/issues/43442

func quickSort<T: Comparable>(_ a: [T]) -> [T] {
  if a.isEmpty {
    return a
  } else {
    let head = a[0]
    let body = a[1..<a.count]
    return quickSort(body.filter({$0 < head})) + [head] + quickSort(body.filter({$0 >= head}))
  }
}
