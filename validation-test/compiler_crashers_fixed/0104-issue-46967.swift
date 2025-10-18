// RUN: %target-swift-frontend %s -emit-ir

// https://github.com/apple/swift/issues/46967

class IndexPath {
  init(indexes: [Int]) {}
}

extension CountableRange where Bound == Int {
  func indexPaths(inSection section: Bound) -> [IndexPath]  {
    return reduce([]) { $0 + [IndexPath(indexes: [section, $1])] } 
  }
}
