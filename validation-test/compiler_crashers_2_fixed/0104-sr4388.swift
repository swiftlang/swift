// RUN: %target-swift-frontend %s -emit-ir

class IndexPath {
  init(indexes: [Int]) {}
}

extension CountableRange where Bound == Int {
  func indexPaths(inSection section: Bound) -> [IndexPath]  {
    return reduce([]) { $0 + [IndexPath(indexes: [section, $1])] } 
  }
}
