// RUN: %target-swift-frontend -emit-ir %s

func test(_ data: UnsafePointer<Int8>?, len: Int) {
  data?.withMemoryRebound(to: UInt8.self, capacity: len) {
    data in
    for i in 0..<len {
      print("\(data[i])")
    }
  }
}
