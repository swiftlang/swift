// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

struct CountAndCapacity {
  var count: Int
  let capacity: Int
}

final class TestManagedBuffer<T> : ManagedBuffer<CountAndCapacity, T> {
  class func create(_ capacity: Int) -> TestManagedBuffer {
    let r = super.create(minimumCapacity: capacity) {
      CountAndCapacity(count: 0, capacity: $0.capacity)
    }
    return r as! TestManagedBuffer
  
  }

  var count: Int {
    get { return header.count }
    set { header.count = newValue }
  }
  
  var myCapacity: Int {
    return header.capacity
  }
  
  deinit {
    teardown()
  }

  func teardown() {
    let count = self.count
    
    withUnsafeMutablePointerToElements { (x: UnsafeMutablePointer<T>) -> () in
      for i in stride(from: 0, to: count, by: 2) {
        (x + i).deinitialize(count: 1)
      }
    }
  }
  
  func append(_ x: T) {
    let count = self.count
    precondition(count + 1 <= myCapacity)
    
    withUnsafeMutablePointerToElements { (p: UnsafeMutablePointer<T>) -> () in
      (p + count).initialize(to: x)
    }
    self.count = count + 1
  }
}

@main
struct Main {
  static func main() {
    let s = TestManagedBuffer<Int>.create(10)
    s.append(42)
    s.append(777)
    s.withUnsafeMutablePointerToElements {
      for i in 0 ..< s.count {
        print($0[i])
      }
    }
    // CHECK: 42
    // CHECK: 777
  }
}
