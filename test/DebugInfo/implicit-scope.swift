// RUN: %target-swift-frontend -emit-ir -g %s | %FileCheck %s

struct S { let i: Int }

// CHECK: define {{.*}}s4mainAAyyFySRyAA1SVGXEfU_
public func main() {
  // UnsafeBufferPointer
  let array = [S(i: 1)]
  array.withUnsafeBufferPointer {
    let buf = $0
    // Test that the first instruction on the next line is in the scope of "buf"
    // CHECK: store {{.*}}, !dbg ![[LOC0:[0-9]+]]
    // CHECK: call {{.*builtinStringLiteral.*}}, !dbg ![[LOC1:[0-9]+]]
    // CHECK: ![[LOC0]] = {{.*}}line: 0
    // CHECK: ![[LOC1]] = {{.*}}line: [[@LINE+1]]
    print("break here ...")
  }
}
