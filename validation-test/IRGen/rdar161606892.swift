// RUN: %target-swift-frontend %s -target %target-swift-5.9-abi-triple -emit-irgen | %IRGenFileCheck %s

// CHECK: Attrs: noinline nounwind{{$}}
// CHECK-NEXT: define {{.*}}@"$s13rdar1616068921PVMa"

public struct P<each T> {
  public var teas: (repeat each T)
}
