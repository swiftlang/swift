// RUN: %target-swift-emit-silgen %s

// rdar://problem/53956564

func foo<T>(x: Int, y: T, z: Int) {
  switch (x, y, z) {
  case (let xx, let yy, let zz), (let xx, let yy, let zz):
    _ = xx
    _ = yy
    _ = zz
    break
  }
}
