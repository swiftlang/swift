func lottaClosures(x: () -> Void, y: () -> Void, z: () -> Void) {
  lottaClosures(x: {}) {} z: {}
}
// RUN: not %refactor -trailingclosure -source-filename %s -pos=2:3

func singleClosure(x: () -> Void) {
  singleClosure {}
}
// RUN: not %refactor -trailingclosure -source-filename %s -pos=7:3
