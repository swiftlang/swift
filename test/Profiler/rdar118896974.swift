// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// rdar://118896974 - Make sure we don't crash.
func foo() {
  do {
    return
  } catch {
    return
  }
}
