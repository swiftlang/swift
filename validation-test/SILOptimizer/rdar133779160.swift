// RUN: %target-build-swift %s -sanitize=thread

// REQUIRES: tsan_runtime

class C {}
func passC(_ b: consuming C) {
  mutateC(&b)
}
func mutateC(_ b: inout C) {}
