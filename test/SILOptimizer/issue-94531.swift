// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -primary-file %s -o /dev/null

@inline(never)
func touch(_ value: inout Box) {}

struct Box {
  var storage: Any

  func updateInout() {
    withUnsafePointer(to: self) {
      touch(&UnsafeMutablePointer(mutating: $0).pointee)
    }
  }

  func updateAssign() {
    withUnsafePointer(to: self) {
      UnsafeMutablePointer(mutating: $0).pointee = Box(storage: 1)
    }
  }
}
