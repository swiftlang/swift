// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

extension AutoreleasingUnsafeMutablePointer {
  func foo(memory: T) {} // expected-error {{'T' has been renamed to 'Memory'}}
}

