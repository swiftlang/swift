// RUN: %target-swift-frontend -emit-sil -verify -import-objc-header %S/Inputs/c-func-member-init.h %s
// REQUIRES: objc_interop

extension MyObject {
  convenience init() {
    self.init(id: 1738)
  }
}
