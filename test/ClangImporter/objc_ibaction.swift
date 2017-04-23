// RUN: %target-swift-frontend -emit-sil %s -import-objc-header %S/Inputs/ibaction.h -verify
// REQUIRES: objc_interop

func foo(object: ConformsToIBActionInProtocol) {
  object.actionMethod(object)
}
