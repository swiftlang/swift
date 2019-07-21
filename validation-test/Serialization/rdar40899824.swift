// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -emit-module -o %t/Library.swiftmodule -I %S/Inputs/custom-modules -DLIBRARY -Xfrontend -enable-objc-interop -Xfrontend -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend %s -I %t -I %S/Inputs/custom-modules -enable-objc-interop -emit-ir > /dev/null

// RUN: %target-swift-frontend %s -I %t -I %S/Inputs/custom-modules -enable-objc-interop -emit-ir -Xcc -DBAD > /dev/null
// RUN: %target-swift-frontend %s -I %t -I %S/Inputs/custom-modules -enable-objc-interop -emit-ir -Xcc -DBAD -O > /dev/null

#if LIBRARY

import rdar40899824Helper

public protocol Proto: class {
  func use(_: SoonToBeMissing)
  func unrelated()
}

extension Impl: Proto {}

#else // LIBRARY

import Library
import rdar40899824Helper

func testGeneric<T: Proto>(_ obj: T) {
  obj.unrelated()
}

func testExistential(_ obj: Proto) {
  obj.unrelated()
}

func test(_ proto: Proto, _ impl: Impl) {
  impl.unrelated()
  testGeneric(impl)
  testExistential(impl)
}

#endif // LIBRARY
