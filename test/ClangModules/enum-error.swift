// RUN: %target-swift-frontend -emit-sil %s -import-objc-header %S/Inputs/enum-error.h -verify
// REQUIRES: OS=macosx

import Foundation

func testError() {
  let terr = getErr()
  switch (terr) { case .TENone, .TEOne, .TETwo: break } // ok

  switch (terr) { case .TENone, .TEOne: break }
    // expected-error@-1 {{switch must be exhaustive, consider adding a default clause}}

  let _ = TestError(rawValue: 2)!

  do {
    throw TestError.TEOne
  } catch is TestError {
  } catch {
  }

  func errorIdentity<T>(err: T) -> T { return err }
  func errorIdentityBridged<T : _BridgedNSError>(err: T) -> T { return err }
  do {
    throw errorIdentityBridged(errorIdentity(TestError.TETwo))
  } catch is TestError {
  } catch {
  }

}
