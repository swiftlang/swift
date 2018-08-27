// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s

import TensorFlow

public func packResultsToAggregate_addressOnlyResult<T>() -> T {
  // expected-error @+1 {{cannot extract TensorFlow result into an address-only type}}
  return #tfop("SomeOp")
}

