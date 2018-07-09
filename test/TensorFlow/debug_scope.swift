// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-sil %s -verify

// This test checks that deabstraction passes debug scope verification with -Onone.
// Prior to https://github.com/apple/swift/pull/17797, this test failed with:
// SIL verification failed: Basic block contains a non-contiguous lexical scope at -Onone: DS == LastSeenScope

import TensorFlow
func test() {
  _ = Tensor(1)
}
