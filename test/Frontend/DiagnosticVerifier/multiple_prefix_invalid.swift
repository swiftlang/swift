// RUN: not --crash %target-swift-frontend %s -verify -verify-additional-prefix first- -verify-additional-prefix first-second- -emit-sil -o /dev/null 2>&1 | %FileCheck %s

// This test makes sure that if we add prefixes such that an earlier prefix is a
// prefix of a later prefix, we crash. We do this since the earlier prefix will
// always succeed whenever the later prefix would succeed implying that this was
// done through programmer error. So it is better to crash to indicate
// programmer error rather than have the compiler author be confused.

// CHECK: Error! Found a verifier diagnostic additional prefix that is a prefix of a later prefix. The later prefix will never be pattern matched!
// CHECK: First Prefix: first-
// CHECK: Second Prefix: first-second-

func test() {

}
