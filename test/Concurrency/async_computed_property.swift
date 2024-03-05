// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend  -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

func asyncFunc(_ value: String) async {}

class ComputedPropertyClass {
  var meep: String {
    //expected-error@+1:11{{'async' call in a function that does not support concurrency}}
    await asyncFunc("Meep")
    return "15"
  }
}
