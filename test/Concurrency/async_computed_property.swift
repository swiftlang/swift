// RUN: %target-typecheck-verify-swift  -disable-availability-checking
// REQUIRES: concurrency

func asyncFunc(_ value: String) async {}

class ComputedPropertyClass {
  var meep: String {
    //expected-error@+1:11{{'async' call in a function that does not support concurrency}}
    await asyncFunc("Meep")
    return "15"
  }
}
