// rdar://183793871 (https://github.com/swiftlang/swift/issues/91156)
// RUN: %target-typecheck-verify-swift

struct Job {
  var partSize: Int?
}

func check(_ job: Job?) {
  let x: Int64? = job?.partSize // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  _ = x
}
