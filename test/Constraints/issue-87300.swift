// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/87300

func f(_ t: Any) -> Int {}
func f(_ t: AnyClass) -> String {}

func f(t: Any) -> Int {}
func f(t: AnyClass) -> String {}

class T {}

do {
  let result = f(T.self)
  let _: String = result
}

struct S: ~Copyable {}

do {
  let result = f(S.self)
  let _: Int = result
}

do {
  let result = f(t: S.self)
  let _: Int = result
}