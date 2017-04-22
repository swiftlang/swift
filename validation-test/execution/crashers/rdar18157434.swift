// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: not --crash %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: OS=macosx

// "cyclic metadata dependency detected, aborting"

struct X<T> {
  enum S {
  case some(T), none
  }
  
  init() { a = .none }
  var a: S
}

X<()>()

