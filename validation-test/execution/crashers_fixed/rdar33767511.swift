// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out

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

func generic<T>(_: T) {}

// We don't want the metadata allocation to be optimized away
@_optimize(none)
func main() {
  generic(X<()>())
}

main()
