// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: not --crash %t/a.out 2>&1 | %S/../../utils/backtrace-check

// This is not supported on watchos, ios, or tvos
// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos

// REQUIRES: executable_test

func main() {
  let x = UnsafePointer<Int>(bitPattern: 0)!
  print("\(x.pointee)")
}

main()
