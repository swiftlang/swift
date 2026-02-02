// RUN: %target-run-simple-swift(-O -Xllvm -sil-disable-pass=deadobject-elim) | %FileCheck %s

// REQUIRES: executable_test

#if canImport(Darwin)
  import Darwin
#elseif canImport(Glibc)
  import Glibc
#elseif os(WASI)
  import WASILibc
#elseif canImport(Android)
  import Android
#elseif os(Windows)
  import CRT
#else
#error("Unsupported platform")
#endif

@inline(never)
public func hiddenExit() {
  // CHECK: okay
  print("okay")
  exit(0)
}

func foo() -> Never {
  while true {
    hiddenExit()
  }
}

func bar(_ x: Any...) {
}

bar(foo())
