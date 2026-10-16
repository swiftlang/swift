// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-6.4-abi-triple -enable-experimental-feature CalledAttribute -Xllvm -sil-disable-pass=implicit-destroy-checker %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/a.out

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=maccatalyst || OS=ios || OS=tvos || OS=watchos || OS=xros

// The simulator's launcher reports a crash as an ordinary failure, so
// `not --crash` can't tell that the program crashed.
// UNSUPPORTED: DARWIN_SIMULATOR={{.*}}

final class Box {
  var count = 0
}

@inline(never)
func drop(_ f: @called(exactlyOnce) () -> Void) {}

@inline(never)
func run(_ box: Box) {
  drop { box.count += 1 }
}

run(Box())
