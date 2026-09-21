// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=wasip1 || OS=emscripten
// REQUIRES: swift_feature_Embedded

#if canImport(WASILibc)
import WASILibc
#elseif canImport(EmscriptenLibc)
import EmscriptenLibc
#else
#error("Unsupported libc")
#endif

@main struct Main {
  static func main() {
    puts("Hello")
    // CHECK: Hello
    let div_result = div(5,2)
    print(div_result.quot)
    // CHECK: 2
    print(div_result.rem)
    // CHECK: 1
  }
}
