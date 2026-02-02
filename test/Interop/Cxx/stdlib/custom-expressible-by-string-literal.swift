// RUN: %target-swift-emit-silgen -verify -I %S/Inputs -cxx-interoperability-mode=upcoming-swift %s | %FileCheck %s

import CxxStdlib
import StdString

extension StringBox: @retroactive ExpressibleByStringLiteral {
  public init(stringLiteral value: String) {
    self.value = std.string(value)
  }
}

func takesAny(_: Any) {}

// CHECK-LABEL: sil hidden [ossa] @$s4main4testyyF
// CHECK:   // function_ref static std{{.*}}basic_string<CChar, std{{.*}}char_traits<CChar>, std{{.*}}allocator<CChar>>.== infix(_:_:)
// CHECK:   // function_ref static std{{.*}}basic_string<CChar, std{{.*}}char_traits<CChar>, std{{.*}}allocator<CChar>>.== infix(_:_:)
// CHECK:   // function_ref static String.== infix(_:_:)
// CHECK:   // function_ref static String.+ infix(_:_:)
// CHECK:   // function_ref static StringBox.+ infix(_:_:)
// CHECK-NEXT: %{{.*}} = function_ref @$sSo9StringBoxV1poiyA2B_ABtFZ
// CHECK:   // function_ref takesAny(_:)
// CHECK-NEXT: %{{.*}} = function_ref @$s4main8takesAnyyyypF
// CHECK: } // end sil function '$s4main4testyyF'
func test() {
  let cxxString: std.string = ""
  let _ = cxxString == "def" // Ok
  let _ = "def" == cxxString // Ok
  let _ = "def" == "hello" // Ok
  
  takesAny("abc" + "def")
  takesAny("abc" + "def" + "xyz")
  takesAny(StringBox("abc") + "def" + "xyz")
  takesAny("abc" + StringBox("def") + "xyz")
}
