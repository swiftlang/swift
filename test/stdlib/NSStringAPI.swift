// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -i -module-cache-path=%t/clang-module-cache -sdk=%sdk %s | FileCheck %s
// REQUIRES: sdk
// REQUIRES: swift_interpreter

import Foundation

func testClassMethods() {
  var encodings: NSStringEncoding[] = String.availableStringEncodings()
  // CHECK: available encodings found
  // CHECK-NOT: 0 available encodings
  println("\(encodings.count) available encodings found")

  // CHECK-NEXT: defaultCStringEncoding is available
  var defaultCStringEncoding = String.defaultCStringEncoding()
  for e in encodings {
    if e == defaultCStringEncoding {
      println("defaultCStringEncoding is available")
    }
  }

  // CHECK-NEXT: It is called
  println("It is called \"\(String.localizedNameOfStringEncoding(defaultCStringEncoding))\"")

  var path = String.pathWithComponents(["flugelhorn", "baritone", "bass"])
  // CHECK-NEXT: <flugelhorn/baritone/bass>
  println("<\(path)>")

  // CHECK-NEXT: true
  println(String.string() == "")

  // CHECK-NEXT: <sox>
  var chars: unichar[] = [ unichar('s'.value), unichar('o'.value), unichar('x'.value) ]
  var sox: String = String.stringWithCharacters(chars.base, chars.count)
  println("<\(sox)>")
}


testClassMethods()
// CHECK: done!
println("done!")
