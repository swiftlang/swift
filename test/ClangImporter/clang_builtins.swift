// RUN: not %target-swift-frontend -typecheck %s 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-%target-runtime %s

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  import Darwin
#elseif os(Android) || os(Cygwin) || os(FreeBSD) || os(Linux)
  import Glibc
#elseif os(Windows)
  import MSVCRT
#endif

func test() {
  let _: Int = strxfrm
  // CHECK: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  let _: Int = strcspn
  // CHECK: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  let _: Int = strspn
  // CHECK: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  let _: Int = strlen
  // CHECK: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'
}

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
// These functions aren't consistently available across platforms, so only
// test for them on Apple platforms.
func testApple() {
  let _: Int = strlcpy
  // CHECK-objc: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  let _: Int = strlcat
  // CHECK-objc: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  // wcslen is different: it wasn't a builtin until Swift 4, and so its return
  // type has always been 'Int'.
  let _: Int = wcslen
  // CHECK-objc: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'
}
#endif
