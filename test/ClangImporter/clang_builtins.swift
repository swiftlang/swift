// RUN: not %target-swift-frontend -swift-version 3 -typecheck %s 2>&1 | %FileCheck -check-prefix=CHECK-3 -check-prefix=CHECK-%target-runtime-3 %s
// RUN: not %target-swift-frontend -swift-version 4 -typecheck %s 2>&1 | %FileCheck -check-prefix=CHECK-4 -check-prefix=CHECK-%target-runtime-4 %s

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
  import Darwin
#else
  import Glibc
#endif

func test() {
  let _: Int = strxfrm
  // CHECK-3: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> UInt'{{( [(]aka .+[)])?}} to specified type 'Int'
  // CHECK-4: [[@LINE-2]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  let _: Int = strcspn
  // CHECK-3: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> UInt'{{( [(]aka .+[)])?}} to specified type 'Int'
  // CHECK-4: [[@LINE-2]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  let _: Int = strspn
  // CHECK-3: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> UInt'{{( [(]aka .+[)])?}} to specified type 'Int'
  // CHECK-4: [[@LINE-2]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  let _: Int = strlen
  // CHECK-3: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> UInt'{{( [(]aka .+[)])?}} to specified type 'Int'
  // CHECK-4: [[@LINE-2]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'
}

// These functions aren't consistently available across platforms, so only
// test for them on Apple platforms.
func testApple() {
  let _: Int = strlcpy
  // CHECK-objc-3: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> UInt'{{( [(]aka .+[)])?}} to specified type 'Int'
  // CHECK-objc-4: [[@LINE-2]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  let _: Int = strlcat
  // CHECK-objc-3: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> UInt'{{( [(]aka .+[)])?}} to specified type 'Int'
  // CHECK-objc-4: [[@LINE-2]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'

  // wcslen is different: it wasn't a builtin until Swift 4, and so its return
  // type has always been 'Int'.
  let _: Int = wcslen
  // CHECK-objc-3: [[@LINE-1]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'
  // CHECK-objc-4: [[@LINE-2]]:16: error: cannot convert value of type '({{.+}}) -> Int'{{( [(]aka .+[)])?}} to specified type 'Int'
}
