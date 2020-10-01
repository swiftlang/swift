do {
    print("hello")
} catch MyErr.a(let code),
        MyErr.b(let code)

// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s | %FileCheck --strict-whitespace %s
// CHECK: key.sourcetext: ""
