#if os(iOS)
class a {
	  func b () {
let i = 3
    }
}
#else
class a {
	  func b () {
let i = 3
    }
}
#endif
#if os(iOS)
class a {
func b () {
let i = 3
    }
}
#elseif os(OSX)
class a {
func b () {
let i = 3
    }
}
#endif
print(false)

// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s -- -target x86_64-apple-macosx10.9 > %t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s -- -target x86_64-apple-macosx10.9 > %t.response
// RUN: %sourcekitd-test -req=format -line=10 -length=1 %s -- -target x86_64-apple-macosx10.9 >> %t.response
// RUN: %sourcekitd-test -req=format -line=16 -length=1 %s -- -target x86_64-apple-macosx10.9 >> %t.response
// RUN: %sourcekitd-test -req=format -line=20 -length=1 %s -- -target x86_64-apple-macosx10.9 >> %t.response
// RUN: %sourcekitd-test -req=format -line=22 -length=1 %s -- -target x86_64-apple-macosx10.9 >> %t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "#else"
// CHECK: key.sourcetext: "        let i = 3"
// CHECK: key.sourcetext: "    func b () {"
// CHECK: key.sourcetext: "#elseif os(OSX)"
// CHECK: key.sourcetext: "    func b () {"
