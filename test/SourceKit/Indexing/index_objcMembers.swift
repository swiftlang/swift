// RUN: %sourcekitd-test -req=index %s -- -Xfrontend -serialize-diagnostics-path -Xfrontend %t.dia %s -Xfrontend -disable-implicit-concurrency-module-import | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response
// REQUIRES: objc_interop

@objcMembers class FixtureClass25: NSObject {
    var someVar: String?
    func someMethod() {}
}
