// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-silgen %s -module-name test \
// RUN:   > %t/test.silgen

// RUN: %FileCheck %s < %t/test.silgen
// RUN: %swift-demangle < %t/test.silgen | %FileCheck %s --check-prefix=DEMANGLED


struct Basic<T: ~Copyable> {
    // DEMANGLED: (extension in test):test.Basic< where A: ~Swift.Copyable>.cali2() -> ()
    // CHECK: $s4test5BasicVAARi_zrlE5cali2yyF
    func cali2() {}
}

extension Basic where T: ~Copyable {
  // DEMANGLED: (extension in test):test.Basic< where A: ~Swift.Copyable>.cali1() -> ()
  // CHECK: $s4test5BasicVAARi_zrlE5cali1yyF
  func cali1() {}
}

extension Basic where T: ~Copyable {
  // Despite being in a noncopyable T extension,
  // 'texas2' adds a Copyable requirement, so it will
  // get mangled as if it were not in the extension.

  // DEMANGLED: test.Basic.texas2() -> ()
  // CHECK: $s4test5BasicV6texas2yyF
  func texas2() where T: Copyable {}
}

extension Basic {
  // DEMANGLED: test.Basic.texas1() -> ()
  // CHECK: @$s4test5BasicV6texas1yyF
  func texas1() {}
}

struct _G_ {}

extension _G_ {
  struct _H_<T: ~Copyable> {
    struct _I_ {
      // DEMANGLED: test._G_._H_._I_.foo() -> ()
      // CHECK: $s4test3_G_V3_H_V3_I_V3fooyyF
      func foo() where T: Copyable {}

      // DEMANGLED: test._G_._H_._I_.foo< where A: ~Swift.Copyable>() -> ()
      // CHECK: $s4test3_G_V3_H_V3_I_V3fooyyRi_zrlF
      func foo() {}
    }
  }
}

extension _G_._H_._I_ {
  // DEMANGLED: test._G_._H_._I_.bar() -> ()
  // CHECK: $s4test3_G_V3_H_V3_I_V3baryyF
  func bar() {}
}

struct Gloop<T: ~Copyable> {}

extension Gloop where T: ~Copyable {
  struct Crash<B: ~Escapable> {
    // DEMANGLED: (extension in test):test.Gloop< where A: ~Swift.Copyable>.Crash.foo< where A1: ~Swift.Escapable>() -> ()
    // CHECK: $s4test5GloopVAARi_zrlE5CrashV3fooyyRi0_d__rlF
    func foo() {}

    // DEMANGLED: test.Gloop.Crash.foo< where A1: ~Swift.Escapable>() -> ()
    // CHECK: $s4test5GloopV5CrashV3fooyyRi0_d__rlF
    func foo() where T: Copyable {}

    // DEMANGLED: (extension in test):test.Gloop< where A: ~Swift.Copyable>.Crash.foo() -> ()
    // CHECK: $s4test5GloopVAARi_zrlE5CrashV3fooyyF
    func foo() where B: Escapable {}

    // DEMANGLED: test.Gloop.Crash.foo() -> ()
    // CHECK: $s4test5GloopV5CrashV3fooyyF
    func foo() where T: Copyable, B: Escapable {}
  }
}

struct X<A: ~Copyable> {
  struct Y<B: ~Copyable> {

    // DEMANGLED: test.X.Y.g() -> ()
    // CHECK: $s4test1XV1YV1gyyF
    func g() where A: Copyable, B: Copyable {}

    // DEMANGLED: (extension in test):test.X.Y< where A1: ~Swift.Copyable>.g() -> ()
    // CHECK: $s4test1XV1YVAARi_d__rlE1gyyF
    func g() where A: Copyable {}

    // DEMANGLED: (extension in test):test.X.Y< where A: ~Swift.Copyable>.g() -> ()
    // CHECK: $s4test1XV1YVAARi_zrlE1gyyF
    func g() where B: Copyable {}

    // DEMANGLED: (extension in test):test.X.Y< where A: ~Swift.Copyable, A1: ~Swift.Copyable>.g() -> ()
    // CHECK: $s4test1XV1YVAARi_zRi_d__rlE1gyyF
    func g() {}
  }
}

struct OverloadComputed<T: ~Copyable> {
  // DEMANGLED: extension in test):test.OverloadComputed< where A: ~Swift.Copyable>.obtain() -> A
  // CHECK: $s4test16OverloadComputedVAARi_zrlE6obtainxyF
  func obtain() -> T { fatalError() }
}
extension OverloadComputed {
  // DEMANGLED: test.OverloadComputed.prop.getter : A
  // CHECK: $s4test16OverloadComputedV4propxvg
  var prop: T { return obtain() }
}
extension OverloadComputed where T: ~Copyable {
  // DEMANGLED: (extension in test):test.OverloadComputed< where A: ~Swift.Copyable>.prop.getter : A
  // CHECK: $s4test16OverloadComputedVAARi_zrlE4propxvg
  var prop: T { return obtain() }
}
