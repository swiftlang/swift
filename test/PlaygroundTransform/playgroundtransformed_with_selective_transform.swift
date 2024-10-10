// Tests the `-playground-option SelectiveTransform` flag and
// the `@_PlaygroundTransformed` function annotation.
//
// REQUIRES: executable_test
//
// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
//
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -playground-option -Xfrontend SelectiveTransform -Xfrontend -debugger-support -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
//
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-option -Xfrontend SelectiveTransform -Xfrontend -debugger-support -o %t/main2 -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s

import PlaygroundSupport

// This function won't be transformed and so won't generate any logging
func g(w: Int, h: Int) -> Int {
    return w * h
}
let goo = g(w: 10, h: 20)

// Only this function will be transformed and generate logging
@_PlaygroundTransformed
func f(x: Int, _ y: Float = 7.62) -> Int {
    return x + Int(y)
}
let foo = f(x: 42)

// CHECK:      {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[x='42']
// CHECK-NEXT: {{.*}} __builtin_log[y='7.62']
// CHECK-NEXT: {{.*}} __builtin_log[='49']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

struct S {
  // Non-annotated method
  func func0(i0: Int) -> Int {
    return i0 + 0
  }

  // Annotated method
  @_PlaygroundTransformed
  func func1(i1: Int) -> Int {
    return i1 + 1
  }

  // Annotated static method
  @_PlaygroundTransformed
  static func func2(i2: Int) -> Int {
    return i2 + 2
  }
}
let s = S()
s.func0(i0: 0)

s.func1(i1: 1)
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[i1='1']
// CHECK-NEXT: {{.*}} __builtin_log[='2']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

S.func2(i2: 2)
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[i2='2']
// CHECK-NEXT: {{.*}} __builtin_log[='4']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

class C {
  // Non-annotated method
  func func0(i0: Int) -> Int {
    return i0 + 0
  }

  // Annotated method
  @_PlaygroundTransformed
  func func1(i1: Int) -> Int {
    return i1 + 1
  }

  // Annotated static method
  @_PlaygroundTransformed
  static func func2(i2: Int) -> Int {
    return i2 + 2
  }

  // Annotated class method
  @_PlaygroundTransformed
  static func func3(i3: Int) -> Int {
    return i3 + 3
  }
}

let c = C()
c.func0(i0: 0)

c.func1(i1: 1)
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[i1='1']
// CHECK-NEXT: {{.*}} __builtin_log[='2']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

C.func2(i2: 2)
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[i2='2']
// CHECK-NEXT: {{.*}} __builtin_log[='4']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

C.func3(i3: 3)
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[i3='3']
// CHECK-NEXT: {{.*}} __builtin_log[='6']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

extension C {
  // Annotated func on extension
  @_PlaygroundTransformed
  func func4(i4: Int) -> Int {
    return i4 + 4
  }
}

c.func4(i4: 4)
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[i4='4']
// CHECK-NEXT: {{.*}} __builtin_log[='8']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

func quadrupled(x: Int) -> Int {
    @_PlaygroundTransformed
    func double(xx: Int) -> Int {
        return xx * 2
    }

    return double(xx: double(xx: x))
}
quadrupled(x: 4)
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[xx='4']
// CHECK-NEXT: {{.*}} __builtin_log[='8']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[xx='8']
// CHECK-NEXT: {{.*}} __builtin_log[='16']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

@_PlaygroundTransformed
func tripled(t: Int) -> Int {
    func _triple(tt: Int) -> Int {
        return tt * 3
    }

    return _triple(tt: t)
}
tripled(t: 4)
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[t='4']
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[tt='4']
// CHECK-NEXT: {{.*}} __builtin_log[='12']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[='12']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit

// @_PlaygroundTransformed can be used on init/deinit
// and get/set/didSet for vars.
class D {
    var _negative: Int {
        @_PlaygroundTransformed
        didSet {
            let actualValue = -_negative
            print(actualValue)
        }
    }
    var foo: Int {
        @_PlaygroundTransformed
        get {
            let getValue = -_negative
            return getValue
        }
        @_PlaygroundTransformed
        set {
            let setValue = -newValue
            _negative = setValue
        }
    }
    @_PlaygroundTransformed
    init() {
        let init_value = 0
        _negative = 0
    }
    @_PlaygroundTransformed
    deinit {
        let deinit_value = 0
        _negative = deinit_value
    }
}
var d: D? = D()
d!.foo = 42
d!.foo
d = nil
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[init_value='0']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[newValue='42']
// CHECK-NEXT: {{.*}} __builtin_log[setValue='-42']
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[actualValue='42']
// CHECK-NEXT: 42
// CHECK-NEXT: {{.*}} __builtin_postPrint
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log[self='main{{.?}}.D']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[getValue='42']
// CHECK-NEXT: {{.*}} __builtin_log[='42']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
// CHECK-NEXT: {{.*}} __builtin_log_scope_entry
// CHECK-NEXT: {{.*}} __builtin_log[deinit_value='0']
// CHECK-NEXT: {{.*}} __builtin_log[self='main{{.?}}.D']
// CHECK-NEXT: {{.*}} __builtin_log_scope_exit
