// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen -parse-stdlib -module-name Swift %s | %FileCheck %s

@_marker protocol Copyable {}
@_marker protocol Escapable {}

enum Optional<Wrapped: ~Copyable>: ~Copyable {
    case none
    case some(Wrapped)
}

extension Optional: Copyable where Wrapped: Copyable { }

func _diagnoseUnexpectedNilOptional(_filenameStart: Builtin.RawPointer,
                                    _filenameLength: Builtin.Word,
                                    _filenameIsASCII: Builtin.Int1,
                                    _line: Builtin.Word,
                                    _isImplicitUnwrap: Builtin.Int1) {
}

precedencegroup AssignmentPrecedence {}

struct NC: ~Copyable {
    borrowing func b() {}
    mutating func m() {}
    consuming func c() {}

    consuming func c2() -> NC { c2() } // expected-warning{{}}
    consuming func c3() -> NCAO { c3() } // expected-warning{{}}
}

struct NCAO: ~Copyable {
    var x: Any

    borrowing func b() {}
    mutating func m() {}
    consuming func c() {}

    consuming func c2() -> NC { c2() } // expected-warning{{}}
    consuming func c3() -> NCAO { c3() } // expected-warning{{}}
}

struct NCPair: ~Copyable {
    var first: NC? = .none
    var second: NCAO? = .none
}

func consumingSwitchSubject(nc: consuming NC?,
                            ncao: consuming NCAO?) {
    switch nc?.c2() {
    default:
      break
    }
    switch ncao?.c2() {
    default:
      break
    }
}

func consumingSwitchSubject2(nc: consuming NC?,
                             ncao: consuming NCAO?) {
    switch nc?.c3() {
    default:
      break
    }
    switch ncao?.c3() {
    default:
      break
    }
}

// CHECK-LABEL: sil {{.*}}@${{.*}}23consumingSwitchSubject3
func consumingSwitchSubject3(ncp: consuming NCPair) {
    // CHECK:   [[PAIR_ACCESS:%.*]] = begin_access [deinit] [unknown] {{.*}} : $*NCPair
    // CHECK:   [[PAIR_MARK:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable] [[PAIR_ACCESS]]
    // CHECK:   [[FIELD:%.*]] = struct_element_addr [[PAIR_MARK]]
    // CHECK:   [[FIELD_ACCESS:%.*]] = begin_access [deinit] [static] [no_nested_conflict] [[FIELD]]
    // CHECK:   cond_br {{.*}}, [[SOME_BB:bb[0-9]+]], [[NONE_BB:bb[0-9]+]]
    // CHECK: [[SOME_BB]]:
    // CHECK:   [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr [[FIELD_ACCESS]]
    // CHECK:   [[PAYLOAD:%.*]] = load [take] [[PAYLOAD_ADDR]]
    // CHECK:   apply {{.*}}({{.*}}, [[PAYLOAD]]) : ${{.*}} (@owned NC)
    // CHECK-NOT: destroy_addr
    // CHECK-NOT: destroy_value
    // CHECK:   end_access [[FIELD_ACCESS]]
    // CHECK-NEXT: end_access [[PAIR_ACCESS]]
    // CHECK-NEXT: inject_enum_addr 
    // CHECK-NEXT: br 
    // CHECK: [[NONE_BB]]:
    // CHECK:   destroy_addr [[FIELD_ACCESS]]
    // CHECK-NEXT: end_access [[FIELD_ACCESS]]

    switch ncp.first?.c3() {
    default:
      break
    }
    switch ncp.second?.c3() {
    default:
      break
    }
}
