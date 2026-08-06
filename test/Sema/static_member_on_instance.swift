//// RUN: %target-typecheck-verify-swift

// =====================================================
// Test 1: Basic instance access
// =====================================================
struct S1 {
    static let v = 1
}
func t1(_ s: S1) {
    _ = s.v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S1', not on the instance s}}
}

// =====================================================
// Test 2: Direct inside same type using self
// =====================================================
struct S2 {
    static let v = 1
    func f() {
        _ = self.v
        // expected-error@-1 {{static member 'v' can only be used on the type 'S2', not on the instance self}}
    }
}

// =====================================================
// Test 3: Array indexing
// =====================================================
struct S3 { static let v = 1 }
func t3(arr: [S3], i: Int) {
    _ = arr[i].v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S3', not on the instance arr[i]}}
}

// =====================================================
// Test 4: Tuple extraction
// =====================================================
struct S4 { static let v = 1 }
func t4(_ s1: S4, _ s2: S4) {
    let tup = (s1, s2)
    _ = tup.0.v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S4', not on the instance tup.0}}
}

// =====================================================
// Test 5: Parenthesized instance (((s)))
// =====================================================
struct S5 { static let v = 1 }
func t5(_ s: S5) {
    _ = (((s))).v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S5', not on the instance s}}
}

// =====================================================
// Test 6: Enum case diagnostic (baseline, unchanged)
// =====================================================
enum E6 { case a }
func t6(_ e: E6) {
    _ = e.a
    // expected-error@-1 {{enum case 'a' cannot be used as an instance member}}
    // (This tests we did not break the unrelated enum case diagnostic.)
}

// =====================================================
// Test 7: Constructor call as base (S()).v
// =====================================================
struct S7 { static let v = 1 }
func t7() {
    _ = (S7()).v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S7', not on the instance S7()}}
}

// =====================================================
// Test 8: Ternary conditional base
// =====================================================
struct S8 { static let v = 1 }
func t8(_ cond: Bool, s1: S8, s2: S8) {
    _ = (cond ? s1 : s2).v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S8', not on the instance cond ? s1 : s2}}
}

// =====================================================
// Test 9: Optional chaining s?.v
// =====================================================
struct S9 { static let v = 1 }
struct C9 { var s: S9? }
func t9(_ c: C9) {
    _ = c.s?.v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S9', not on the instance c.s?}}
}

// =====================================================
// Test 10: Forced unwrap s!.v
// =====================================================
struct S10 { static let v = 1 }
func t10(_ s: S10?) {
    _ = s!.v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S10', not on the instance s!}}
}

// =====================================================
// Test 11: Valid static access
// =====================================================
struct S11 { static let v = 1 }
func t11_ok() {
    _ = S11.v   // OK
}

// =====================================================
// Test 12: Shadowed type name (let S = S())
// =====================================================
struct S12 { static let v = 1 }
func t12() {
    let S = S12()
    _ = S.v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S12', not on the instance S}}
}

// =====================================================
// Test 13: Nested struct access b.a.v
// =====================================================
struct A13 { static let v = 1 }
struct B13 { let a: A13 }
func t13(_ b: B13) {
    _ = b.a.v
    // expected-error@-1 {{static member 'v' can only be used on the type 'A13', not on the instance b.a}}
}

// =====================================================
// Test 14: Optional member chain c.s?.v
// =====================================================
struct S14 { static let v = 1 }
struct C14 { var s: S14? }
func t14(_ c: C14) {
    _ = c.s?.v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S14', not on the instance c.s?}}
}

// =====================================================
// Test 15: S().v (again) — duplicate of constructor path
// =====================================================
struct S15 { static let v = 1 }
func t15() {
    _ = (S15()).v
    // expected-error@-1 {{static member 'v' can only be used on the type 'S15', not on the instance S15()}}
}

