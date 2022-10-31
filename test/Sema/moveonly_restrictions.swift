// RUN: %target-typecheck-verify-swift -enable-experimental-move-only

class CopyableKlass {}
@_moveOnly
class MoveOnlyKlass {}

class C {
    var copyable: CopyableKlass? = nil
    var moveOnly: MoveOnlyKlass? = nil
}

@_moveOnly
class CMoveOnly {
    var copyable: CopyableKlass? = nil
    var moveOnly: MoveOnlyKlass? = nil
}

struct OptionalGrandField<T> { // expected-error {{generic struct 'OptionalGrandField' cannot contain a move-only type without also being move-only}}
    var moveOnly3: T?
    var moveOnly2: MoveOnlyKlass // expected-note {{contained move-only property 'OptionalGrandField.moveOnly2'}}
}

struct S0 {
    var moveOnly3: OptionalGrandField<MoveOnlyKlass>
}

struct SCopyable {
    var copyable: CopyableKlass
}

struct S { // expected-error {{struct 'S' cannot contain a move-only type without also being move-only}}
    var copyable: CopyableKlass
    var moveOnly2: MoveOnlyKlass?
    var moveOnly: MoveOnlyKlass // expected-note {{contained move-only property 'S.moveOnly'}}
    var moveOnly3: OptionalGrandField<MoveOnlyKlass>
}

@_moveOnly
struct SMoveOnly {
    var copyable: CopyableKlass
    var moveOnly: MoveOnlyKlass
}

enum E { // expected-error {{enum 'E' cannot contain a move-only type without also being move-only}}
    case lhs(CopyableKlass)
    case rhs(MoveOnlyKlass) // expected-note {{contained move-only enum case 'E.rhs'}}
    case rhs2(OptionalGrandField<MoveOnlyKlass>)
}

@_moveOnly
enum EMoveOnly {
    case lhs(CopyableKlass)
    case rhs(MoveOnlyKlass)
}

func foo() {
    class C2 {
        var copyable: CopyableKlass? = nil
        var moveOnly: MoveOnlyKlass? = nil
    }

    @_moveOnly
    class C2MoveOnly {
        var copyable: CopyableKlass? = nil
        var moveOnly: MoveOnlyKlass? = nil
    }

    struct S2 { // expected-error {{struct 'S2' cannot contain a move-only type without also being move-only}}
        var copyable: CopyableKlass
        var moveOnly: MoveOnlyKlass // expected-note {{contained move-only property 'S2.moveOnly'}}
    }

    @_moveOnly
    struct S2MoveOnly {
        var copyable: CopyableKlass
        var moveOnly: MoveOnlyKlass
    }

    enum E2 { // expected-error {{enum 'E2' cannot contain a move-only type without also being move-only}}
        case lhs(CopyableKlass)
        case rhs(MoveOnlyKlass) // expected-note {{contained move-only enum case 'E2.rhs'}}
    }

    @_moveOnly
    enum E2MoveOnly {
        case lhs(CopyableKlass)
        case rhs(MoveOnlyKlass)
    }
    {
        class C3 {
            var copyable: CopyableKlass? = nil
            var moveOnly: MoveOnlyKlass? = nil
        }

        @_moveOnly
        class C3MoveOnly {
            var copyable: CopyableKlass? = nil
            var moveOnly: MoveOnlyKlass? = nil
        }

        struct S3 { // expected-error {{struct 'S3' cannot contain a move-only type without also being move-only}}
            var copyable: CopyableKlass
            var moveOnly: MoveOnlyKlass // expected-note {{contained move-only property 'S3.moveOnly'}}
        }

        @_moveOnly
        struct S3MoveOnly {
            var copyable: CopyableKlass
            var moveOnly: MoveOnlyKlass
        }

        enum E3 { // expected-error {{enum 'E3' cannot contain a move-only type without also being move-only}}
            case lhs(CopyableKlass)
            case rhs(MoveOnlyKlass) // expected-note {{contained move-only enum case 'E3.rhs'}}
        }

        @_moveOnly
        enum E3MoveOnly {
            case lhs(CopyableKlass)
            case rhs(MoveOnlyKlass)
        }
        print("1")
        print("2")
    }()
}

// Make sure we do not crash on this.
struct UnsafePointerWithOwner<T> {
    var owner: AnyObject? = nil
    var data: UnsafePointer<T>? = nil

    func doNothing() {}
}

