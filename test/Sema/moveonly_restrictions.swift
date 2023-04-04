// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature MoveOnlyClasses

// REQUIRES: concurrency

class CopyableKlass {}

class MoveOnlyKlass: ~Copyable {
    init?() {} // expected-error {{move-only types cannot have failable initializers yet}}
}

class MoveOnlyStruct: ~Copyable {
    init?(one: Bool) {} // expected-error {{move-only types cannot have failable initializers yet}}
    init!(two: Bool) {} // expected-error {{move-only types cannot have failable initializers yet}}
}

class C {
    var copyable: CopyableKlass? = nil
    var moveOnlyC: MoveOnlyKlass? = nil // expected-error {{move-only type 'MoveOnlyKlass' cannot be used with generics yet}}
    var moveOnlyS: MoveOnlyStruct? = nil // expected-error {{move-only type 'MoveOnlyStruct' cannot be used with generics yet}}
}

class CMoveOnly: ~Copyable {
    var copyable: CopyableKlass? = nil
    var moveOnlyC: MoveOnlyKlass? = nil // expected-error {{move-only type 'MoveOnlyKlass' cannot be used with generics yet}}
    var moveOnlyS: MoveOnlyStruct? = nil // expected-error {{move-only type 'MoveOnlyStruct' cannot be used with generics yet}}
}

struct OptionalGrandField<T> { // expected-error {{generic struct 'OptionalGrandField' cannot contain a move-only type without also being move-only}}
    var moveOnly3: T?
    var moveOnly2: MoveOnlyKlass // expected-note {{contained move-only property 'OptionalGrandField.moveOnly2'}}
}

struct S0 {
    var moveOnly3: OptionalGrandField<MoveOnlyKlass> // expected-error {{move-only type 'MoveOnlyKlass' cannot be used with generics yet}}
    var moveOnly4: OptionalGrandField<MoveOnlyStruct> // expected-error {{move-only type 'MoveOnlyStruct' cannot be used with generics yet}}
}

struct SCopyable {
    var copyable: CopyableKlass
}

struct S { // expected-error {{struct 'S' cannot contain a move-only type without also being move-only}}
    var copyable: CopyableKlass
    var moveOnly2: MoveOnlyStruct? // expected-error {{move-only type 'MoveOnlyStruct' cannot be used with generics yet}}
    var moveOnly: MoveOnlyStruct // expected-note {{contained move-only property 'S.moveOnly'}}
    var moveOnly3: OptionalGrandField<MoveOnlyKlass> // expected-error {{move-only type 'MoveOnlyKlass' cannot be used with generics yet}}
    var moveOnly4: OptionalGrandField<MoveOnlyStruct> // expected-error {{move-only type 'MoveOnlyStruct' cannot be used with generics yet}}
}

struct SMoveOnly: ~Copyable {
    var copyable: CopyableKlass
    var moveOnly: MoveOnlyKlass
}

enum E { // expected-error {{enum 'E' cannot contain a move-only type without also being move-only}}
    case lhs(CopyableKlass)
    case rhs(MoveOnlyKlass) // expected-note {{contained move-only enum case 'E.rhs'}}
    case rhs2(OptionalGrandField<MoveOnlyKlass>) // expected-error {{move-only type 'MoveOnlyKlass' cannot be used with generics yet}}
}

enum EMoveOnly: ~Copyable {
    case lhs(CopyableKlass)
    case rhs(MoveOnlyKlass)

    init?() {} // expected-error {{move-only types cannot have failable initializers yet}}
}

extension EMoveOnly {
    init!(three: Bool) {} // expected-error {{move-only types cannot have failable initializers yet}}
}

extension MoveOnlyStruct {
    convenience init?(three: Bool) {} // expected-error {{move-only types cannot have failable initializers yet}}
}

func foo() {
    class C2 {
        var copyable: CopyableKlass? = nil
        var moveOnly: MoveOnlyKlass? = nil // expected-error {{move-only type 'MoveOnlyKlass' cannot be used with generics yet}}
    }

    class C2MoveOnly: ~Copyable {
        var copyable: CopyableKlass? = nil
        var moveOnly: MoveOnlyKlass? = nil // expected-error {{move-only type 'MoveOnlyKlass' cannot be used with generics yet}}
    }

    struct S2 { // expected-error {{struct 'S2' cannot contain a move-only type without also being move-only}}
        var copyable: CopyableKlass
        var moveOnly: MoveOnlyKlass // expected-note {{contained move-only property 'S2.moveOnly'}}
    }

    struct S2MoveOnly: ~Copyable {
        var copyable: CopyableKlass
        var moveOnly: MoveOnlyKlass
    }

    enum E2 { // expected-error {{enum 'E2' cannot contain a move-only type without also being move-only}}
        case lhs(CopyableKlass)
        case rhs(MoveOnlyKlass) // expected-note {{contained move-only enum case 'E2.rhs'}}
    }

    enum E2MoveOnly: ~Copyable {
        case lhs(CopyableKlass)
        case rhs(MoveOnlyKlass)
    }
    {
        class C3 {
            var copyable: CopyableKlass? = nil
            var moveOnly: MoveOnlyKlass? = nil // expected-error {{move-only type 'MoveOnlyKlass' cannot be used with generics yet}}
        }

        class C3MoveOnly: ~Copyable {
            var copyable: CopyableKlass? = nil
            var moveOnly: MoveOnlyKlass? = nil // expected-error {{move-only type 'MoveOnlyKlass' cannot be used with generics yet}}
        }

        struct S3 { // expected-error {{struct 'S3' cannot contain a move-only type without also being move-only}}
            var copyable: CopyableKlass
            var moveOnly: MoveOnlyKlass // expected-note {{contained move-only property 'S3.moveOnly'}}
        }

        struct S3MoveOnly: ~Copyable {
            var copyable: CopyableKlass
            var moveOnly: MoveOnlyKlass
        }

        enum E3 { // expected-error {{enum 'E3' cannot contain a move-only type without also being move-only}}
            case lhs(CopyableKlass)
            case rhs(MoveOnlyKlass) // expected-note {{contained move-only enum case 'E3.rhs'}}
        }

        enum E3MoveOnly: ~Copyable {
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

// Make sure we error whenever we attempt to conform a move only type to a
// protocol.
protocol P {}
protocol Q {}
class ProtocolCheckMoveOnlyKlass: ~Copyable {}
struct ProtocolCheckMoveOnlyStruct: ~Copyable {
    var k: MoveOnlyKlass
}
enum ProtocolCheckMoveOnlyEnum: ~Copyable {}

extension ProtocolCheckMoveOnlyKlass : P {} // expected-error {{move-only class 'ProtocolCheckMoveOnlyKlass' cannot conform to 'P'}}
extension ProtocolCheckMoveOnlyStruct : P, Q {}
// expected-error@-1 {{move-only struct 'ProtocolCheckMoveOnlyStruct' cannot conform to 'P'}}
// expected-error@-2 {{move-only struct 'ProtocolCheckMoveOnlyStruct' cannot conform to 'Q'}}
// expected-note@-3 {{'ProtocolCheckMoveOnlyStruct' declares conformance to protocol 'P' here}}

extension ProtocolCheckMoveOnlyStruct: P {}
// expected-error@-1 {{redundant conformance of 'ProtocolCheckMoveOnlyStruct' to protocol 'P'}}
// expected-error@-2 {{move-only struct 'ProtocolCheckMoveOnlyStruct' cannot conform to 'P'}}

extension ProtocolCheckMoveOnlyEnum : P & Q, Sendable {}
// expected-error@-1 {{move-only enum 'ProtocolCheckMoveOnlyEnum' cannot conform to 'P & Q'}}

extension ProtocolCheckMoveOnlyEnum : Any {}
// expected-error@-1 {{move-only enum 'ProtocolCheckMoveOnlyEnum' cannot conform to 'Any'}}

extension ProtocolCheckMoveOnlyEnum : AnyHashable {}
// expected-error@-1 {{move-only enum 'ProtocolCheckMoveOnlyEnum' cannot conform to 'AnyHashable'}}
// expected-error@-2 {{inheritance from non-protocol type 'AnyHashable'}}

// But a normal extension is ok.
extension ProtocolCheckMoveOnlyKlass {}
extension ProtocolCheckMoveOnlyStruct {}
extension ProtocolCheckMoveOnlyEnum {}

// Check if we define a move only type and make it conform on the base type
class MoveOnlyKlassP : P, ~Copyable {} // expected-error {{move-only class 'MoveOnlyKlassP' cannot conform to 'P'}}
struct MoveOnlyStructP : ~Copyable, P { // expected-error {{move-only struct 'MoveOnlyStructP' cannot conform to 'P'}}
    var mv: MoveOnlyKlass
}
enum MoveOnlyEnumP : ~Copyable, P {} // expected-error {{move-only enum 'MoveOnlyEnumP' cannot conform to 'P'}}

// ensure there is no auto-synthesis of Equatable, Hashable, etc, for this move-only enum,
// because it normally would be synthesized since it only has cases without associated values.
enum Color: ~Copyable {
    case red
    case green
    case blue

    static func same(_ c1: borrowing Color, c2: borrowing Color) -> Bool {
        return c1 == c2
        // expected-error@-1 {{binary operator '==' cannot be applied to two 'Color' operands}}
    }
}

enum StrengthLevel: Int, ~Copyable { // ensure move-only raw enums do not conform to RawRepresentable
    case none = 0
    case low
    case high

    static func lowEnergy() {
        _ = StrengthLevel(rawValue: 1)
        // expected-error@-1 {{'StrengthLevel' cannot be constructed because it has no accessible initializers}}
    }
}

public class Holder {
    var one: MoveOnlyStruct {
        get async {  } // expected-error {{getter of move-only type cannot be 'async' or 'throws'}}
    }
    var two: MoveOnlyKlass {
        get throws {  } // expected-error {{getter of move-only type cannot be 'async' or 'throws'}}
    }
}

struct StructHolder {
    var three: EMoveOnly {
        get async throws {  } // expected-error {{getter of move-only type cannot be 'async' or 'throws'}}
    }
}

protocol DanceHall: ~Copyable {} // expected-error {{cannot suppress conformances here}}

// FIXME: why does adding this cause warnings about @_moveOnly being deprecated?
// <unknown>:0: error: unexpected warning produced: '@_moveOnly' attribute is deprecated; use the ~Copyable constraint suppression instead; this is an error in Swift 6
//<unknown>:0: warning: diagnostic produced elsewhere: '@_moveOnly' attribute is deprecated; use the ~Copyable constraint suppression instead; this is an error in Swift 6
actor Speedcore: ~Copyable {}



