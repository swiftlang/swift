// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature MoveOnlyClasses

// REQUIRES: concurrency
// REQUIRES: swift_feature_MoveOnlyClasses

class CopyableKlass {}

class MoveOnlyKlass: ~Copyable { // expected-note 6{{class 'MoveOnlyKlass' has '~Copyable' constraint preventing 'Copyable' conformance}}
    init?() {}
}

struct MoveOnlyStruct: ~Copyable { // expected-note {{struct 'MoveOnlyStruct' has '~Copyable' constraint preventing 'Copyable' conformance}}
    init?(one: Bool) {}
    init!(two: Bool) {}
}

class C {
    var copyable: CopyableKlass? = nil
    var moveOnlyC: MoveOnlyKlass? = nil
    var moveOnlyS: MoveOnlyStruct? = nil
    var copyableCs: [CopyableKlass] = []
    var moveOnlyCs: [MoveOnlyKlass] = [] // expected-error {{type 'MoveOnlyKlass' does not conform to protocol 'Copyable'}}
    var moveOnlySs: [MoveOnlyStruct] = [] // expected-error {{type 'MoveOnlyStruct' does not conform to protocol 'Copyable'}}
}

class CMoveOnly: ~Copyable {
    var copyable: CopyableKlass? = nil
    var moveOnlyC: MoveOnlyKlass? = nil
    var moveOnlyS: MoveOnlyStruct? = nil
    var copyableCs: [CopyableKlass] = []
    var moveOnlyCs: [MoveOnlyKlass] = [] // expected-error {{type 'MoveOnlyKlass' does not conform to protocol 'Copyable'}}
    var moveOnlySs: [MoveOnlyStruct] = [] // expected-error {{type 'MoveOnlyStruct' does not conform to protocol 'Copyable'}}
}

struct OptionalGrandField<T> { // expected-note {{consider adding '~Copyable' to generic struct 'OptionalGrandField'}}
    var moveOnly3: T?
    var moveOnly2: MoveOnlyKlass // expected-error {{stored property 'moveOnly2' of 'Copyable'-conforming generic struct 'OptionalGrandField' has non-Copyable type 'MoveOnlyKlass'}}
}

struct S0 {
    var moveOnly3: OptionalGrandField<MoveOnlyKlass> // expected-error {{type 'MoveOnlyKlass' does not conform to protocol 'Copyable'}}
    var moveOnly4: OptionalGrandField<MoveOnlyStruct> // expected-error {{type 'MoveOnlyStruct' does not conform to protocol 'Copyable'}}
}

struct SCopyable {
    var copyable: CopyableKlass
}

struct S { // expected-note {{consider adding '~Copyable' to struct 'S'}}
    var copyable: CopyableKlass
    var moveOnly2: MoveOnlyStruct? // expected-error {{stored property 'moveOnly2' of 'Copyable'-conforming struct 'S' has non-Copyable type 'MoveOnlyStruct?'}}
    // FIXME(NCG): Shouldn't this be also an error?
    var moveOnly: MoveOnlyStruct
    var moveOnly3: OptionalGrandField<MoveOnlyKlass> // expected-error {{type 'MoveOnlyKlass' does not conform to protocol 'Copyable'}}
    var moveOnly3: OptionalGrandField<MoveOnlyStruct> // expected-error {{type 'MoveOnlyStruct' does not conform to protocol 'Copyable'}}
}

struct S2 { // expected-note {{consider adding '~Copyable' to struct 'S2'}}
    var moveOnly: MoveOnlyStruct // expected-error {{stored property 'moveOnly' of 'Copyable'-conforming struct 'S2' has non-Copyable type 'MoveOnlyStruct'}}
}

struct SMoveOnly: ~Copyable {
    var copyable: CopyableKlass
    var moveOnly: MoveOnlyKlass
}

enum E { // expected-note {{consider adding '~Copyable' to enum 'E'}}
    case lhs(CopyableKlass)
    case rhs(MoveOnlyKlass) // expected-error {{associated value 'rhs' of 'Copyable'-conforming enum 'E' has non-Copyable type 'MoveOnlyKlass'}}
    case rhs2(OptionalGrandField<MoveOnlyKlass>) // expected-error {{type 'MoveOnlyKlass' does not conform to protocol 'Copyable'}}
}

enum EMoveOnly: ~Copyable {
    case lhs(CopyableKlass)
    case rhs(MoveOnlyKlass)

    init?() {}
}

extension EMoveOnly {
    init!(three: Bool) {}
}

extension MoveOnlyKlass {
    convenience init?(three: Bool) {}
}

extension MoveOnlyStruct {
    init?(three: Bool) {}
}

func foo() {
    class C2 {
        var copyable: CopyableKlass? = nil
        var moveOnly: MoveOnlyKlass? = nil
        var copyables: [CopyableKlass] = []
        var moveOnlies: [MoveOnlyKlass] = [] // expected-error {{type 'MoveOnlyKlass' does not conform to protocol 'Copyable'}}
    }

class C2MoveOnly: ~Copyable {
        var copyable: CopyableKlass? = nil
        var moveOnly: MoveOnlyKlass? = nil
        var copyables: [CopyableKlass] = []
        var moveOnlies: [MoveOnlyKlass] = [] // expected-error {{type 'MoveOnlyKlass' does not conform to protocol 'Copyable'}}
    }

    struct S2 { // expected-note {{consider adding '~Copyable' to struct 'S2'}}
        var copyable: CopyableKlass
        var moveOnly: MoveOnlyKlass // expected-error {{stored property 'moveOnly' of 'Copyable'-conforming struct 'S2' has non-Copyable type 'MoveOnlyKlass'}}
    }

struct S2MoveOnly: ~Copyable {
        var copyable: CopyableKlass
        var moveOnly: MoveOnlyKlass
    }

    enum E2 { // expected-note {{consider adding '~Copyable' to enum 'E2'}}
        case lhs(CopyableKlass)
        case rhs(MoveOnlyKlass) // expected-error {{associated value 'rhs' of 'Copyable'-conforming enum 'E2' has non-Copyable type 'MoveOnlyKlass'}}
    }

enum E2MoveOnly: ~Copyable {
        case lhs(CopyableKlass)
        case rhs(MoveOnlyKlass)
    }
    {
        class C3 {
            var copyable: CopyableKlass? = nil
            var moveOnly: MoveOnlyKlass? = nil
            var copyables: [CopyableKlass] = []
            var moveOnlies: [MoveOnlyKlass] = [] // expected-error {{type 'MoveOnlyKlass' does not conform to protocol 'Copyable'}}
        }

class C3MoveOnly: ~Copyable {
            var copyable: CopyableKlass? = nil
            var moveOnly: MoveOnlyKlass? = nil
            var copyables: [CopyableKlass] = []
            var moveOnlies: [MoveOnlyKlass] = [] // expected-error {{type 'MoveOnlyKlass' does not conform to protocol 'Copyable'}}
        }

        struct S3 { // expected-note {{consider adding '~Copyable' to struct 'S3'}}
            var copyable: CopyableKlass
            var moveOnly: MoveOnlyKlass // expected-error {{stored property 'moveOnly' of 'Copyable'-conforming struct 'S3' has non-Copyable type 'MoveOnlyKlass'}}
        }

struct S3MoveOnly: ~Copyable {
            var copyable: CopyableKlass
            var moveOnly: MoveOnlyKlass
        }

        enum E3 { // expected-note {{consider adding '~Copyable' to enum 'E3'}}
            case lhs(CopyableKlass)
            case rhs(MoveOnlyKlass) // expected-error {{associated value 'rhs' of 'Copyable'-conforming enum 'E3' has non-Copyable type 'MoveOnlyKlass'}}
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
protocol P {} // expected-note 6{{type}}
protocol Q {} // expected-note 2{{type}}
class ProtocolCheckMoveOnlyKlass: ~Copyable {}
struct ProtocolCheckMoveOnlyStruct: ~Copyable {
    var k: MoveOnlyKlass
}
enum ProtocolCheckMoveOnlyEnum: ~Copyable {}

extension ProtocolCheckMoveOnlyKlass : P {} // expected-error {{type 'ProtocolCheckMoveOnlyKlass' does not conform to protocol 'Copyable'}}
extension ProtocolCheckMoveOnlyStruct : P, Q {}
// expected-error@-1 2{{type 'ProtocolCheckMoveOnlyStruct' does not conform to protocol 'Copyable'}}
// expected-note@-2 {{'ProtocolCheckMoveOnlyStruct' declares conformance to protocol 'P' here}}

extension ProtocolCheckMoveOnlyStruct: P {}
// expected-error@-1 {{redundant conformance of 'ProtocolCheckMoveOnlyStruct' to protocol 'P'}}

extension ProtocolCheckMoveOnlyEnum : P & Q {}
// expected-error@-1 2{{type 'ProtocolCheckMoveOnlyEnum' does not conform to protocol 'Copyable'}}

extension ProtocolCheckMoveOnlyEnum : Any {}

extension ProtocolCheckMoveOnlyEnum : AnyHashable {}
// expected-error@-1 {{inheritance from non-protocol type 'AnyHashable'}}

// But a normal extension is ok.
extension ProtocolCheckMoveOnlyKlass {}
extension ProtocolCheckMoveOnlyStruct {}
extension ProtocolCheckMoveOnlyEnum {}

// Check if we define a move only type and make it conform on the base type
class MoveOnlyKlassP : P, ~Copyable {} // expected-error {{type 'MoveOnlyKlassP' does not conform to protocol 'Copyable'}}
struct MoveOnlyStructP : P, ~Copyable { // expected-error {{type 'MoveOnlyStructP' does not conform to protocol 'Copyable'}}
    var mv: MoveOnlyKlass
}
enum MoveOnlyEnumP : P, ~Copyable {} // expected-error {{type 'MoveOnlyEnumP' does not conform to protocol 'Copyable'}}

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

// expected-error@+1:21 {{'StrengthLevel' declares raw type 'Int', but cannot yet conform to RawRepresentable because it is noncopyable}}
enum StrengthLevel: Int, ~Copyable {
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
        get async {  } // expected-error {{getter of noncopyable type cannot be 'async' or 'throws'}}
    }
    var two: MoveOnlyKlass {
        get throws {  } // expected-error {{getter of noncopyable type cannot be 'async' or 'throws'}}
    }
}

struct StructHolder {
    var three: EMoveOnly {
        get async throws {  } // expected-error {{getter of noncopyable type cannot be 'async' or 'throws'}}
    }
}

struct BarStruct { // expected-note {{consider adding '~Copyable' to struct 'BarStruct'}}
  init() {}
  deinit {} // expected-error {{deinitializer cannot be declared in struct 'BarStruct' that conforms to 'Copyable'}}
}

enum BarUnion { // expected-note {{consider adding '~Copyable' to enum 'BarUnion'}}
  init() {}
  deinit {} // expected-error {{deinitializer cannot be declared in enum 'BarUnion' that conforms to 'Copyable'}}
}
