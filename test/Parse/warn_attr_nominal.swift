// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@warn(ExistentialAny, as: error)
class Foo {}

@warn(ExistentialAny, as: ignored)
struct Bar {}

@warn(ExistentialAny, as: warning)
enum Baz {}

@warn(ExistentialAny, as: error)
actor Qux {
    @warn(ExistentialAny, as: ignored)
    struct Quux {}
}
@warn(ExistentialAny, as: warning)
protocol Corge {}

@warn(ExistentialAny, as: ignored)
extension Bar {}
