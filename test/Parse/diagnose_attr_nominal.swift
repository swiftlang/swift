// RUN: %target-typecheck-verify-swift

@diagnose(ExistentialAny, as: error)
class Foo {}

@diagnose(ExistentialAny, as: ignored)
struct Bar {}

@diagnose(ExistentialAny, as: warning)
enum Baz {}

@diagnose(ExistentialAny, as: error)
actor Qux {
    @diagnose(ExistentialAny, as: ignored)
    struct Quux {}
}
@diagnose(ExistentialAny, as: warning)
protocol Corge {}

@diagnose(ExistentialAny, as: ignored)
extension Bar {}
