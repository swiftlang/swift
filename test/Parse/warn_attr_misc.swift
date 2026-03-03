// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@warn(ExistentialAny, as: error)
import Swift

struct garply: ~Copyable {
    @warn(ExistentialAny, as: ignored)
    init() {}

    @warn(ExistentialAny, as: ignored)
    subscript(index: Int) -> Int {
        return 11
    }

    @warn(ExistentialAny, as: ignored)
    var computedProperty: Int {
        return 11
    }

    var property: Int {
        @warn(ExistentialAny, as: error)
        get {
            return 11
        }
        @warn(ExistentialAny, as: warning)
        set {
            let _ = 11
        }
    }
}
