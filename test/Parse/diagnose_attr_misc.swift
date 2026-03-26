// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@diagnose(ExistentialAny, as: error)
import Swift

struct garply: ~Copyable {
    @diagnose(ExistentialAny, as: ignored)
    init() {}

    @diagnose(ExistentialAny, as: ignored)
    subscript(index: Int) -> Int {
        return 11
    }

    @diagnose(ExistentialAny, as: ignored)
    var computedProperty: Int {
        return 11
    }

    var property: Int {
        @diagnose(ExistentialAny, as: error)
        get {
            return 11
        }
        @diagnose(ExistentialAny, as: warning)
        set {
            let _ = 11
        }
    }
}
