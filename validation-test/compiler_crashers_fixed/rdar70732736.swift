// RUN: %target-swift-frontend -emit-ir %s

struct Horse {}

class Reproducer {
    lazy var barn: Any = {
        class Barn {
            var horse: Horse {
                return Horse()
            }
        }
        return Barn()
    }()
}

