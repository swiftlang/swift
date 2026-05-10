// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

protocol P {
    associatedtype Value
}

class G<Value>: P {}

extension P {
    func map<Return>(_ transform: (Value) -> Return) -> G<Return> {
        fatalError()
    }

    func filter(_ predicate: (Value) -> Bool) -> G<Value> {
        fatalError()
    }
}

class Zip<First, Second, Third, Fourth, Fifth>: P {
    typealias Value = (First?, Second?, Third?, Fourth?, Fifth?)

    func lazy() -> G<(First,Second,Third,Fourth,Fifth)> {
        return filter {
            $0 != nil && $1 != nil && $2 != nil && $3 != nil && $4 != nil
        }.map {
            return ($0!,$1!,$2!,$3!,$4!)
        }
    }

}

