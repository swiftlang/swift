// RUN: %target-typecheck-verify-swift

protocol SR11572_P {
    associatedtype T   
    func SR11572(arg: T)
}
 
struct SR11572_S: SR11572_P {
    func SR11572(arg: Void) { }
}

func SR11572<T: SR11572_P>(some args: T ...) where T.T == Void { }

func SR11572<T: SR11572_P, K: SR11572_P>(some arg1: T, _ arg2: K) { }

func SR11572(another arg1: SR11572_S, _ arg2: SR11572_S) { }

SR11572(some: SR11572_S(), SR11572_S()) // Ok
