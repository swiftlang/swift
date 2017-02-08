// RUN: %target-typecheck-verify-swift

protocol Observer {
    associatedtype Value
    
    func onNext(_ item: Value) -> Void
    func onCompleted() -> Void
    func onError(_ error: String) -> Void
}

protocol Observable {
    associatedtype Value

    func subscribe<O: Observer>(_ observer: O) -> Any where O.Value == Value
}

class Subject<T>: Observer, Observable {
    typealias Value = T
    
    // Observer implementation
    
    var onNextFunc: ((T) -> Void)?
    var onCompletedFunc: (() -> Void)?
    var onErrorFunc: ((String) -> Void)?
    
    func onNext(_ item: T) -> Void {
        onNextFunc?(item)
    }
    
    func onCompleted() -> Void {
        onCompletedFunc?()
    }
    
    func onError(_ error: String) -> Void {
        onErrorFunc?(error)
    }
    
    // Observable implementation
    
    func subscribe<O: Observer>(_ observer: O) -> Any where O.Value == T {
        self.onNextFunc = { (item: T) -> Void in
            observer.onNext(item)
        }
        
        self.onCompletedFunc = {
            observer.onCompleted()
        }
        
        self.onErrorFunc = { (error: String) -> Void in
            observer.onError(error)
        }
        
        return self
    }
}

protocol P {
    associatedtype A
    
    func onNext(_ item: A) -> Void
}

struct IP<T> : P {
    typealias A = T

    init<O:P>(x:O) where O.A == IP.A {
       _onNext = { (item: A) in x.onNext(item) }
    }

    func onNext(_ item: A) { _onNext(item) }

    var _onNext: (A) -> ()
}
