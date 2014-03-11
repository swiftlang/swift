// RUN: %swift -parse %s -verify

protocol Observer {
    typealias Value
    
    func onNext(item: Value) -> Void
    func onCompleted() -> Void
    func onError(error: String) -> Void
}

protocol Observable {
    typealias Value

    func subscribe<O: Observer where O.Value == Value>(observer: O) -> Any // expected-note {{protocol requires function 'subscribe' with type '<O> (observer: O) -> Any'}}
}

class Subject<T>: Observer, Observable { // expected-error {{type 'Subject<T>' does not conform to protocol 'Observable'}}
    typealias Value = T
    
    // Observer implementation
    
    var onNextFunc: ((T) -> Void)?
    var onCompletedFunc: (() -> Void)?
    var onErrorFunc: ((String) -> Void)?
    
    func onNext(item: T) -> Void {
        onNextFunc?(item)
    }
    
    func onCompleted() -> Void {
        onCompletedFunc?()
    }
    
    func onError(error: String) -> Void {
        onErrorFunc?(error)
    }
    
    // Observable implementation
    
    func subscribe<O: Observer where O.Value == T>(observer: O) -> Any { // expected-note {{candidate has non-matching type '<T, O> (observer: O) -> Any'}}
        self.onNextFunc = { (item: T) -> Void  in // expected-error {{expression does not type-check}}
            observer.onNext(item)
        }
        
        self.onCompletedFunc = {
            observer.onCompleted()
        }
        
        self.onErrorFunc = { (error: String) -> Void in
            observer.onError(error)
        }
        
        return self;
    }
}
