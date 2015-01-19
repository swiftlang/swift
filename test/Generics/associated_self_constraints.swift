// RUN: %target-parse-verify-swift

protocol Observer {
    typealias Value
    
    func onNext(item: Value) -> Void
    func onCompleted() -> Void
    func onError(error: String) -> Void
}

protocol Observable {
    typealias Value

    func subscribe<O: Observer where O.Value == Value>(observer: O) -> Any
}

class Subject<T>: Observer, Observable {
    typealias Value = T
    
    // Observer implementation
    
    var onNextFunc: ((T) -> Void)? = nil
    var onCompletedFunc: (() -> Void)? = nil
    var onErrorFunc: ((String) -> Void)? = nil
    
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
    
    func subscribe<O: Observer where O.Value == T>(observer: O) -> Any {
        self.onNextFunc = { (item: T) -> Void  in
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

struct X<T> {

  mutating func replace<C: _CollectionType where C._Element == T>(a: C) {
    for i in a.startIndex..<a.endIndex {
      var x: T = a[i]
    }
  }
}

protocol P {
    typealias A
    
    func onNext(item: A) -> Void
}

struct IP<T> : P {
    typealias A = T

    init<O:P where O.A == IP.A>(x:O) {
       _onNext = { (item: A) in x.onNext(item) }
    }

    func onNext(item: A) { _onNext(item) }

    var _onNext: (A)->()
}
