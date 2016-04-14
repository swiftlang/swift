// RUN: %target-parse-verify-swift

protocol Observer {
    associatedtype Value
    
    func onNext(_ item: Value) -> Void
    func onCompleted() -> Void
    func onError(_ error: String) -> Void
}

protocol Observable {
    associatedtype Value

    func subscribe<O: Observer where O.Value == Value>(_ observer: O) -> Any
}

class Subject<T>: Observer, Observable {
    typealias Value = T
    
    // Observer implementation
    
    var onNextFunc: ((T) -> Void)? = nil
    var onCompletedFunc: (() -> Void)? = nil
    var onErrorFunc: ((String) -> Void)? = nil
    
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
    
    func subscribe<O: Observer where O.Value == T>(_ observer: O) -> Any {
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

struct X<T> {

  // FIXME swift-3-indexing-model: Check with Ted K or Joe P about
  // whether this change to the test is legit.  They appear to have
  // added it in fad874708e05cff56aec5587a4b0f49cdadc6d11 or
  // 53c69a956f3fac878bc5a00a243bb05245520208, respectively
  mutating func replace<
    C : Collection where C.Iterator.Element == T, C.Index : Strideable
  >(a: C) {
    for i in a.startIndex..<a.endIndex {
      _ = a[i] as T
    }
  }
}

protocol P {
    associatedtype A
    
    func onNext(_ item: A) -> Void
}

struct IP<T> : P {
    typealias A = T

    init<O:P where O.A == IP.A>(x:O) {
       _onNext = { (item: A) in x.onNext(item) }
    }

    func onNext(_ item: A) { _onNext(item) }

    var _onNext: (A) -> ()
}
