// RUN: %swift -parse %s -verify

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
       self.onNextFunc = { (item: T) -> Void in
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
