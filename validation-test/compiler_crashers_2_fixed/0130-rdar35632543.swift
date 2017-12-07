// RUN: %target-swift-frontend %s -emit-ir

public protocol ObservableType {
  associatedtype E
}

public protocol SubjectType : ObservableType {
  associatedtype SubjectObserverType : ObserverType
}

extension ObservableType {
  public func multicast<S: SubjectType>(_ subject: S) -> Observable<S.E> {
    while true {}
  }
}


extension ObservableType {
  public func publish() -> Observable<E> {
    return self.multicast(PublishSubject())
  }
}

public class Observable<Element> : ObservableType {
  public typealias E = Element
}

public protocol ObserverType {
  associatedtype E
}

public final class PublishSubject<Element> : Observable<Element>, SubjectType, ObserverType
{
  public typealias SubjectObserverType = PublishSubject<Element>
}

