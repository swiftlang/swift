import Foundation
import CounterFramework

public protocol CounterProtocol {
  var value: Int32 { get set }
}

extension Counter : CounterProtocol {}

open class MyCounter : Counter {
  open override var value: Int32 { didSet { } }
}
