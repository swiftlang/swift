import Foundation
import CounterFramework

open class MyCounter : Counter {
  open override var value: Int32 { didSet { } }
}
