import Foundation
import Observation

@available(SwiftStdlib 5.9, *)
@Observable final public class AnotherObservableClass {
  public var name: String

  init(name: String) {
    self.name = name
  }
}
