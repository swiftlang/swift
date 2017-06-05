import ObjectiveC

class OtherObjCClass: NSObject {
  @objc private var privateVar = 1 // expected-note 4{{privateVar' declared here}}
  @objc private(set) var privateSetVar = 2 // expected-note 2{{'privateSetVar' declared here}}
  @objc internal var internalVar = 2

  @objc internal func internalFunc() {}

  private func privateFunc() {} // expected-note 2{{'privateFunc' declared here}}
}
