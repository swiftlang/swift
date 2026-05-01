// {"kind":"emit-sil","original":"1030a31d","signature":"(anonymous namespace)::RawSILInstLowering::run()","signatureAssert":"Assertion failed: (b.getModule().getASTContext().hadError() && \"assign_or_init must have a valid mode\"), function lowerAssignOrInitInstruction","signatureNext":"SILPassManager::runPassOnFunction"}
// RUN: %target-swift-frontend -emit-sil %s > /dev/null
@propertyWrapper
struct a<b> {
  var wrappedValue: b
}
struct c {
  @a var d = 1
  init() {
  }
  init(e: Int) {
    self.init()
    d = e
  }
}
