// {"kind":"typecheck","original":"4b77614b","signature":"swift::GenericEnvironment::mapTypeIntoEnvironment(swift::GenericEnvironment*, swift::Type)","signatureAssert":"Assertion failed: (!type->hasPrimaryArchetype() && \"already have a contextual type\"), function mapTypeIntoEnvironment","signatureNext":"getInnermostIsolatedContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  @globalActor actor GlobalCoordinator {
    static var shared = GlobalCoordinator
    func register() {
      {
        @GlobalCoordinator in
        GlobalCoordinator.shared
      }
    }
  }
}
