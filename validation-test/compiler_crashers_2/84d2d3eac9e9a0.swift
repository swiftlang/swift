// {"signature":"swift::emitLoadedModuleTraceIfNeeded(swift::ModuleDecl*, swift::DependencyTracker*, swift::FrontendOptions const&, swift::InputFile const&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  wrappedValue : b var projectedValue init(projectedValue c) {
    func d(@a Int) d
