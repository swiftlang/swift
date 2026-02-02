// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

struct AnyPublisher<T> {
  public func sink(_: @escaping ((T) -> Void)) -> Void { fatalError() }
}
class MyClass {
  func fetchFile<T>(with: T) -> AnyPublisher<T> { fatalError() }
  init() {
    fetchFile(with: #^COMPLETE^#42)
      .sink { a in var b = a }
  }
}
