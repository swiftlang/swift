// RUN: not %target-swift-frontend -typecheck %s

struct CommandContext<Command> {
  init() {
    let input = [1,2,3].filter {

extension CommandContext {
  struct Options {
    subscript<T>(path: T) -> Int {
      fatalError()
    }
  }
}
