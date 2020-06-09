// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

protocol Runnable {
    init()
    func run()
}

extension Runnable {
    static func main() {
        let it = Self.init()
        it.run()
    }
}

@main
struct EntryPoint : Runnable {
  func run() {
  }
}
