// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

protocol Runnable {
    init()
    func run()
}

protocol OtherThing {
}

extension Runnable where Self : OtherThing {
    static func main() {
        let it = Self.init()
        it.run()
    }
}

@main //expected-error{{'EntryPoint' is annotated with '@main' and must provide a main static function}}
struct EntryPoint : Runnable {
  func run() {
  }
}

