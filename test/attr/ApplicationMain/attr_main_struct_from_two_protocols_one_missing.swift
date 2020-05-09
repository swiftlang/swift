// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

protocol Runnable {
    init()
    func run()
}

protocol OtherThing {
}

extension Runnable where Self : OtherThing { // expected-note{{where 'Self' = 'EntryPoint'}}
    static func main() {
        let it = Self.init()
        it.run()
    }
}

@main // expected-error{{referencing static method 'main()' on 'Runnable' requires that 'EntryPoint' conform to 'OtherThing'}}
struct EntryPoint : Runnable {
  func run() {
  }
}

