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
// expected-note@-1{{add 'static func main()'}} {{23:31-31=\n    static func main() {\n        <#code#>\n    }\n}}
// expected-note@-2{{add 'static func main() throws'}} {{23:31-31=\n    static func main() throws {\n        <#code#>\n    }\n}}
// expected-note@-3{{add 'static func main() async'}} {{23:31-31=\n    static func main() async {\n        <#code#>\n    }\n}}
// expected-note@-4{{add 'static func main() async throws'}} {{23:31-31=\n    static func main() async throws {\n        <#code#>\n    }\n}}
struct EntryPoint : Runnable {
  func run() {
  }
}
