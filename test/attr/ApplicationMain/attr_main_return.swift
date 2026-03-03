// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main // expected-error{{'MyBase' is annotated with '@main' and must provide a main static function}}
// expected-note@-1{{add 'static func main()'}} {{8:16-16=\n    static func main() {\n        <#code#>\n    }\n}}
// expected-note@-2{{add 'static func main() throws'}} {{8:16-16=\n    static func main() throws {\n        <#code#>\n    }\n}}
// expected-note@-3{{add 'static func main() async'}} {{8:16-16=\n    static func main() async {\n        <#code#>\n    }\n}}
// expected-note@-4{{add 'static func main() async throws'}} {{8:16-16=\n    static func main() async throws {\n        <#code#>\n    }\n}}
struct MyBase {
  static func main() -> Int {
  }
}
