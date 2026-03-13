// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

class EntryPoint {
}

@main // expected-error{{'EntryPoint' is annotated with '@main' and must provide a main static function}}
// expected-note@-1{{add 'static func main()'}} {{11:23-23=\n    static func main() {\n        <#code#>\n    }\n}}
// expected-note@-2{{add 'static func main() throws'}} {{11:23-23=\n    static func main() throws {\n        <#code#>\n    }\n}}
// expected-note@-3{{add 'static func main() async'}} {{11:23-23=\n    static func main() async {\n        <#code#>\n    }\n}}
// expected-note@-4{{add 'static func main() async throws'}} {{11:23-23=\n    static func main() async throws {\n        <#code#>\n    }\n}}
extension EntryPoint {
}
