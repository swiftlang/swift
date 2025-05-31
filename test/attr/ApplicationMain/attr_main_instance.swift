// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main // expected-error{{'MyBase' is annotated with '@main' and must provide a main static function}} {{4:15-15=\n    static func main() {\n        <#code#>\n    }\n}} {{4:15-15=\n    static func main() throws {\n        <#code#>\n    }\n}} {{4:15-15=\n    static func main() async {\n        <#code#>\n    }\n}} {{4:15-15=\n    static func main() async throws {\n        <#code#>\n    }\n}}
class MyBase {
  func main() {
  }
}

