// RUN: %target-swift-frontend -typecheck -parse-as-library -target %target-swift-5.0-abi-triple -verify %s
// REQUIRES: OS=macosx && CPU=x86_64

@main // expected-error{{'MyBaseWithoutAsyncSupport' is annotated with '@main' and must provide a main static function}}
// expected-note@-1{{add 'static func main()'}} {{7:34-34=\n    static func main() {\n        <#code#>\n    }\n}}
// expected-note@-2{{add 'static func main() throws'}} {{7:34-34=\n    static func main() throws {\n        <#code#>\n    }\n}}
class MyBaseWithoutAsyncSupport {
  func main() {
  }
}
