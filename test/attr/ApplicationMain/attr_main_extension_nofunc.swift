// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

class EntryPoint {
}

@main // expected-error{{'EntryPoint' is annotated with '@main' and must provide a main static function}}
extension EntryPoint {
}


