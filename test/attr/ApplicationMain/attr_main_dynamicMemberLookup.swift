// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main @dynamicMemberLookup // expected-error{{'Main' is annotated with '@main' and must provide a main static function}}
struct Main {
    subscript(dynamicMember member: String) -> () -> Void {
        return {
        }
    }
}

