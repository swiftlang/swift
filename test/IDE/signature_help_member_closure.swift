// RUN: %target-swift-ide-test -signature-help -code-completion-token=MEMBER_CLOSURE -source-filename=%s | %FileCheck %s --check-prefix=MEMBER_CLOSURE

struct Observable {
  var observer: (String, Int?, [AnyHashable: [Double?]]) async throws -> [Observable?]
  
  func notify() async throws {
    _ = try await observer("EVENT", #^MEMBER_CLOSURE^#, [:])
    // MEMBER_CLOSURE:     Begin signatures, 1 items
    // MEMBER_CLOSURE-DAG: Signature[Active]: observer(<param>String</param>, <param active>Int?</param>, <param>[AnyHashable : [Double?]]</param>) async throws -> [Observable?]
  }
}
