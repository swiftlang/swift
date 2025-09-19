// RUN: %target-swift-ide-test -signature-help -code-completion-token=MEMBER_CLOSURE -source-filename=%s | %FileCheck %s --check-prefix=MEMBER_CLOSURE

struct Observable {
  var observer: (String, Int?, [AnyHashable: [Double?]]) async throws -> [Observable?]
  
  func notify() async throws {
    _ = try await observer("EVENT", #^MEMBER_CLOSURE^#, [:])
  }
}

// MEMBER_CLOSURE:      Begin signatures, 1 items
// MEMBER_CLOSURE-NEXT: Signature[Active]: observer(<param name="">String</param>, <param name="" active>Int?</param>, <param name="">[AnyHashable : [Double?]]</param>) async throws -> [Observable?]
// MEMBER_CLOSURE-NEXT: End signatures
