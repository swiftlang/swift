// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -module-name Lib -target %target-future-triple %s | %FileCheck %s

public func run(_ body: () throws -> Void, _ matches: (any Error) -> Bool) -> Bool {
  do { try body(); return false } catch { return matches(error) }
}

// CHECK-LABEL: sil @$s3Lib5checkySbxm_yyKXEts5ErrorRzlF
// CHECK-NOT:     @error_type
// CHECK:         checked_cast_addr_br test_only any Error in {{%.*}} to E,
// CHECK-NOT:     @error_type
public func check<E: Error>(_ t: E.Type, _ body: () throws -> Void) -> Bool {
  run(body, { $0 is E })
}

// CHECK-LABEL: sil @$s3Lib10checkArrayySbxm_yyKXEts5ErrorRzlF
// CHECK-NOT:     @error_type
// CHECK:         checked_cast_addr_br test_only any Error in {{%.*}} to Array<E>,
// CHECK-NOT:     @error_type
public func checkArray<E: Error>(_ t: E.Type, _ body: () throws -> Void) -> Bool {
  run(body, { $0 is [E] })
}
