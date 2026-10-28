// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM %S/../Inputs/COM.swift

// The defining module can provide convenience members for identity protocols.
extension COMInterface {
  var convenience: Int { 0 }
}
extension COMActivatable {
  var convenience: Int { 0 }
}
