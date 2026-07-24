// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -import-objc-header %S/Inputs/objc_implementation_async_availability.h -target %target-stable-abi-triple -Xcc -Wno-nullability-completeness
// REQUIRES: objc_interop

// An imported Objective-C completion-handler method that carries an
// availability attribute must still expose its async alternative. Otherwise an
// async member of an '@objc @implementation' extension fails to match the
// completion-handler requirement, no ObjC thunk is generated, and calling the
// method through the ObjC runtime crashes with "unrecognized selector".

@objc @implementation extension WidgetStore {
  @available(SwiftStdlib 5.1, *)
  public func fetchWidgets() async -> [any Widget] {
    return []
  }
}
