// RUN: %target-typecheck-verify-swift -parse-as-library
// RUN: %target-typecheck-verify-swift -parse-as-library -application-extension

@available(macOS 11, *)
@available(macOSApplicationExtension, unavailable)
public struct UnavailableMacOSExtensionsStruct {
  @backDeployed(before: macOS 12)
  public func memberFunc() {}
}
