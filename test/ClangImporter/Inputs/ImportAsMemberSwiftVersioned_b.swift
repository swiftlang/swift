import Foundation
import ImportAsMember.Class

@inline(__always) public func call_foo() -> Any {
  return foo("hello" as IncompleteImportTargetName)
}
