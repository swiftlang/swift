import Foundation
@exported import AppKit

// Fix the ARGV type of NSApplicationMain, which nonsensically takes
// argv as a const char**.
@asmname("NSApplicationMain")
public func NSApplicationMain(
  argc: Int32, _ argv: UnsafeMutablePointer<UnsafeMutablePointer<CChar>>
) -> Int32

