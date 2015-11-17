import Foundation
@_exported import AppKit

// Fix the ARGV type of NSApplicationMain, which nonsensically takes
// argv as a const char**.
@_silgen_name("NSApplicationMain")
public func NSApplicationMain(
  argc: Int32, _ argv: UnsafeMutablePointer<UnsafeMutablePointer<CChar>>
) -> Int32

