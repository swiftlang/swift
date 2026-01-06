// RUN: %target-swift-emit-silgen -swift-version 5 -verify %s

// In Swift 6 mode, we introduce an implicit `@Sendable` conversion on the
// function value. Ensure that this doesn't impede C function pointer
// conversion.
// RUN: %target-swift-emit-silgen -swift-version 6 -verify %s

public typealias PDCallbackFunction = @convention(c) (UnsafeMutableRawPointer?) -> Int32

public enum System {
  public static func setUpdateCallback(update: @escaping PDCallbackFunction, userdata: UnsafeMutableRawPointer?) {
  }
}

@_cdecl("eventHandler")
public func eventHandler(
  pointer: UnsafeMutableRawPointer!
) -> Int32 {
  System.setUpdateCallback(update: update(pointer:), userdata: nil)
  return 0
}

func update(pointer: UnsafeMutableRawPointer!) -> Int32 { 0 }
