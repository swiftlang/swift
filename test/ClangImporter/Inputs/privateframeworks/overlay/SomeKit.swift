@_exported import SomeKit
import Foundation

extension SKWidget {
  public func extensionMethod() -> ExtensionType { return ExtensionType() }

  public struct ExtensionType { }
}

extension SKWidget.ExtensionType {
  public func foo() { }
}

extension NSObject {
  public func doSomethingElse(_: SKWidget) { }
}

extension SKWidget.Error {
  public func getCode(from widget: SKWidget) -> SKWidget.Error.Code {
    return widget.getCurrentError()
  }
}

extension SKWidget.Error.Code {
  public var isBoom: Bool {
    return self == .boom
  }
}

public protocol HasAnObject {
  var anObject: NSObject { get set }
}

extension SKWidget : HasAnObject { }

@inline(__always)
public func inlineWidgetOperations(_ widget: SKWidget) {
  widget.extensionMethod().foo()
  widget.someObjCMethod()
  widget.someObjCExtensionMethod()
  widget.doSomething(with: widget)
  widget.doSomethingElse(widget)
  let obj = widget.anObject
  widget.anObject = obj
  _ = SKWidget.Error(.boom).getCode(from: widget).isBoom
  var hao: HasAnObject = widget
  someKitGlobalFunc()
  hao.anObject = widget
}

@available(swift, deprecated: 4.2)
public func someKitOtherGlobalFunc() { }

extension SKWidget {
  public var name: String { return "blah" }
}
