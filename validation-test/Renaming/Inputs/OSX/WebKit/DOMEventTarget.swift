
@available(OSX 10.4, *)
protocol DOMEventTarget : NSObjectProtocol, NSCopying {
  @available(OSX 10.5, *)
  func addEventListener(_ type: String!, listener listener: DOMEventListener!, useCapture useCapture: Bool)
  @available(OSX 10.5, *)
  func removeEventListener(_ type: String!, listener listener: DOMEventListener!, useCapture useCapture: Bool)
  @discardableResult
  func dispatchEvent(_ event: DOMEvent!) -> Bool
}
