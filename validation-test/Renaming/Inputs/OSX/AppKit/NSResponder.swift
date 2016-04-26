
class NSResponder : NSObject, NSCoding {
  unowned(unsafe) var next: @sil_unmanaged NSResponder?
  @discardableResult
  func tryToPerform(_ anAction: Selector, with anObject: AnyObject?) -> Bool
  @discardableResult
  func performKeyEquivalent(_ theEvent: NSEvent) -> Bool
  @discardableResult
  func validRequestor(forSendType sendType: String, returnType returnType: String) -> AnyObject?
  func mouseDown(_ theEvent: NSEvent)
  func rightMouseDown(_ theEvent: NSEvent)
  func otherMouseDown(_ theEvent: NSEvent)
  func mouseUp(_ theEvent: NSEvent)
  func rightMouseUp(_ theEvent: NSEvent)
  func otherMouseUp(_ theEvent: NSEvent)
  func mouseMoved(_ theEvent: NSEvent)
  func mouseDragged(_ theEvent: NSEvent)
  func scrollWheel(_ theEvent: NSEvent)
  func rightMouseDragged(_ theEvent: NSEvent)
  func otherMouseDragged(_ theEvent: NSEvent)
  func mouseEntered(_ theEvent: NSEvent)
  func mouseExited(_ theEvent: NSEvent)
  func keyDown(_ theEvent: NSEvent)
  func keyUp(_ theEvent: NSEvent)
  func flagsChanged(_ theEvent: NSEvent)
  func tabletPoint(_ theEvent: NSEvent)
  func tabletProximity(_ theEvent: NSEvent)
  @available(OSX 10.5, *)
  func cursorUpdate(_ event: NSEvent)
  @available(OSX 10.5, *)
  func magnify(with event: NSEvent)
  @available(OSX 10.5, *)
  func rotate(with event: NSEvent)
  @available(OSX 10.5, *)
  func swipe(with event: NSEvent)
  @available(OSX 10.5, *)
  func beginGesture(with event: NSEvent)
  @available(OSX 10.5, *)
  func endGesture(with event: NSEvent)
  @available(OSX 10.8, *)
  func smartMagnify(with event: NSEvent)
  @available(OSX 10.6, *)
  func touchesBegan(with event: NSEvent)
  @available(OSX 10.6, *)
  func touchesMoved(with event: NSEvent)
  @available(OSX 10.6, *)
  func touchesEnded(with event: NSEvent)
  @available(OSX 10.6, *)
  func touchesCancelled(with event: NSEvent)
  @available(OSX 10.8, *)
  func quickLook(with event: NSEvent)
  @available(OSX 10.10.3, *)
  func pressureChange(with event: NSEvent)
  func noResponder(for eventSelector: Selector)
  var acceptsFirstResponder: Bool { get }
  @discardableResult
  func becomeFirstResponder() -> Bool
  @discardableResult
  func resignFirstResponder() -> Bool
  func interpretKeyEvents(_ eventArray: [NSEvent])
  func flushBufferedKeyEvents()
  var menu: NSMenu?
  func showContextHelp(_ sender: AnyObject?)
  func helpRequested(_ eventPtr: NSEvent)
  @discardableResult
  func shouldBeTreated(asInkEvent theEvent: NSEvent) -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  func wantsScrollEventsForSwipeTracking(on axis: NSEventGestureAxis) -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  func wantsForwardedScrollEvents(for axis: NSEventGestureAxis) -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  func supplementalTarget(forAction action: Selector, sender sender: AnyObject?) -> AnyObject?
}
extension NSResponder {
  func insertText(_ insertString: AnyObject)
  func doCommand(by aSelector: Selector)
  func moveForward(_ sender: AnyObject?)
  func moveRight(_ sender: AnyObject?)
  func moveBackward(_ sender: AnyObject?)
  func moveLeft(_ sender: AnyObject?)
  func moveUp(_ sender: AnyObject?)
  func moveDown(_ sender: AnyObject?)
  func moveWordForward(_ sender: AnyObject?)
  func moveWordBackward(_ sender: AnyObject?)
  func moveToBeginningOfLine(_ sender: AnyObject?)
  func moveToEndOfLine(_ sender: AnyObject?)
  func moveToBeginningOfParagraph(_ sender: AnyObject?)
  func moveToEndOfParagraph(_ sender: AnyObject?)
  func moveToEndOfDocument(_ sender: AnyObject?)
  func moveToBeginningOfDocument(_ sender: AnyObject?)
  func pageDown(_ sender: AnyObject?)
  func pageUp(_ sender: AnyObject?)
  func centerSelectionInVisibleArea(_ sender: AnyObject?)
  func moveBackwardAndModifySelection(_ sender: AnyObject?)
  func moveForwardAndModifySelection(_ sender: AnyObject?)
  func moveWordForwardAndModifySelection(_ sender: AnyObject?)
  func moveWordBackwardAndModifySelection(_ sender: AnyObject?)
  func moveUpAndModifySelection(_ sender: AnyObject?)
  func moveDownAndModifySelection(_ sender: AnyObject?)
  func moveToBeginningOfLineAndModifySelection(_ sender: AnyObject?)
  func moveToEndOfLineAndModifySelection(_ sender: AnyObject?)
  func moveToBeginningOfParagraphAndModifySelection(_ sender: AnyObject?)
  func moveToEndOfParagraphAndModifySelection(_ sender: AnyObject?)
  func moveToEndOfDocumentAndModifySelection(_ sender: AnyObject?)
  func moveToBeginningOfDocumentAndModifySelection(_ sender: AnyObject?)
  func pageDownAndModifySelection(_ sender: AnyObject?)
  func pageUpAndModifySelection(_ sender: AnyObject?)
  func moveParagraphForwardAndModifySelection(_ sender: AnyObject?)
  func moveParagraphBackwardAndModifySelection(_ sender: AnyObject?)
  func moveWordRight(_ sender: AnyObject?)
  func moveWordLeft(_ sender: AnyObject?)
  func moveRightAndModifySelection(_ sender: AnyObject?)
  func moveLeftAndModifySelection(_ sender: AnyObject?)
  func moveWordRightAndModifySelection(_ sender: AnyObject?)
  func moveWordLeftAndModifySelection(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func moveToLeftEndOfLine(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func moveToRightEndOfLine(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func moveToLeftEndOfLineAndModifySelection(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func moveToRightEndOfLineAndModifySelection(_ sender: AnyObject?)
  func scrollPageUp(_ sender: AnyObject?)
  func scrollPageDown(_ sender: AnyObject?)
  func scrollLineUp(_ sender: AnyObject?)
  func scrollLineDown(_ sender: AnyObject?)
  func scrollToBeginningOfDocument(_ sender: AnyObject?)
  func scrollToEndOfDocument(_ sender: AnyObject?)
  func transpose(_ sender: AnyObject?)
  func transposeWords(_ sender: AnyObject?)
  func selectAll(_ sender: AnyObject?)
  func selectParagraph(_ sender: AnyObject?)
  func selectLine(_ sender: AnyObject?)
  func selectWord(_ sender: AnyObject?)
  func indent(_ sender: AnyObject?)
  func insertTab(_ sender: AnyObject?)
  func insertBacktab(_ sender: AnyObject?)
  func insertNewline(_ sender: AnyObject?)
  func insertParagraphSeparator(_ sender: AnyObject?)
  func insertNewlineIgnoringFieldEditor(_ sender: AnyObject?)
  func insertTabIgnoringFieldEditor(_ sender: AnyObject?)
  func insertLineBreak(_ sender: AnyObject?)
  func insertContainerBreak(_ sender: AnyObject?)
  @available(OSX 10.5, *)
  func insertSingleQuoteIgnoringSubstitution(_ sender: AnyObject?)
  @available(OSX 10.5, *)
  func insertDoubleQuoteIgnoringSubstitution(_ sender: AnyObject?)
  func changeCaseOfLetter(_ sender: AnyObject?)
  func uppercaseWord(_ sender: AnyObject?)
  func lowercaseWord(_ sender: AnyObject?)
  func capitalizeWord(_ sender: AnyObject?)
  func deleteForward(_ sender: AnyObject?)
  func deleteBackward(_ sender: AnyObject?)
  func deleteBackwardByDecomposingPreviousCharacter(_ sender: AnyObject?)
  func deleteWordForward(_ sender: AnyObject?)
  func deleteWordBackward(_ sender: AnyObject?)
  func deleteToBeginningOfLine(_ sender: AnyObject?)
  func deleteToEndOfLine(_ sender: AnyObject?)
  func deleteToBeginningOfParagraph(_ sender: AnyObject?)
  func deleteToEndOfParagraph(_ sender: AnyObject?)
  func yank(_ sender: AnyObject?)
  func complete(_ sender: AnyObject?)
  func setMark(_ sender: AnyObject?)
  func deleteToMark(_ sender: AnyObject?)
  func selectToMark(_ sender: AnyObject?)
  func swapWithMark(_ sender: AnyObject?)
  func cancelOperation(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func makeBaseWritingDirectionNatural(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func makeBaseWritingDirectionLeftToRight(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func makeBaseWritingDirectionRightToLeft(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func makeTextWritingDirectionNatural(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func makeTextWritingDirectionLeftToRight(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  func makeTextWritingDirectionRightToLeft(_ sender: AnyObject?)
  @available(OSX 10.8, *)
  func quickLookPreviewItems(_ sender: AnyObject?)
}
extension NSResponder {
  var undoManager: NSUndoManager? { get }
}
extension NSResponder {
  @available(OSX 10.7, *)
  @discardableResult
  func validateProposedFirstResponder(_ responder: NSResponder, for event: NSEvent?) -> Bool
}
extension NSResponder {
  func presentError(_ error: NSError, modalFor window: NSWindow, delegate delegate: AnyObject?, didPresent didPresentSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @discardableResult
  func presentError(_ error: NSError) -> Bool
  @discardableResult
  func willPresentError(_ error: NSError) -> NSError
}
extension NSResponder {
  @available(OSX 10.7, *)
  func performTextFinderAction(_ sender: AnyObject?)
}
extension NSResponder {
}
