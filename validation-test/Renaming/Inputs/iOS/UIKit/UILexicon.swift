
@available(iOS 8.0, *)
class UILexiconEntry : NSObject, NSCopying {
  var documentText: String { get }
  var userInput: String { get }
}
@available(iOS 8.0, *)
class UILexicon : NSObject, NSCopying {
  var entries: [UILexiconEntry] { get }
}
