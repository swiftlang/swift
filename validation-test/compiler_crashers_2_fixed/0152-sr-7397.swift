// RUN: %target-typecheck-verify-swift %s

protocol _UnicodeParser_ {
    associatedtype Encoding: _UnicodeEncoding_
}
protocol _UnicodeEncoding_ {
    associatedtype CodeUnit : BinaryInteger_
    associatedtype ForwardParser : _UnicodeParser_
      where ForwardParser.Encoding == Self

}
protocol BinaryInteger_ {
    associatedtype Words: Collection_ where Words.Index == Int_
}
protocol Collection_ {
    associatedtype Index: Comparable_
}
protocol Comparable_ {}
struct Int_: Comparable_ {}
