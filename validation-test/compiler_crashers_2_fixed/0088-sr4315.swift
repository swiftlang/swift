// RUN: %target-swift-frontend -emit-ir %s

public protocol ParsableResult {
    associatedtype Parser
}

final class ResultElement<T> {
}


enum InteractiveAPIError: Error {
    case malformedResult
}

class XMLSAXElementParser<ChildElement> {
    public required init() {}
}



final class InteractiveAPICall<Document: ParsableResult, Parser>:
XMLSAXElementParser<Document.Parser> where Document.Parser == Parser {

    required init() {}

    func result(_: Document) {
    }
}
