// {"signature":"swift::Parser::consumeTokenWithoutFeedingReceiver()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@_extern(a
