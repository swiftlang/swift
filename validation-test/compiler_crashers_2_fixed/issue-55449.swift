// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/55449

func verifyDecoder<Decoder>() {

extension ByteToMessageDecoderVerifier {

class RecordingChannel {
  func readInbound<T>(foo bar: Int) {}
