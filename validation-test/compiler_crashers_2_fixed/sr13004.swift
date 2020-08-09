// RUN: not %target-swift-frontend -typecheck %s

func verifyDecoder<Decoder>() {

extension ByteToMessageDecoderVerifier {

class RecordingChannel {
  func readInbound<T>(foo bar: Int) {}
