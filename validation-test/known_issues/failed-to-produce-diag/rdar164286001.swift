// rdar://164286001
// RUN: %target-typecheck-verify-swift

func memset(_ b: UnsafeMutableRawPointer, _ c: Int32, _ len: Int) -> UnsafeMutableRawPointer { b }

class AVAudioPCMBuffer {
  var floatChannelData: UnsafePointer<UnsafeMutablePointer<Float>>? { nil }
}

func pad(paddedBuffer: AVAudioPCMBuffer, samplesToAdd: Int) {
  memset(paddedBuffer.floatChannelData?[0], 0, samplesToAdd * MemoryLayout<Float>.size) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
