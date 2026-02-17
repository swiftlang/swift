// Test that unavailable protocol requirements don't crash the compiler during IR generation
// rdar://170184865
// RUN: %target-swift-emit-irgen %s -O -enable-library-evolution

// Witness table should NOT contain entries for the unavailable methods
// RUN: %target-swift-emit-silgen %s -O -enable-library-evolution | %FileCheck %s --check-prefix=CHECK-SIL
// CHECK-SIL-NOT: _deinitializeAttachableValue{{.*}}FTW

@available(*, unavailable)
public protocol AttachableAsImage {
  func _deinitializeAttachableValue()
}

@available(*, unavailable)
public final class ImageWrapper<Image: AttachableAsImage> {
  public let wrappedValue: Image
  init(image: Image) {
      self.wrappedValue = image
  }
  deinit {
      wrappedValue._deinitializeAttachableValue() 
  }
}
