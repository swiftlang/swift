// RUN: %target-swift-frontend -c %s

struct NC<Element>: ~Copyable {
  var count: Int = 0
}
func playback_handler2() {
  var iou_ptr: UnsafePointer<Bool>? = nil
  var noncopyable = NC<Int>()
  _ = noncopyable.count
  _ = iou_ptr!.pointee
}
