
protocol CAMediaTiming {
  var beginTime: CFTimeInterval { get set }
  var duration: CFTimeInterval { get set }
  var speed: Float { get set }
  var timeOffset: CFTimeInterval { get set }
  var repeatCount: Float { get set }
  var repeatDuration: CFTimeInterval { get set }
  var autoreverses: Bool { get set }
  var fillMode: String { get set }
}
@available(OSX 10.5, *)
let kCAFillModeForwards: String
@available(OSX 10.5, *)
let kCAFillModeBackwards: String
@available(OSX 10.5, *)
let kCAFillModeBoth: String
@available(OSX 10.5, *)
let kCAFillModeRemoved: String
