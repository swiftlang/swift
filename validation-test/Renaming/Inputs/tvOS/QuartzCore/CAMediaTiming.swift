
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
@available(tvOS 2.0, *)
let kCAFillModeForwards: String
@available(tvOS 2.0, *)
let kCAFillModeBackwards: String
@available(tvOS 2.0, *)
let kCAFillModeBoth: String
@available(tvOS 2.0, *)
let kCAFillModeRemoved: String
