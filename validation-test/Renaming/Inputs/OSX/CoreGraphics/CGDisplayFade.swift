
typealias CGDisplayFadeReservationToken = UInt32
var kCGDisplayFadeReservationInvalidToken: Int32 { get }
typealias CGDisplayBlendFraction = Float
var kCGDisplayBlendNormal: Double { get }
var kCGDisplayBlendSolidColor: Double { get }
typealias CGDisplayFadeInterval = Float
@available(OSX 10.2, *)
@discardableResult
func CGConfigureDisplayFadeEffect(_ config: CGDisplayConfigRef?, _ fadeOutSeconds: CGDisplayFadeInterval, _ fadeInSeconds: CGDisplayFadeInterval, _ fadeRed: Float, _ fadeGreen: Float, _ fadeBlue: Float) -> CGError
typealias CGDisplayReservationInterval = Float
@available(OSX 10.2, *)
@discardableResult
func CGAcquireDisplayFadeReservation(_ seconds: CGDisplayReservationInterval, _ token: UnsafeMutablePointer<CGDisplayFadeReservationToken>?) -> CGError
@available(OSX 10.2, *)
@discardableResult
func CGReleaseDisplayFadeReservation(_ token: CGDisplayFadeReservationToken) -> CGError
@available(OSX 10.2, *)
@discardableResult
func CGDisplayFade(_ token: CGDisplayFadeReservationToken, _ duration: CGDisplayFadeInterval, _ startBlend: CGDisplayBlendFraction, _ endBlend: CGDisplayBlendFraction, _ redBlend: Float, _ greenBlend: Float, _ blueBlend: Float, _ synchronous: boolean_t) -> CGError
