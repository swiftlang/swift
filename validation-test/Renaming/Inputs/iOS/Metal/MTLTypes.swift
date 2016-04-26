
struct MTLOrigin {
  var x: Int
  var y: Int
  var z: Int
  init()
  init(x x: Int, y y: Int, z z: Int)
}
@discardableResult
func MTLOriginMake(_ x: Int, _ y: Int, _ z: Int) -> MTLOrigin
struct MTLSize {
  var width: Int
  var height: Int
  var depth: Int
  init()
  init(width width: Int, height height: Int, depth depth: Int)
}
@discardableResult
func MTLSizeMake(_ width: Int, _ height: Int, _ depth: Int) -> MTLSize
struct MTLRegion {
  var origin: MTLOrigin
  var size: MTLSize
  init()
  init(origin origin: MTLOrigin, size size: MTLSize)
}
@discardableResult
func MTLRegionMake1D(_ x: Int, _ width: Int) -> MTLRegion
@discardableResult
func MTLRegionMake2D(_ x: Int, _ y: Int, _ width: Int, _ height: Int) -> MTLRegion
@discardableResult
func MTLRegionMake3D(_ x: Int, _ y: Int, _ z: Int, _ width: Int, _ height: Int, _ depth: Int) -> MTLRegion
