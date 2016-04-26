
@discardableResult
func GLKVector4Make(_ x: Float, _ y: Float, _ z: Float, _ w: Float) -> GLKVector4
@discardableResult
func GLKVector4MakeWithArray(_ values: UnsafeMutablePointer<Float>!) -> GLKVector4
@discardableResult
func GLKVector4MakeWithVector3(_ vector: GLKVector3, _ w: Float) -> GLKVector4
@discardableResult
func GLKVector4Negate(_ vector: GLKVector4) -> GLKVector4
@discardableResult
func GLKVector4Add(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> GLKVector4
@discardableResult
func GLKVector4Subtract(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> GLKVector4
@discardableResult
func GLKVector4Multiply(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> GLKVector4
@discardableResult
func GLKVector4Divide(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> GLKVector4
@discardableResult
func GLKVector4AddScalar(_ vector: GLKVector4, _ value: Float) -> GLKVector4
@discardableResult
func GLKVector4SubtractScalar(_ vector: GLKVector4, _ value: Float) -> GLKVector4
@discardableResult
func GLKVector4MultiplyScalar(_ vector: GLKVector4, _ value: Float) -> GLKVector4
@discardableResult
func GLKVector4DivideScalar(_ vector: GLKVector4, _ value: Float) -> GLKVector4
@discardableResult
func GLKVector4Maximum(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> GLKVector4
@discardableResult
func GLKVector4Minimum(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> GLKVector4
@discardableResult
func GLKVector4AllEqualToVector4(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> Bool
@discardableResult
func __builtin_neon_vget_lane_i32(_ _: int2, _ _: Int32) -> UInt32
@discardableResult
func GLKVector4AllEqualToScalar(_ vector: GLKVector4, _ value: Float) -> Bool
@discardableResult
func GLKVector4AllGreaterThanVector4(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> Bool
@discardableResult
func GLKVector4AllGreaterThanScalar(_ vector: GLKVector4, _ value: Float) -> Bool
@discardableResult
func GLKVector4AllGreaterThanOrEqualToVector4(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> Bool
@discardableResult
func GLKVector4AllGreaterThanOrEqualToScalar(_ vector: GLKVector4, _ value: Float) -> Bool
@discardableResult
func GLKVector4Normalize(_ vector: GLKVector4) -> GLKVector4
@discardableResult
func GLKVector4DotProduct(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> Float
@discardableResult
func __builtin_neon_vget_lane_f32(_ _: float2, _ _: Int32) -> Float
@discardableResult
func GLKVector4Length(_ vector: GLKVector4) -> Float
@discardableResult
func GLKVector4Distance(_ vectorStart: GLKVector4, _ vectorEnd: GLKVector4) -> Float
@discardableResult
func GLKVector4Lerp(_ vectorStart: GLKVector4, _ vectorEnd: GLKVector4, _ t: Float) -> GLKVector4
@discardableResult
func GLKVector4CrossProduct(_ vectorLeft: GLKVector4, _ vectorRight: GLKVector4) -> GLKVector4
@discardableResult
func GLKVector4Project(_ vectorToProject: GLKVector4, _ projectionVector: GLKVector4) -> GLKVector4
