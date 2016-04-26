
@discardableResult
func GLKVector2Make(_ x: Float, _ y: Float) -> GLKVector2
@discardableResult
func GLKVector2MakeWithArray(_ values: UnsafeMutablePointer<Float>!) -> GLKVector2
@discardableResult
func GLKVector2Negate(_ vector: GLKVector2) -> GLKVector2
@discardableResult
func GLKVector2Add(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> GLKVector2
@discardableResult
func GLKVector2Subtract(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> GLKVector2
@discardableResult
func GLKVector2Multiply(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> GLKVector2
@discardableResult
func GLKVector2Divide(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> GLKVector2
@discardableResult
func GLKVector2AddScalar(_ vector: GLKVector2, _ value: Float) -> GLKVector2
@discardableResult
func GLKVector2SubtractScalar(_ vector: GLKVector2, _ value: Float) -> GLKVector2
@discardableResult
func GLKVector2MultiplyScalar(_ vector: GLKVector2, _ value: Float) -> GLKVector2
@discardableResult
func GLKVector2DivideScalar(_ vector: GLKVector2, _ value: Float) -> GLKVector2
@discardableResult
func GLKVector2Maximum(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> GLKVector2
@discardableResult
func GLKVector2Minimum(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> GLKVector2
@discardableResult
func GLKVector2AllEqualToVector2(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> Bool
@discardableResult
func GLKVector2AllEqualToScalar(_ vector: GLKVector2, _ value: Float) -> Bool
@discardableResult
func GLKVector2AllGreaterThanVector2(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> Bool
@discardableResult
func GLKVector2AllGreaterThanScalar(_ vector: GLKVector2, _ value: Float) -> Bool
@discardableResult
func GLKVector2AllGreaterThanOrEqualToVector2(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> Bool
@discardableResult
func GLKVector2AllGreaterThanOrEqualToScalar(_ vector: GLKVector2, _ value: Float) -> Bool
@discardableResult
func GLKVector2Normalize(_ vector: GLKVector2) -> GLKVector2
@discardableResult
func GLKVector2DotProduct(_ vectorLeft: GLKVector2, _ vectorRight: GLKVector2) -> Float
@discardableResult
func GLKVector2Length(_ vector: GLKVector2) -> Float
@discardableResult
func GLKVector2Distance(_ vectorStart: GLKVector2, _ vectorEnd: GLKVector2) -> Float
@discardableResult
func GLKVector2Lerp(_ vectorStart: GLKVector2, _ vectorEnd: GLKVector2, _ t: Float) -> GLKVector2
@discardableResult
func GLKVector2Project(_ vectorToProject: GLKVector2, _ projectionVector: GLKVector2) -> GLKVector2
