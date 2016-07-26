
@discardableResult
func GLKVector3Make(_ x: Float, _ y: Float, _ z: Float) -> GLKVector3
@discardableResult
func GLKVector3MakeWithArray(_ values: UnsafeMutablePointer<Float>!) -> GLKVector3
@discardableResult
func GLKVector3Negate(_ vector: GLKVector3) -> GLKVector3
@discardableResult
func GLKVector3Add(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> GLKVector3
@discardableResult
func GLKVector3Subtract(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> GLKVector3
@discardableResult
func GLKVector3Multiply(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> GLKVector3
@discardableResult
func GLKVector3Divide(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> GLKVector3
@discardableResult
func GLKVector3AddScalar(_ vector: GLKVector3, _ value: Float) -> GLKVector3
@discardableResult
func GLKVector3SubtractScalar(_ vector: GLKVector3, _ value: Float) -> GLKVector3
@discardableResult
func GLKVector3MultiplyScalar(_ vector: GLKVector3, _ value: Float) -> GLKVector3
@discardableResult
func GLKVector3DivideScalar(_ vector: GLKVector3, _ value: Float) -> GLKVector3
@discardableResult
func GLKVector3Maximum(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> GLKVector3
@discardableResult
func GLKVector3Minimum(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> GLKVector3
@discardableResult
func GLKVector3AllEqualToVector3(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> Bool
@discardableResult
func GLKVector3AllEqualToScalar(_ vector: GLKVector3, _ value: Float) -> Bool
@discardableResult
func GLKVector3AllGreaterThanVector3(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> Bool
@discardableResult
func GLKVector3AllGreaterThanScalar(_ vector: GLKVector3, _ value: Float) -> Bool
@discardableResult
func GLKVector3AllGreaterThanOrEqualToVector3(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> Bool
@discardableResult
func GLKVector3AllGreaterThanOrEqualToScalar(_ vector: GLKVector3, _ value: Float) -> Bool
@discardableResult
func GLKVector3Normalize(_ vector: GLKVector3) -> GLKVector3
@discardableResult
func GLKVector3DotProduct(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> Float
@discardableResult
func GLKVector3Length(_ vector: GLKVector3) -> Float
@discardableResult
func GLKVector3Distance(_ vectorStart: GLKVector3, _ vectorEnd: GLKVector3) -> Float
@discardableResult
func GLKVector3Lerp(_ vectorStart: GLKVector3, _ vectorEnd: GLKVector3, _ t: Float) -> GLKVector3
@discardableResult
func GLKVector3CrossProduct(_ vectorLeft: GLKVector3, _ vectorRight: GLKVector3) -> GLKVector3
@discardableResult
func GLKVector3Project(_ vectorToProject: GLKVector3, _ projectionVector: GLKVector3) -> GLKVector3
