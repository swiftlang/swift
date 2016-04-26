
class GLKMatrixStack {
}
@discardableResult
func GLKMatrixStackCreate(_ alloc: CFAllocator?) -> Unmanaged<GLKMatrixStack>?
@discardableResult
func GLKMatrixStackGetTypeID() -> CFTypeID
func GLKMatrixStackPush(_ stack: GLKMatrixStack)
func GLKMatrixStackPop(_ stack: GLKMatrixStack)
@discardableResult
func GLKMatrixStackSize(_ stack: GLKMatrixStack) -> Int32
func GLKMatrixStackLoadMatrix4(_ stack: GLKMatrixStack, _ matrix: GLKMatrix4)
@discardableResult
func GLKMatrixStackGetMatrix4(_ stack: GLKMatrixStack) -> GLKMatrix4
@discardableResult
func GLKMatrixStackGetMatrix3(_ stack: GLKMatrixStack) -> GLKMatrix3
@discardableResult
func GLKMatrixStackGetMatrix2(_ stack: GLKMatrixStack) -> GLKMatrix2
@discardableResult
func GLKMatrixStackGetMatrix4Inverse(_ stack: GLKMatrixStack) -> GLKMatrix4
@discardableResult
func GLKMatrixStackGetMatrix4InverseTranspose(_ stack: GLKMatrixStack) -> GLKMatrix4
@discardableResult
func GLKMatrixStackGetMatrix3Inverse(_ stack: GLKMatrixStack) -> GLKMatrix3
@discardableResult
func GLKMatrixStackGetMatrix3InverseTranspose(_ stack: GLKMatrixStack) -> GLKMatrix3
func GLKMatrixStackMultiplyMatrix4(_ stack: GLKMatrixStack, _ matrix: GLKMatrix4)
func GLKMatrixStackMultiplyMatrixStack(_ stackLeft: GLKMatrixStack, _ stackRight: GLKMatrixStack)
func GLKMatrixStackTranslate(_ stack: GLKMatrixStack, _ tx: Float, _ ty: Float, _ tz: Float)
func GLKMatrixStackTranslateWithVector3(_ stack: GLKMatrixStack, _ translationVector: GLKVector3)
func GLKMatrixStackTranslateWithVector4(_ stack: GLKMatrixStack, _ translationVector: GLKVector4)
func GLKMatrixStackScale(_ stack: GLKMatrixStack, _ sx: Float, _ sy: Float, _ sz: Float)
func GLKMatrixStackScaleWithVector3(_ stack: GLKMatrixStack, _ scaleVector: GLKVector3)
func GLKMatrixStackScaleWithVector4(_ stack: GLKMatrixStack, _ scaleVector: GLKVector4)
func GLKMatrixStackRotate(_ stack: GLKMatrixStack, _ radians: Float, _ x: Float, _ y: Float, _ z: Float)
func GLKMatrixStackRotateWithVector3(_ stack: GLKMatrixStack, _ radians: Float, _ axisVector: GLKVector3)
func GLKMatrixStackRotateWithVector4(_ stack: GLKMatrixStack, _ radians: Float, _ axisVector: GLKVector4)
func GLKMatrixStackRotateX(_ stack: GLKMatrixStack, _ radians: Float)
func GLKMatrixStackRotateY(_ stack: GLKMatrixStack, _ radians: Float)
func GLKMatrixStackRotateZ(_ stack: GLKMatrixStack, _ radians: Float)
