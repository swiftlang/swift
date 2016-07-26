
typealias CFTreeRetainCallBack = @convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!
typealias CFTreeReleaseCallBack = @convention(c) (UnsafePointer<Void>!) -> Void
typealias CFTreeCopyDescriptionCallBack = @convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!
struct CFTreeContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: CFTreeRetainCallBack!
  var release: CFTreeReleaseCallBack!
  var copyDescription: CFTreeCopyDescriptionCallBack!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: CFTreeRetainCallBack!, release release: CFTreeReleaseCallBack!, copyDescription copyDescription: CFTreeCopyDescriptionCallBack!)
}
typealias CFTreeApplierFunction = @convention(c) (UnsafePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void
class CFTree {
}
@discardableResult
func CFTreeGetTypeID() -> CFTypeID
@discardableResult
func CFTreeCreate(_ allocator: CFAllocator!, _ context: UnsafePointer<CFTreeContext>!) -> CFTree!
@discardableResult
func CFTreeGetParent(_ tree: CFTree!) -> CFTree!
@discardableResult
func CFTreeGetNextSibling(_ tree: CFTree!) -> CFTree!
@discardableResult
func CFTreeGetFirstChild(_ tree: CFTree!) -> CFTree!
func CFTreeGetContext(_ tree: CFTree!, _ context: UnsafeMutablePointer<CFTreeContext>!)
@discardableResult
func CFTreeGetChildCount(_ tree: CFTree!) -> CFIndex
@discardableResult
func CFTreeGetChildAtIndex(_ tree: CFTree!, _ idx: CFIndex) -> CFTree!
func CFTreeGetChildren(_ tree: CFTree!, _ children: UnsafeMutablePointer<Unmanaged<CFTree>?>!)
func CFTreeApplyFunctionToChildren(_ tree: CFTree!, _ applier: CFTreeApplierFunction!, _ context: UnsafeMutablePointer<Void>!)
@discardableResult
func CFTreeFindRoot(_ tree: CFTree!) -> CFTree!
func CFTreeSetContext(_ tree: CFTree!, _ context: UnsafePointer<CFTreeContext>!)
func CFTreePrependChild(_ tree: CFTree!, _ newChild: CFTree!)
func CFTreeAppendChild(_ tree: CFTree!, _ newChild: CFTree!)
func CFTreeInsertSibling(_ tree: CFTree!, _ newSibling: CFTree!)
func CFTreeRemove(_ tree: CFTree!)
func CFTreeRemoveAllChildren(_ tree: CFTree!)
func CFTreeSortChildren(_ tree: CFTree!, _ comparator: CFComparatorFunction!, _ context: UnsafeMutablePointer<Void>!)
