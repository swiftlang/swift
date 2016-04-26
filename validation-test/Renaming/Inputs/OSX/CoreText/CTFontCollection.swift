
class CTFontCollection {
}
class CTMutableFontCollection {
}
@available(OSX 10.5, *)
@discardableResult
func CTFontCollectionGetTypeID() -> CFTypeID
typealias CTFontCollectionSortDescriptorsCallback = @convention(c) (CTFontDescriptor, CTFontDescriptor, UnsafeMutablePointer<Void>) -> CFComparisonResult
@available(OSX 10.5, *)
let kCTFontCollectionRemoveDuplicatesOption: CFString
@available(OSX 10.7, *)
let kCTFontCollectionIncludeDisabledFontsOption: CFString
@available(OSX 10.7, *)
let kCTFontCollectionDisallowAutoActivationOption: CFString
@available(OSX 10.5, *)
@discardableResult
func CTFontCollectionCreateFromAvailableFonts(_ options: CFDictionary?) -> CTFontCollection
@available(OSX 10.5, *)
@discardableResult
func CTFontCollectionCreateWithFontDescriptors(_ queryDescriptors: CFArray?, _ options: CFDictionary?) -> CTFontCollection
@available(OSX 10.5, *)
@discardableResult
func CTFontCollectionCreateCopyWithFontDescriptors(_ original: CTFontCollection, _ queryDescriptors: CFArray?, _ options: CFDictionary?) -> CTFontCollection
@available(OSX 10.7, *)
@discardableResult
func CTFontCollectionCreateMutableCopy(_ original: CTFontCollection) -> CTMutableFontCollection
@available(OSX 10.7, *)
@discardableResult
func CTFontCollectionCopyQueryDescriptors(_ collection: CTFontCollection) -> CFArray?
@available(OSX 10.7, *)
func CTFontCollectionSetQueryDescriptors(_ collection: CTMutableFontCollection, _ descriptors: CFArray?)
@available(OSX 10.7, *)
@discardableResult
func CTFontCollectionCopyExclusionDescriptors(_ collection: CTFontCollection) -> CFArray?
@available(OSX 10.7, *)
func CTFontCollectionSetExclusionDescriptors(_ collection: CTMutableFontCollection, _ descriptors: CFArray?)
@available(OSX 10.5, *)
@discardableResult
func CTFontCollectionCreateMatchingFontDescriptors(_ collection: CTFontCollection) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func CTFontCollectionCreateMatchingFontDescriptorsSortedWithCallback(_ collection: CTFontCollection, _ sortCallback: CTFontCollectionSortDescriptorsCallback?, _ refCon: UnsafeMutablePointer<Void>?) -> CFArray?
@available(OSX 10.7, *)
@discardableResult
func CTFontCollectionCreateMatchingFontDescriptorsWithOptions(_ collection: CTFontCollection, _ options: CFDictionary?) -> CFArray?
@available(OSX 10.7, *)
@discardableResult
func CTFontCollectionCreateMatchingFontDescriptorsForFamily(_ collection: CTFontCollection, _ familyName: CFString, _ options: CFDictionary?) -> CFArray?
@available(OSX 10.7, *)
struct CTFontCollectionCopyOptions : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var unique: CTFontCollectionCopyOptions { get }
  static var standardSort: CTFontCollectionCopyOptions { get }
}
@available(OSX 10.7, *)
@discardableResult
func CTFontCollectionCopyFontAttribute(_ collection: CTFontCollection, _ attributeName: CFString, _ options: CTFontCollectionCopyOptions) -> CFArray
@available(OSX 10.7, *)
@discardableResult
func CTFontCollectionCopyFontAttributes(_ collection: CTFontCollection, _ attributeNames: CFSet, _ options: CTFontCollectionCopyOptions) -> CFArray
