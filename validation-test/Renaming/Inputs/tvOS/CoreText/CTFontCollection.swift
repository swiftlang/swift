
class CTFontCollection {
}
class CTMutableFontCollection {
}
@available(tvOS 3.2, *)
@discardableResult
func CTFontCollectionGetTypeID() -> CFTypeID
typealias CTFontCollectionSortDescriptorsCallback = @convention(c) (CTFontDescriptor, CTFontDescriptor, UnsafeMutablePointer<Void>) -> CFComparisonResult
@available(tvOS 3.2, *)
let kCTFontCollectionRemoveDuplicatesOption: CFString
@available(tvOS 3.2, *)
@discardableResult
func CTFontCollectionCreateFromAvailableFonts(_ options: CFDictionary?) -> CTFontCollection
@available(tvOS 3.2, *)
@discardableResult
func CTFontCollectionCreateWithFontDescriptors(_ queryDescriptors: CFArray?, _ options: CFDictionary?) -> CTFontCollection
@available(tvOS 3.2, *)
@discardableResult
func CTFontCollectionCreateCopyWithFontDescriptors(_ original: CTFontCollection, _ queryDescriptors: CFArray?, _ options: CFDictionary?) -> CTFontCollection
@available(tvOS 3.2, *)
@discardableResult
func CTFontCollectionCreateMatchingFontDescriptors(_ collection: CTFontCollection) -> CFArray?
@available(tvOS 3.2, *)
@discardableResult
func CTFontCollectionCreateMatchingFontDescriptorsSortedWithCallback(_ collection: CTFontCollection, _ sortCallback: CTFontCollectionSortDescriptorsCallback?, _ refCon: UnsafeMutablePointer<Void>?) -> CFArray?
