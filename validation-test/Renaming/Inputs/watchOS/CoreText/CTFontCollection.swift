
class CTFontCollection {
}
class CTMutableFontCollection {
}
@available(watchOS 2.0, *)
@discardableResult
func CTFontCollectionGetTypeID() -> CFTypeID
typealias CTFontCollectionSortDescriptorsCallback = @convention(c) (CTFontDescriptor, CTFontDescriptor, UnsafeMutablePointer<Void>) -> CFComparisonResult
@available(watchOS 2.0, *)
let kCTFontCollectionRemoveDuplicatesOption: CFString
@available(watchOS 2.0, *)
@discardableResult
func CTFontCollectionCreateFromAvailableFonts(_ options: CFDictionary?) -> CTFontCollection
@available(watchOS 2.0, *)
@discardableResult
func CTFontCollectionCreateWithFontDescriptors(_ queryDescriptors: CFArray?, _ options: CFDictionary?) -> CTFontCollection
@available(watchOS 2.0, *)
@discardableResult
func CTFontCollectionCreateCopyWithFontDescriptors(_ original: CTFontCollection, _ queryDescriptors: CFArray?, _ options: CFDictionary?) -> CTFontCollection
@available(watchOS 2.0, *)
@discardableResult
func CTFontCollectionCreateMatchingFontDescriptors(_ collection: CTFontCollection) -> CFArray?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCollectionCreateMatchingFontDescriptorsSortedWithCallback(_ collection: CTFontCollection, _ sortCallback: CTFontCollectionSortDescriptorsCallback?, _ refCon: UnsafeMutablePointer<Void>?) -> CFArray?
