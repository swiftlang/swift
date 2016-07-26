
@available(iOS 3.0, *)
let kUTExportedTypeDeclarationsKey: CFString
@available(iOS 3.0, *)
let kUTImportedTypeDeclarationsKey: CFString
@available(iOS 3.0, *)
let kUTTypeIdentifierKey: CFString
@available(iOS 3.0, *)
let kUTTypeTagSpecificationKey: CFString
@available(iOS 3.0, *)
let kUTTypeConformsToKey: CFString
@available(iOS 3.0, *)
let kUTTypeDescriptionKey: CFString
@available(iOS 3.0, *)
let kUTTypeIconFileKey: CFString
@available(iOS 3.0, *)
let kUTTypeReferenceURLKey: CFString
@available(iOS 3.0, *)
let kUTTypeVersionKey: CFString
@available(iOS 3.0, *)
let kUTTagClassFilenameExtension: CFString
@available(iOS 3.0, *)
let kUTTagClassMIMEType: CFString
@available(iOS 3.0, *)
@discardableResult
func UTTypeCreatePreferredIdentifierForTag(_ inTagClass: CFString, _ inTag: CFString, _ inConformingToUTI: CFString?) -> Unmanaged<CFString>?
@available(iOS 3.0, *)
@discardableResult
func UTTypeCreateAllIdentifiersForTag(_ inTagClass: CFString, _ inTag: CFString, _ inConformingToUTI: CFString?) -> Unmanaged<CFArray>?
@available(iOS 3.0, *)
@discardableResult
func UTTypeCopyPreferredTagWithClass(_ inUTI: CFString, _ inTagClass: CFString) -> Unmanaged<CFString>?
@available(iOS 8.0, *)
@discardableResult
func UTTypeCopyAllTagsWithClass(_ inUTI: CFString, _ inTagClass: CFString) -> Unmanaged<CFArray>?
@available(iOS 3.0, *)
@discardableResult
func UTTypeEqual(_ inUTI1: CFString, _ inUTI2: CFString) -> Bool
@available(iOS 3.0, *)
@discardableResult
func UTTypeConformsTo(_ inUTI: CFString, _ inConformsToUTI: CFString) -> Bool
@available(iOS 3.0, *)
@discardableResult
func UTTypeCopyDescription(_ inUTI: CFString) -> Unmanaged<CFString>?
@available(iOS 8.0, *)
@discardableResult
func UTTypeIsDeclared(_ inUTI: CFString) -> Bool
@available(iOS 8.0, *)
@discardableResult
func UTTypeIsDynamic(_ inUTI: CFString) -> Bool
@available(iOS 3.0, *)
@discardableResult
func UTTypeCopyDeclaration(_ inUTI: CFString) -> Unmanaged<CFDictionary>?
@available(iOS 3.0, *)
@discardableResult
func UTTypeCopyDeclaringBundleURL(_ inUTI: CFString) -> Unmanaged<CFURL>?
