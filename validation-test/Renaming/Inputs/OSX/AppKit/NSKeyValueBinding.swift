
var NSMultipleValuesMarker: AnyObject
var NSNoSelectionMarker: AnyObject
var NSNotApplicableMarker: AnyObject
@discardableResult
func NSIsControllerMarker(_ object: AnyObject?) -> Bool
let NSObservedObjectKey: String
let NSObservedKeyPathKey: String
let NSOptionsKey: String
extension NSObject {
  class func exposeBinding(_ binding: String)
  var exposedBindings: [String] { get }
  @discardableResult
  class func valueClass(forBinding binding: String) -> AnyClass?
  @discardableResult
  func valueClass(forBinding binding: String) -> AnyClass?
  class func bind(_ binding: String, to observable: AnyObject, withKeyPath keyPath: String, options options: [String : AnyObject]? = [:])
  func bind(_ binding: String, to observable: AnyObject, withKeyPath keyPath: String, options options: [String : AnyObject]? = [:])
  class func unbind(_ binding: String)
  func unbind(_ binding: String)
  @discardableResult
  class func info(forBinding binding: String) -> [String : AnyObject]?
  @discardableResult
  func info(forBinding binding: String) -> [String : AnyObject]?
  @available(OSX 10.5, *)
  @discardableResult
  class func optionDescriptions(forBinding aBinding: String) -> [NSAttributeDescription]
  @available(OSX 10.5, *)
  @discardableResult
  func optionDescriptions(forBinding aBinding: String) -> [NSAttributeDescription]
  class func exposedBindings() -> [String]
}
extension NSObject {
  class func setDefaultPlaceholder(_ placeholder: AnyObject?, forMarker marker: AnyObject?, withBinding binding: String)
  @discardableResult
  class func defaultPlaceholder(forMarker marker: AnyObject?, withBinding binding: String) -> AnyObject?
}
extension NSObject {
  class func objectDidBeginEditing(_ editor: AnyObject)
  func objectDidBeginEditing(_ editor: AnyObject)
  class func objectDidEndEditing(_ editor: AnyObject)
  func objectDidEndEditing(_ editor: AnyObject)
}
extension NSObject {
  class func discardEditing()
  func discardEditing()
  @discardableResult
  class func commitEditing() -> Bool
  @discardableResult
  func commitEditing() -> Bool
  class func commitEditing(withDelegate delegate: AnyObject?, didCommit didCommitSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  func commitEditing(withDelegate delegate: AnyObject?, didCommit didCommitSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @available(OSX 10.7, *)
  class func commitEditingAndReturnError() throws
  @available(OSX 10.7, *)
  func commitEditingAndReturnError() throws
}
let NSAlignmentBinding: String
let NSAlternateImageBinding: String
let NSAlternateTitleBinding: String
let NSAnimateBinding: String
let NSAnimationDelayBinding: String
let NSArgumentBinding: String
let NSAttributedStringBinding: String
let NSContentArrayBinding: String
let NSContentArrayForMultipleSelectionBinding: String
let NSContentBinding: String
@available(OSX 10.5, *)
let NSContentDictionaryBinding: String
let NSContentHeightBinding: String
let NSContentObjectBinding: String
let NSContentObjectsBinding: String
let NSContentSetBinding: String
let NSContentValuesBinding: String
let NSContentWidthBinding: String
let NSCriticalValueBinding: String
let NSDataBinding: String
let NSDisplayPatternTitleBinding: String
let NSDisplayPatternValueBinding: String
let NSDocumentEditedBinding: String
let NSDoubleClickArgumentBinding: String
let NSDoubleClickTargetBinding: String
let NSEditableBinding: String
let NSEnabledBinding: String
@available(OSX 10.5, *)
let NSExcludedKeysBinding: String
let NSFilterPredicateBinding: String
let NSFontBinding: String
let NSFontBoldBinding: String
let NSFontFamilyNameBinding: String
let NSFontItalicBinding: String
let NSFontNameBinding: String
let NSFontSizeBinding: String
let NSHeaderTitleBinding: String
let NSHiddenBinding: String
let NSImageBinding: String
@available(OSX 10.5, *)
let NSIncludedKeysBinding: String
@available(OSX 10.5, *)
let NSInitialKeyBinding: String
@available(OSX 10.5, *)
let NSInitialValueBinding: String
let NSIsIndeterminateBinding: String
let NSLabelBinding: String
@available(OSX 10.5, *)
let NSLocalizedKeyDictionaryBinding: String
let NSManagedObjectContextBinding: String
let NSMaximumRecentsBinding: String
let NSMaxValueBinding: String
let NSMaxWidthBinding: String
let NSMinValueBinding: String
let NSMinWidthBinding: String
let NSMixedStateImageBinding: String
let NSOffStateImageBinding: String
let NSOnStateImageBinding: String
@available(OSX 10.7, *)
let NSPositioningRectBinding: String
let NSPredicateBinding: String
let NSRecentSearchesBinding: String
let NSRepresentedFilenameBinding: String
let NSRowHeightBinding: String
let NSSelectedIdentifierBinding: String
let NSSelectedIndexBinding: String
let NSSelectedLabelBinding: String
let NSSelectedObjectBinding: String
let NSSelectedObjectsBinding: String
let NSSelectedTagBinding: String
let NSSelectedValueBinding: String
let NSSelectedValuesBinding: String
let NSSelectionIndexesBinding: String
let NSSelectionIndexPathsBinding: String
let NSSortDescriptorsBinding: String
let NSTargetBinding: String
let NSTextColorBinding: String
let NSTitleBinding: String
let NSToolTipBinding: String
@available(OSX 10.5, *)
let NSTransparentBinding: String
let NSValueBinding: String
let NSValuePathBinding: String
let NSValueURLBinding: String
let NSVisibleBinding: String
let NSWarningValueBinding: String
let NSWidthBinding: String
let NSAllowsEditingMultipleValuesSelectionBindingOption: String
let NSAllowsNullArgumentBindingOption: String
let NSAlwaysPresentsApplicationModalAlertsBindingOption: String
let NSConditionallySetsEditableBindingOption: String
let NSConditionallySetsEnabledBindingOption: String
let NSConditionallySetsHiddenBindingOption: String
let NSContinuouslyUpdatesValueBindingOption: String
let NSCreatesSortDescriptorBindingOption: String
let NSDeletesObjectsOnRemoveBindingsOption: String
let NSDisplayNameBindingOption: String
let NSDisplayPatternBindingOption: String
@available(OSX 10.5, *)
let NSContentPlacementTagBindingOption: String
let NSHandlesContentAsCompoundValueBindingOption: String
let NSInsertsNullPlaceholderBindingOption: String
let NSInvokesSeparatelyWithArrayObjectsBindingOption: String
let NSMultipleValuesPlaceholderBindingOption: String
let NSNoSelectionPlaceholderBindingOption: String
let NSNotApplicablePlaceholderBindingOption: String
let NSNullPlaceholderBindingOption: String
let NSRaisesForNotApplicableKeysBindingOption: String
let NSPredicateFormatBindingOption: String
let NSSelectorNameBindingOption: String
let NSSelectsAllWhenSettingContentBindingOption: String
let NSValidatesImmediatelyBindingOption: String
let NSValueTransformerNameBindingOption: String
let NSValueTransformerBindingOption: String
