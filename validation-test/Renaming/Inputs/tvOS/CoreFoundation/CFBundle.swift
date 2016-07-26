
class CFBundle {
}
class CFPlugIn {
}
let kCFBundleInfoDictionaryVersionKey: CFString!
let kCFBundleExecutableKey: CFString!
let kCFBundleIdentifierKey: CFString!
let kCFBundleVersionKey: CFString!
let kCFBundleDevelopmentRegionKey: CFString!
let kCFBundleNameKey: CFString!
let kCFBundleLocalizationsKey: CFString!
@discardableResult
func CFBundleGetMainBundle() -> CFBundle!
@discardableResult
func CFBundleGetBundleWithIdentifier(_ bundleID: CFString!) -> CFBundle!
@discardableResult
func CFBundleGetAllBundles() -> CFArray!
@discardableResult
func CFBundleGetTypeID() -> CFTypeID
@discardableResult
func CFBundleCreate(_ allocator: CFAllocator!, _ bundleURL: CFURL!) -> CFBundle!
@discardableResult
func CFBundleCreateBundlesFromDirectory(_ allocator: CFAllocator!, _ directoryURL: CFURL!, _ bundleType: CFString!) -> CFArray!
@discardableResult
func CFBundleCopyBundleURL(_ bundle: CFBundle!) -> CFURL!
@discardableResult
func CFBundleGetValueForInfoDictionaryKey(_ bundle: CFBundle!, _ key: CFString!) -> CFTypeRef!
@discardableResult
func CFBundleGetInfoDictionary(_ bundle: CFBundle!) -> CFDictionary!
@discardableResult
func CFBundleGetLocalInfoDictionary(_ bundle: CFBundle!) -> CFDictionary!
func CFBundleGetPackageInfo(_ bundle: CFBundle!, _ packageType: UnsafeMutablePointer<UInt32>!, _ packageCreator: UnsafeMutablePointer<UInt32>!)
@discardableResult
func CFBundleGetIdentifier(_ bundle: CFBundle!) -> CFString!
@discardableResult
func CFBundleGetVersionNumber(_ bundle: CFBundle!) -> UInt32
@discardableResult
func CFBundleGetDevelopmentRegion(_ bundle: CFBundle!) -> CFString!
@discardableResult
func CFBundleCopySupportFilesDirectoryURL(_ bundle: CFBundle!) -> CFURL!
@discardableResult
func CFBundleCopyResourcesDirectoryURL(_ bundle: CFBundle!) -> CFURL!
@discardableResult
func CFBundleCopyPrivateFrameworksURL(_ bundle: CFBundle!) -> CFURL!
@discardableResult
func CFBundleCopySharedFrameworksURL(_ bundle: CFBundle!) -> CFURL!
@discardableResult
func CFBundleCopySharedSupportURL(_ bundle: CFBundle!) -> CFURL!
@discardableResult
func CFBundleCopyBuiltInPlugInsURL(_ bundle: CFBundle!) -> CFURL!
@discardableResult
func CFBundleCopyInfoDictionaryInDirectory(_ bundleURL: CFURL!) -> CFDictionary!
@discardableResult
func CFBundleGetPackageInfoInDirectory(_ url: CFURL!, _ packageType: UnsafeMutablePointer<UInt32>!, _ packageCreator: UnsafeMutablePointer<UInt32>!) -> Bool
@discardableResult
func CFBundleCopyResourceURL(_ bundle: CFBundle!, _ resourceName: CFString!, _ resourceType: CFString!, _ subDirName: CFString!) -> CFURL!
@discardableResult
func CFBundleCopyResourceURLsOfType(_ bundle: CFBundle!, _ resourceType: CFString!, _ subDirName: CFString!) -> CFArray!
@discardableResult
func CFBundleCopyLocalizedString(_ bundle: CFBundle!, _ key: CFString!, _ value: CFString!, _ tableName: CFString!) -> CFString!
@discardableResult
func CFBundleCopyResourceURLInDirectory(_ bundleURL: CFURL!, _ resourceName: CFString!, _ resourceType: CFString!, _ subDirName: CFString!) -> CFURL!
@discardableResult
func CFBundleCopyResourceURLsOfTypeInDirectory(_ bundleURL: CFURL!, _ resourceType: CFString!, _ subDirName: CFString!) -> CFArray!
@discardableResult
func CFBundleCopyBundleLocalizations(_ bundle: CFBundle!) -> CFArray!
@discardableResult
func CFBundleCopyPreferredLocalizationsFromArray(_ locArray: CFArray!) -> CFArray!
@discardableResult
func CFBundleCopyLocalizationsForPreferences(_ locArray: CFArray!, _ prefArray: CFArray!) -> CFArray!
@discardableResult
func CFBundleCopyResourceURLForLocalization(_ bundle: CFBundle!, _ resourceName: CFString!, _ resourceType: CFString!, _ subDirName: CFString!, _ localizationName: CFString!) -> CFURL!
@discardableResult
func CFBundleCopyResourceURLsOfTypeForLocalization(_ bundle: CFBundle!, _ resourceType: CFString!, _ subDirName: CFString!, _ localizationName: CFString!) -> CFArray!
@discardableResult
func CFBundleCopyInfoDictionaryForURL(_ url: CFURL!) -> CFDictionary!
@discardableResult
func CFBundleCopyLocalizationsForURL(_ url: CFURL!) -> CFArray!
@available(tvOS 2.0, *)
@discardableResult
func CFBundleCopyExecutableArchitecturesForURL(_ url: CFURL!) -> CFArray!
@discardableResult
func CFBundleCopyExecutableURL(_ bundle: CFBundle!) -> CFURL!
var kCFBundleExecutableArchitectureI386: Int { get }
var kCFBundleExecutableArchitecturePPC: Int { get }
var kCFBundleExecutableArchitectureX86_64: Int { get }
var kCFBundleExecutableArchitecturePPC64: Int { get }
@available(tvOS 2.0, *)
@discardableResult
func CFBundleCopyExecutableArchitectures(_ bundle: CFBundle!) -> CFArray!
@available(tvOS 2.0, *)
@discardableResult
func CFBundlePreflightExecutable(_ bundle: CFBundle!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CFBundleLoadExecutableAndReturnError(_ bundle: CFBundle!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@discardableResult
func CFBundleLoadExecutable(_ bundle: CFBundle!) -> Bool
@discardableResult
func CFBundleIsExecutableLoaded(_ bundle: CFBundle!) -> Bool
func CFBundleUnloadExecutable(_ bundle: CFBundle!)
@discardableResult
func CFBundleGetFunctionPointerForName(_ bundle: CFBundle!, _ functionName: CFString!) -> UnsafeMutablePointer<Void>!
func CFBundleGetFunctionPointersForNames(_ bundle: CFBundle!, _ functionNames: CFArray!, _ ftbl: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!)
@discardableResult
func CFBundleGetDataPointerForName(_ bundle: CFBundle!, _ symbolName: CFString!) -> UnsafeMutablePointer<Void>!
func CFBundleGetDataPointersForNames(_ bundle: CFBundle!, _ symbolNames: CFArray!, _ stbl: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!)
@discardableResult
func CFBundleCopyAuxiliaryExecutableURL(_ bundle: CFBundle!, _ executableName: CFString!) -> CFURL!
@discardableResult
func CFBundleGetPlugIn(_ bundle: CFBundle!) -> CFPlugIn!
typealias CFBundleRefNum = Int32
@discardableResult
func CFBundleOpenBundleResourceMap(_ bundle: CFBundle!) -> CFBundleRefNum
@discardableResult
func CFBundleOpenBundleResourceFiles(_ bundle: CFBundle!, _ refNum: UnsafeMutablePointer<CFBundleRefNum>!, _ localizedRefNum: UnsafeMutablePointer<CFBundleRefNum>!) -> Int32
func CFBundleCloseBundleResourceMap(_ bundle: CFBundle!, _ refNum: CFBundleRefNum)
