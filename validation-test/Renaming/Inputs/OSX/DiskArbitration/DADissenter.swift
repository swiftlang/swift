
var kDAReturnSuccess: Int { get }
var kDAReturnError: Int { get }
var kDAReturnBusy: Int { get }
var kDAReturnBadArgument: Int { get }
var kDAReturnExclusiveAccess: Int { get }
var kDAReturnNoResources: Int { get }
var kDAReturnNotFound: Int { get }
var kDAReturnNotMounted: Int { get }
var kDAReturnNotPermitted: Int { get }
var kDAReturnNotPrivileged: Int { get }
var kDAReturnNotReady: Int { get }
var kDAReturnNotWritable: Int { get }
var kDAReturnUnsupported: Int { get }
typealias DAReturn = mach_error_t
class DADissenter {
}
@discardableResult
func DADissenterCreate(_ allocator: CFAllocator?, _ status: DAReturn, _ string: CFString?) -> DADissenter
@discardableResult
func DADissenterGetStatus(_ dissenter: DADissenter) -> DAReturn
@discardableResult
func DADissenterGetStatusString(_ dissenter: DADissenter) -> CFString?
