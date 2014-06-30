import Foundation
@exported import UIKit

@asmname("UIApplicationMain") @public
func UIApplicationMain(argc: CInt,
                       argv: UnsafePointer<CString>,
                       principalClassName: NSString?,
                       delegateClassName: NSString?) -> CInt
