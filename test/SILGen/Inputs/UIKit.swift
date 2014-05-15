import Foundation
@exported import UIKit

@asmname("UIApplicationMain")
func UIApplicationMain(argc: CInt,
                       argv: UnsafePointer<CString>,
                       principalClassName: NSString?,
                       delegateClassName: NSString?) -> CInt
