import Foundation
@exported import UIKit

@asmname("UIApplicationMain") @public
func UIApplicationMain(argc: CInt,
                       argv: UnsafePointer<UnsafePointer<CChar>>,
                       principalClassName: NSString?,
                       delegateClassName: NSString?) -> CInt
