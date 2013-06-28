import Foundation
import AppKit

class REPLApplication : NSApplication {
}

/// Initializes and runs a REPLApplication on the main thread asynchronously.
func replApplicationMain() {
  assert(NSApp === nil)
  // Create a REPLApplication as the NSApp.
  var app = REPLApplication.sharedApplication() as! REPLApplication

  // Set the activation policy so we get a dock icon and can go foreground.
  // FIXME: enum type
  app.setActivationPolicy(
    NSApplicationActivationPolicy(NSApplicationActivationPolicyRegular))

  // Run asynchronously.
  app.performSelector("run", withObject:nil, afterDelay:0.0)

  // Quit the NSApplication when the REPL quits.
  atREPLExit({ app.terminate(nil) })
}
