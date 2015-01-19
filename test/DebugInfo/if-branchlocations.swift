// RUN: %target-swift-frontend %s -emit-sil -emit-verbose-sil -g -o - | FileCheck %s

class NSURL {}

class NSPathControlItem {
  var URL: NSURL?
}

class NSPathControl {
  var clickedPathItem: NSPathControlItem?;
}

class AppDelegate {
    
  func LogStr(message: String) {
  }

  func componentClicked(sender: AnyObject)
  {
    if let control = sender as? NSPathControl
    {
      LogStr( "Got a path control" )
      if let item = control.clickedPathItem
      {
        LogStr( "Got an NSPathControlItem" )
        // Verify that the branch's location is >= the cleanup's location.
        // ( The implicit false block of the conditional
        //   below inherits the location from the condition. )
        // CHECK: destroy_addr{{.*}}Optional<NSURL>{{.*}}line:[[@LINE+2]]
        // CHECK-NEXT: br{{.*}}line:[[@LINE+1]]
        if let url = item.URL
        {
          LogStr( "There is a url" )
        }
        // Verify that the branch's location is >= the cleanup's location.
        // CHECK: strong_release{{.*}}$NSPathControlItem{{.*}}line:[[@LINE+2]]
        // CHECK-NEXT: br{{.*}}line:[[@LINE+1]]
      }
    }
  }
}
