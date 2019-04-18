// Checks that we don't crash.
// RUN: not %sourcekitd-test -req=cursor -pos=7:5 %s -- %s 2>&1 | %FileCheck %s
// CHECK: (Request Failed): Unable to resolve cursor info

class CameraViewController
{
  lazy var cameraController : CameraController	= CameraController(delegate: self)

  override func viewDidLoad()
  {
    cameraController.checkDeviceAuthorizationStatusAndConfigureDevice()
  }
}
