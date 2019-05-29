// Checks that we don't crash.
// RUN: %sourcekitd-test -req=cursor -pos=7:5 %s -- %s | %FileCheck %s
// CHECK: <empty cursor info; internal diagnostic: "Unable to resolve cursor info.">

class CameraViewController
{
  lazy var cameraController : CameraController	= CameraController(delegate: self)

  override func viewDidLoad()
  {
    cameraController.checkDeviceAuthorizationStatusAndConfigureDevice()
  }
}
