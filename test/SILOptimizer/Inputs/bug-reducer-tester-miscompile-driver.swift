
// We use this driver file to ensure that we are not dependent on how main/start
// are setup on our platform. The c function interface that we use is very
// standard, so we should be ok to rely on it.

@_silgen_name("function_2")
func function_2() -> ()

function_2()
