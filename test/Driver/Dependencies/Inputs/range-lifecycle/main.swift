private var privateInMain = 33
// watchMe will get more and more specific
// By running the program at various stages, one can see if the incrementality
// is correct.
func watchMe(_: Any) -> String {"Any"}
print ("watchMe is", watchMe(17))
