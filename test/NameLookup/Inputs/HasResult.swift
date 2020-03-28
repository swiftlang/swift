public enum Result<Value, Error> {
case success(Value)
case failure(Error)
}
