public struct Library {
  var catalog: [Book]
}

public struct Book {
  var title: String
  var checkOutCount: Int
}
