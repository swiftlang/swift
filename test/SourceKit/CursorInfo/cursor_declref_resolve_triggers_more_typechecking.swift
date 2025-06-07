struct GorEach<Data, Content> {
  public init(_ data: Data, content: (Data) -> Void) {}
}

struct MavigationLink<Label, Destination> {
  init(destination: PeopleDetail) {}
}

struct PeopleDetail {
  init(peopleId: Int) {}
}

func test(peoples: [Int]) {
  GorEach(peoples) { people in
    // Should not crash
    // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):56 %s -- %s
    MavigationLink(destination: PeopleDetail(peopleId: people))
  }
}
