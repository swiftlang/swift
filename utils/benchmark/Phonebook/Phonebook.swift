@asmname("mach_absolute_time") func __mach_absolute_time__() -> UInt64

var words=[
"James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph",
"Charles", "Thomas", "Christopher", "Daniel", "Matthew", "Donald", "Anthony",
"Paul", "Mark", "George", "Steven", "Kenneth", "Andrew", "Edward", "Brian",
"Joshua", "Kevin", "Ronald", "Timothy", "Jason", "Jeffrey", "Gary", "Ryan",
"Nicholas", "Eric", "Stephen", "Jacob", "Larry", "Frank", "Jonathan", "Scott",
"Justin", "Raymond", "Brandon", "Gregory", "Samuel", "Patrick", "Benjamin",
"Jack", "Dennis", "Jerry", "Alexander", "Tyler", "Douglas", "Henry", "Peter",
"Walter", "Aaron", "Jose", "Adam", "Harold", "Zachary", "Nathan", "Carl",
"Kyle", "Arthur", "Gerald", "Lawrence", "Roger", "Albert", "Keith", "Jeremy",
"Terry", "Joe", "Sean", "Willie", "Jesse", "Ralph", "Billy", "Austin", "Bruce",
"Christian", "Roy", "Bryan", "Eugene", "Louis", "Harry", "Wayne", "Ethan",
"Jordan", "Russell", "Alan", "Philip", "Randy", "Juan", "Howard", "Vincent",
"Bobby", "Dylan", "Johnny", "Phillip", "Craig"]

// This is a phone book record.
struct Record : Comparable {
  var first : String
  var last : String

  init(_ first_ : String,_ last_ : String) {
    first = first_
    last = last_
  }
}
func ==(lhs: Record, rhs: Record) -> Bool {
  return lhs.last.compare(rhs.last) == 0 &&
         lhs.first.compare(rhs.first) == 0
}
func <(lhs: Record, rhs: Record) -> Bool {
  let lastComp = lhs.last.compare(rhs.last)
  if lastComp == -1 {
    return true
  }
  if lastComp == 1 {
    return false
  }

  let firstComp = lhs.first.compare(rhs.first)
  if firstComp == -1 {
    return true
  }

  return false
}

// The list of names in the phonebook.
var Names : Record[] = []
for first in words {
  for last in words {
    Names.append(Record(first, last))
  }
}

func benchStringSort() {
  let start = __mach_absolute_time__()
  for (var i = 0; i < 100; i++) {
    var t = Names;
    sort(&t)
  }
  let delta = __mach_absolute_time__() - start

  print("\(delta) nanoseconds.")
}

benchStringSort()
