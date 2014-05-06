
words=[
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

# This is a phone book record.
class Record
  def initialize(firstname, lastname)
    @first = firstname
    @last = lastname
  end
  def first
    return @first
  end
  def last
    return @last
  end

  def <=>(other)
    comp = self.last <=> other.last
    if comp != 0
      return comp
    end
    return self.first <=> other.first
  end
end

Names = []

words.each do |first|
  words.each do |last|
    Names.push(Record.new(first, last))
  end
end

(0..100).each do |i|
  y = Array.new(Names)
  y.sort! {|a,b| a <=> b}
  #y.each do |r|
  #  puts r.last + " " + r.first
  #end
end

