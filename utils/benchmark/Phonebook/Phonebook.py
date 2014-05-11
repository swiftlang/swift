
words=[
 u"James", u"John", u"Robert", u"Michael", u"William", u"David", u"Richard", u"Joseph",
 u"Charles", u"Thomas", u"Christopher", u"Daniel", u"Matthew", u"Donald", u"Anthony",
 u"Paul", u"Mark", u"George", u"Steven", u"Kenneth", u"Andrew", u"Edward", u"Brian",
 u"Joshua", u"Kevin", u"Ronald", u"Timothy", u"Jason", u"Jeffrey", u"Gary", u"Ryan",
 u"Nicholas", u"Eric", u"Stephen", u"Jacob", u"Larry", u"Frank", u"Jonathan", u"Scott",
 u"Justin", u"Raymond", u"Brandon", u"Gregory", u"Samuel", u"Patrick", u"Benjamin",
 u"Jack", u"Dennis", u"Jerry", u"Alexander", u"Tyler", u"Douglas", u"Henry", u"Peter",
 u"Walter", u"Aaron", u"Jose", u"Adam", u"Harold", u"Zachary", u"Nathan", u"Carl",
 u"Kyle", u"Arthur", u"Gerald", u"Lawrence", u"Roger", u"Albert", u"Keith", u"Jeremy",
 u"Terry", u"Joe", u"Sean", u"Willie", u"Jesse", u"Ralph", u"Billy", u"Austin", u"Bruce",
 u"Christian", u"Roy", u"Bryan", u"Eugene", u"Louis", u"Harry", u"Wayne", u"Ethan",
 u"Jordan", u"Russell", u"Alan", u"Philip", u"Randy", u"Juan", u"Howard", u"Vincent",
 u"Bobby", u"Dylan", u"Johnny", u"Phillip", u"Craig"]

# This is a phone book record.
class Record:
  def __init__(self, firstname, lastname):
    self.first = firstname
    self.last = lastname

  def __lt__(self, other):
    if self.last < other.last:
      return True
    if self.last > other.last:
      return False
    return self.first < other.first

Records = []

for first in words:
  for last in words:
    Records.append(Record(first, last))

for i in xrange(100):
  y = Records[:]
  y = sorted(y)
  #for w in y:
  #  print w.first, w.last

