import machine as m

class __extend__(m.cl_clo):
  def pprint(self, indent):
    return ' '* indent + '(clo %s env-hidden)' % self.l0.pprint(0)

class __extend__(m.cl_appinfo):
  def pprint(self, indent):
    return ' '* indent + '(appinfo %s %s)' % (self.e0.pprint(0), self.es1.pprint(0))
