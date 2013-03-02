import conf
from scan import *
from route import ERoutingResult

class StdinScanner(Scanner,conf.ScannerPlugin):
	elName = "stdinscanner"
	
	def run(self):
		for line in sys.stdin:
			self._notifyListeners(line[:-1])
	
	
	def _stopAction(self):
		sys.stdin.close()
	
	def create(el, name):
		handler = StdinScanner(name)
		
		return handler